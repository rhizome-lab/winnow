use reincarnate_core::ir::Type;

use crate::emit::sanitize_ident;

/// Map an IR [`Type`] to its TypeScript representation.
pub fn ts_type(ty: &Type) -> String {
    match ty {
        Type::Void => "void".into(),
        Type::Bool => "boolean".into(),
        Type::Int(_) | Type::UInt(_) | Type::Float(_) => "number".into(),
        Type::String => "string".into(),
        Type::Array(elem) => format!("{}[]", ts_type_paren(elem)),
        Type::Map(k, v) => {
            // Map keys should be `unknown` rather than `any` — `any` disables
            // type checking on lookups while `unknown` forces explicit narrowing.
            let key = if matches!(k.as_ref(), Type::Dynamic) {
                "unknown".to_string()
            } else {
                ts_type(k)
            };
            format!("Map<{}, {}>", key, ts_type(v))
        }
        Type::Option(inner) => format!("{} | null", ts_type_paren(inner)),
        Type::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().map(ts_type).collect();
            format!("[{}]", parts.join(", "))
        }
        Type::Struct(name) | Type::Enum(name) => {
            let short = name.rsplit("::").next().unwrap_or(name);
            // AS3/JS `Object` is a dynamic property bag, not TypeScript's `Object`
            // interface. TypeScript's `Object` has no index signature, so any dynamic
            // key access causes TS7053. Map it to `Record<string, any>` instead.
            if short == "Object" {
                return "Record<string, any>".into();
            }
            sanitize_ident(short)
        }
        Type::Function(sig) => {
            let params: Vec<_> = sig
                .params
                .iter()
                .enumerate()
                .map(|(i, t)| format!("p{}: {}", i, ts_type(t)))
                .collect();
            format!("({}) => {}", params.join(", "), ts_type(&sig.return_ty))
        }
        Type::Coroutine {
            yield_ty,
            return_ty,
        } => format!(
            "Generator<{}, {}, unknown>",
            ts_type(yield_ty),
            ts_type(return_ty)
        ),
        Type::Union(types) => {
            let mut parts = Vec::new();
            for t in types {
                let s = ts_type(t);
                if !parts.contains(&s) {
                    parts.push(s);
                }
            }
            parts.join(" | ")
        }
        Type::Var(_) | Type::Dynamic => "any".into(),
        Type::Unknown => "unknown".into(),
    }
}

/// Wrap compound types in parens when used in contexts like `T[]`.
fn ts_type_paren(ty: &Type) -> String {
    match ty {
        Type::Option(_) | Type::Function(_) | Type::Union(_) => format!("({})", ts_type(ty)),
        _ => ts_type(ty),
    }
}
