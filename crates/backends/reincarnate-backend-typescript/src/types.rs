use reincarnate_core::ir::Type;

/// Map an IR [`Type`] to its TypeScript representation.
pub fn ts_type(ty: &Type) -> String {
    match ty {
        Type::Void => "void".into(),
        Type::Bool => "boolean".into(),
        Type::Int(_) | Type::UInt(_) | Type::Float(_) => "number".into(),
        Type::String => "string".into(),
        Type::Array(elem) => format!("{}[]", ts_type_paren(elem)),
        Type::Map(k, v) => format!("Map<{}, {}>", ts_type(k), ts_type(v)),
        Type::Option(inner) => format!("{} | null", ts_type_paren(inner)),
        Type::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().map(ts_type).collect();
            format!("[{}]", parts.join(", "))
        }
        Type::Struct(name) | Type::Enum(name) => name.clone(),
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
        Type::Var(_) | Type::Dynamic => "any".into(),
    }
}

/// Wrap compound types in parens when used in contexts like `T[]`.
fn ts_type_paren(ty: &Type) -> String {
    match ty {
        Type::Option(_) | Type::Function(_) => format!("({})", ts_type(ty)),
        _ => ts_type(ty),
    }
}
