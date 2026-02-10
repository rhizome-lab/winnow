//! Flash/AVM2-specific rewrites for SystemCall → JS construct lowering.
//!
//! This module converts AVM2 runtime calls (scope lookups, super dispatch,
//! object construction, etc.) into native JavaScript constructs during the
//! `lower` pass. All Flash-specific knowledge is confined here.

use std::collections::{HashMap, HashSet};

use reincarnate_core::ir::ast::Expr;
use reincarnate_core::ir::Constant;

use crate::js_ast::{JsExpr, JsStmt};
use crate::lower::{lower_expr, lower_exprs, LowerCtx};

// ---------------------------------------------------------------------------
// Flash-specific lowering context
// ---------------------------------------------------------------------------

/// Context needed for Flash/AVM2 scope resolution and rewrite decisions.
pub struct FlashLowerCtx {
    /// Qualified class name → sanitized short name.
    pub class_names: HashMap<String, String>,
    /// Short names of the current class and all its ancestors.
    pub ancestors: HashSet<String>,
    /// Method short names visible in the class hierarchy.
    pub method_names: HashSet<String>,
    /// Instance field short names visible in the class hierarchy.
    pub instance_fields: HashSet<String>,
    /// Whether we are inside a method (have a `this`).
    pub has_self: bool,
    /// Suppress `super()` calls (class has no real superclass).
    pub suppress_super: bool,
    /// Whether we are inside a cinit (class static initializer).
    pub is_cinit: bool,
    /// Static field short names declared on the current class.
    pub static_fields: HashSet<String>,
}

// ---------------------------------------------------------------------------
// Scope-lookup detection and resolution
// ---------------------------------------------------------------------------

/// Check whether an expression is a Flash scope-lookup SystemCall
/// (findPropStrict or findProperty).
pub fn is_scope_lookup(expr: &Expr) -> bool {
    scope_lookup_args(expr).is_some()
}

/// Extract the args from a scope-lookup SystemCall, or None.
fn scope_lookup_args(expr: &Expr) -> Option<&[Expr]> {
    match expr {
        Expr::SystemCall {
            system,
            method,
            args,
        } if system == "Flash.Scope"
            && (method == "findPropStrict" || method == "findProperty") =>
        {
            Some(args)
        }
        _ => None,
    }
}

/// Extract the class name from a scope-lookup arg string constant.
///
/// For arg like `"classes:SomeClass::someField"`, returns `"SomeClass"`.
fn class_from_scope_arg(args: &[Expr]) -> Option<String> {
    let arg = args.first()?;
    if let Expr::Literal(Constant::String(s)) = arg {
        let prefix = s.rsplit_once("::")?.0;
        let class_name = prefix.rsplit_once(':')?.1;
        Some(class_name.to_string())
    } else {
        None
    }
}

enum ScopeResolution {
    /// The lookup matched an ancestor class.
    Ancestor(String),
    /// Generic scope lookup (class ref, global, etc.).
    ScopeLookup,
}

fn resolve_scope_lookup(args: &[Expr], flash: &FlashLowerCtx) -> ScopeResolution {
    if let Some(class_name) = class_from_scope_arg(args) {
        if flash.ancestors.contains(&class_name) {
            return ScopeResolution::Ancestor(class_name);
        }
    }
    ScopeResolution::ScopeLookup
}

// ---------------------------------------------------------------------------
// Field resolution (scope-lookup in object position)
// ---------------------------------------------------------------------------

/// Try to resolve `Field { object: scope_lookup, field }` for Flash.
///
/// Returns `Some(JsExpr)` if the object was a scope lookup, `None` otherwise.
pub fn try_resolve_field(object: &Expr, field: &str, flash: &FlashLowerCtx) -> Option<JsExpr> {
    let args = scope_lookup_args(object)?;
    let effective = field.rsplit("::").next().unwrap_or(field);

    Some(match resolve_scope_lookup(args, flash) {
        ScopeResolution::Ancestor(ref class_name) => {
            if flash.is_cinit || flash.instance_fields.contains(effective) {
                JsExpr::Field {
                    object: Box::new(JsExpr::This),
                    field: effective.to_string(),
                }
            } else {
                JsExpr::Field {
                    object: Box::new(JsExpr::Var(class_name.clone())),
                    field: effective.to_string(),
                }
            }
        }
        ScopeResolution::ScopeLookup => {
            // Check if the full field is a known class name.
            if let Some(short) = flash.class_names.get(field) {
                return Some(JsExpr::Var(short.clone()));
            }
            // In cinit, static fields must be accessed as this.field.
            if flash.is_cinit && flash.static_fields.contains(effective) {
                JsExpr::Field {
                    object: Box::new(JsExpr::This),
                    field: effective.to_string(),
                }
            } else {
                JsExpr::Var(effective.to_string())
            }
        }
    })
}

// ---------------------------------------------------------------------------
// Call resolution (scope-lookup in receiver position)
// ---------------------------------------------------------------------------

/// Try to resolve a Call where the receiver is a Flash scope lookup.
///
/// Returns `Some(JsExpr)` if the receiver was a scope lookup, `None` otherwise.
pub fn try_resolve_scope_call(
    method: &str,
    receiver: &Expr,
    rest_args: &[Expr],
    flash: &FlashLowerCtx,
    ctx: &LowerCtx,
) -> Option<JsExpr> {
    if !is_scope_lookup(receiver) {
        return None;
    }
    let lowered_rest = lower_exprs(rest_args, ctx);
    let use_this = (flash.has_self && flash.method_names.contains(method)) || flash.is_cinit;
    let callee = if use_this {
        JsExpr::Field {
            object: Box::new(JsExpr::This),
            field: method.to_string(),
        }
    } else {
        JsExpr::Var(method.to_string())
    };
    Some(JsExpr::Call {
        callee: Box::new(callee),
        args: lowered_rest,
    })
}

// ---------------------------------------------------------------------------
// SystemCall expression rewrites
// ---------------------------------------------------------------------------

/// Try to rewrite a Flash SystemCall in expression context.
///
/// Returns `Some(JsExpr)` if the call was recognized, `None` for unmapped calls.
pub fn try_lower_system_call_expr(
    system: &str,
    method: &str,
    args: &[Expr],
    flash: &FlashLowerCtx,
    ctx: &LowerCtx,
) -> Option<JsExpr> {
    // constructSuper → super() or void 0
    if system == "Flash.Class" && method == "constructSuper" {
        if flash.suppress_super {
            return Some(JsExpr::Literal(Constant::Null));
        }
        return Some(JsExpr::SuperCall(lower_exprs(&args[1..], ctx)));
    }

    // newFunction → this.methodRef
    if system == "Flash.Object" && method == "newFunction" && args.len() == 1 {
        if let Expr::Literal(Constant::String(name)) = &args[0] {
            let short = name.rsplit("::").next().unwrap_or(name);
            return Some(JsExpr::Field {
                object: Box::new(JsExpr::This),
                field: short.to_string(),
            });
        }
    }

    // construct → new Ctor(args)
    if system == "Flash.Object" && method == "construct" {
        if let Some((ctor, rest)) = args.split_first() {
            return Some(JsExpr::New {
                callee: Box::new(lower_expr(ctor, ctx)),
                args: lower_exprs(rest, ctx),
            });
        }
    }

    // findPropStrict/findProperty → scope resolution
    if system == "Flash.Scope" && (method == "findPropStrict" || method == "findProperty") {
        return Some(match resolve_scope_lookup(args, flash) {
            ScopeResolution::Ancestor(ref class_name) => {
                if flash.is_cinit {
                    JsExpr::This
                } else {
                    JsExpr::Var(class_name.clone())
                }
            }
            ScopeResolution::ScopeLookup => {
                // Standalone scope lookup — consumed by Field/Call during lowering.
                // If it reaches here, emit empty (will be wrapped in Expr stmt and skipped).
                JsExpr::Var(String::new())
            }
        });
    }

    // newActivation → ({})
    if system == "Flash.Scope" && method == "newActivation" && args.is_empty() {
        return Some(JsExpr::Activation);
    }

    // typeOf → typeof expr
    if system == "Flash.Object" && method == "typeOf" && args.len() == 1 {
        return Some(JsExpr::TypeOf(Box::new(lower_expr(&args[0], ctx))));
    }

    // hasProperty(obj, k) → k in obj
    if system == "Flash.Object" && method == "hasProperty" && args.len() == 2 {
        return Some(JsExpr::In {
            key: Box::new(lower_expr(&args[1], ctx)),
            object: Box::new(lower_expr(&args[0], ctx)),
        });
    }

    // deleteProperty(obj, k) → delete obj[k]
    if system == "Flash.Object" && method == "deleteProperty" && args.len() == 2 {
        return Some(JsExpr::Delete {
            object: Box::new(lower_expr(&args[0], ctx)),
            key: Box::new(lower_expr(&args[1], ctx)),
        });
    }

    // newObject(k1, v1, k2, v2, ...) → { k1: v1, k2: v2, ... }
    if system == "Flash.Object" && method == "newObject" {
        if args.is_empty() {
            return Some(JsExpr::ObjectInit(Vec::new()));
        }
        if args.len().is_multiple_of(2) {
            // Deduplicate keys (last value wins, matching JS/Flash runtime semantics).
            let mut keys: Vec<String> = Vec::new();
            let mut values: HashMap<String, JsExpr> = HashMap::new();
            for pair in args.chunks_exact(2) {
                let key = extract_object_key(&pair[0], ctx);
                let val = lower_expr(&pair[1], ctx);
                if !values.contains_key(&key) {
                    keys.push(key.clone());
                }
                values.insert(key, val);
            }
            let pairs: Vec<_> = keys
                .into_iter()
                .map(|k| {
                    let v = values.remove(&k).unwrap();
                    (k, v)
                })
                .collect();
            return Some(JsExpr::ObjectInit(pairs));
        }
    }

    // callSuper(this, "method", ...args) → super.method(args)
    if system == "Flash.Class" && method == "callSuper" && args.len() >= 2 {
        if let Expr::Literal(Constant::String(name)) = &args[1] {
            return Some(JsExpr::SuperMethodCall {
                method: name.clone(),
                args: lower_exprs(&args[2..], ctx),
            });
        }
    }

    // getSuper(this, "prop") → super.prop
    if system == "Flash.Class" && method == "getSuper" && args.len() == 2 {
        if let Expr::Literal(Constant::String(name)) = &args[1] {
            return Some(JsExpr::SuperGet(name.clone()));
        }
    }

    // setSuper(this, "prop", value) → (super.prop = value) in expression context
    if system == "Flash.Class" && method == "setSuper" && args.len() == 3 {
        if let Expr::Literal(Constant::String(name)) = &args[1] {
            return Some(JsExpr::SuperSet {
                prop: name.clone(),
                value: Box::new(lower_expr(&args[2], ctx)),
            });
        }
    }

    None
}

/// Extract an object-literal key string from a key expression.
///
/// For newObject, keys are typically string literals. We extract the raw
/// string for use as the object property name. For non-string keys, we
/// fall back to a printed representation.
fn extract_object_key(expr: &Expr, ctx: &LowerCtx) -> String {
    match expr {
        Expr::Literal(Constant::String(s)) => s.clone(),
        _ => {
            // Fallback: lower to JsExpr and use debug format.
            // In practice, newObject keys are always string literals.
            format!("{:?}", lower_expr(expr, ctx))
        }
    }
}

// ---------------------------------------------------------------------------
// SystemCall statement rewrites
// ---------------------------------------------------------------------------

/// Result of attempting a statement-level SystemCall rewrite.
pub enum StmtRewrite {
    /// Rewrite not applicable — fall through to default lowering.
    Pass,
    /// Replace with this statement.
    Replace(JsStmt),
    /// Skip entirely (emit nothing).
    Skip,
}

/// Try to rewrite a Flash SystemCall in statement context.
///
/// Statement context allows producing `Throw`, skipping suppressed super calls,
/// and converting `setSuper` to assignments.
pub fn try_lower_system_call_stmt(
    system: &str,
    method: &str,
    args: &[Expr],
    flash: &FlashLowerCtx,
    ctx: &LowerCtx,
) -> StmtRewrite {
    // constructSuper → super() or skip
    if system == "Flash.Class" && method == "constructSuper" {
        if flash.suppress_super {
            return StmtRewrite::Skip;
        }
        return StmtRewrite::Replace(JsStmt::Expr(JsExpr::SuperCall(lower_exprs(
            &args[1..],
            ctx,
        ))));
    }

    // throw(x) → throw x;
    if system == "Flash.Exception" && method == "throw" && args.len() == 1 {
        return StmtRewrite::Replace(JsStmt::Throw(lower_expr(&args[0], ctx)));
    }

    // setSuper(this, "prop", value) → super.prop = value;
    if system == "Flash.Class" && method == "setSuper" && args.len() == 3 {
        if let Expr::Literal(Constant::String(name)) = &args[1] {
            return StmtRewrite::Replace(JsStmt::Assign {
                target: JsExpr::SuperGet(name.clone()),
                value: lower_expr(&args[2], ctx),
            });
        }
    }

    StmtRewrite::Pass
}
