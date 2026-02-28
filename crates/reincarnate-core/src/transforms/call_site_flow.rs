use std::collections::HashMap;

use crate::error::CoreError;
use crate::ir::{Module, Op, Type};
use crate::pipeline::{Transform, TransformResult};

/// Interprocedural call-site type narrowing — collects argument types from all
/// call sites and narrows callee parameter types that are still `Dynamic`.
///
/// This pass bridges the gap between intra-function `TypeInference` (which
/// refines types within a function) and `ConstraintSolve` (which flows callee
/// param types into callers, but not the reverse). By observing what types
/// callers actually pass, we can narrow `Dynamic` params to concrete types.
///
/// Design decisions:
/// - Only narrows `Dynamic` → concrete. Never overrides an already-concrete
///   type (TypeInference's intra-function evidence is more reliable).
/// - If callers disagree on types, the param stays `Dynamic` (no union types).
/// - `CallIndirect` is skipped (no name to resolve).
/// - `SystemCall` is skipped (runtime stubs have known signatures).
/// - Self-calls (recursive) are skipped to avoid circular reasoning.
pub struct CallSiteTypeFlow;

/// Collected argument type observations: `(callee_name, param_index) → Vec<Type>`.
///
/// A `Dynamic` entry means a caller passed an unresolved value — this prevents
/// narrowing because Dynamic means "could be anything at runtime."
pub(crate) type Observations = HashMap<(String, usize), Vec<Type>>;

/// Collect argument types from all call sites in the module.
///
/// Shared by `CallSiteTypeFlow` (narrows Dynamic params) and
/// `CallSiteTypeWiden` (widens params whose ConstraintSolve-narrowed type
/// conflicts with what callers actually pass).
pub(crate) fn collect_call_site_types(module: &Module) -> Observations {
    let mut observations: Observations = HashMap::new();

    for func in module.functions.values() {
        for block in func.blocks.values() {
            for &inst_id in &block.insts {
                let inst = &func.insts[inst_id];
                match &inst.op {
                    Op::Call {
                        func: callee_name,
                        args,
                    } => {
                        // Skip self-calls (recursive).
                        if callee_name == &func.name {
                            continue;
                        }
                        for (i, &arg) in args.iter().enumerate() {
                            let ty = &func.value_types[arg];
                            observations
                                .entry((callee_name.clone(), i))
                                .or_default()
                                .push(ty.clone());
                        }
                    }
                    Op::MethodCall {
                        method,
                        args,
                        ..
                    } => {
                        // Skip self-calls.
                        if method == &func.name {
                            continue;
                        }
                        // MethodCall args exclude the receiver — args[0] is
                        // the first explicit argument, mapping to param[1]
                        // (param[0] is `self`). We record against param
                        // index i+1 so that write-back aligns with the
                        // callee's sig.params which includes self at [0].
                        for (i, &arg) in args.iter().enumerate() {
                            let ty = &func.value_types[arg];
                            observations
                                .entry((method.clone(), i + 1))
                                .or_default()
                                .push(ty.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    observations
}

/// Narrow a set of observed types to a single type, or `None` if they disagree
/// or if any caller passes `Dynamic` (meaning "could be anything at runtime").
fn narrow(types: &[Type]) -> Option<Type> {
    if types.is_empty() {
        return None;
    }
    // If any caller passes Dynamic, we can't narrow — the param genuinely
    // receives unknown types at runtime.
    if types.contains(&Type::Dynamic) {
        return None;
    }
    // ClassRef callers block narrowing. GML object class names (OBJT) are integer
    // indices at runtime, but TypeScript represents them as class constructors
    // (`typeof ClassName`). Narrowing a callee param to ClassRef would cause
    // TypeScript type errors whenever that param is used in numeric/arithmetic
    // contexts — the param type should stay Dynamic so the caller-passed class
    // constructor is treated as `any`.
    if types.iter().any(|t| matches!(t, Type::ClassRef(_))) {
        return None;
    }
    let first = &types[0];
    if types.iter().all(|t| t == first) {
        Some(first.clone())
    } else {
        // Callers disagree — leave as Dynamic (no union types for now).
        None
    }
}

impl Transform for CallSiteTypeFlow {
    fn name(&self) -> &str {
        "call-site-type-flow"
    }

    fn run_once(&self) -> bool {
        true
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let observations = collect_call_site_types(&module);
        let mut changed = false;

        // Build a name → func_id map for write-back.
        let name_to_id: HashMap<String, _> = module
            .functions
            .iter()
            .map(|(id, f)| (f.name.clone(), id))
            .collect();

        // For each observation, try to narrow the callee's param type.
        // Group observations by callee name first to avoid repeated lookups.
        let mut per_callee: HashMap<String, Vec<(usize, Vec<Type>)>> = HashMap::new();
        for ((name, idx), types) in &observations {
            per_callee
                .entry(name.clone())
                .or_default()
                .push((idx.to_owned(), types.clone()));
        }

        for (callee_name, params) in &per_callee {
            let func_id = match name_to_id.get(callee_name) {
                Some(&id) => id,
                None => continue, // External function — skip.
            };

            for &(param_idx, ref types) in params {
                let func = &module.functions[func_id];

                // Bounds check.
                if param_idx >= func.sig.params.len() {
                    continue;
                }

                // Only narrow Dynamic params.
                if func.sig.params[param_idx] != Type::Dynamic {
                    continue;
                }

                if let Some(narrowed) = narrow(types) {
                    let func = &mut module.functions[func_id];

                    // Update signature.
                    func.sig.params[param_idx] = narrowed.clone();

                    // Update entry block param type.
                    let entry = func.entry;
                    if param_idx < func.blocks[entry].params.len() {
                        func.blocks[entry].params[param_idx].ty = narrowed.clone();
                        let value = func.blocks[entry].params[param_idx].value;
                        func.value_types[value] = narrowed;
                    }

                    changed = true;
                }
            }
        }

        Ok(TransformResult { module, changed })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityRef;
    use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
    use crate::ir::ty::FunctionSig;
    use crate::ir::{FuncId, Type, Visibility};

    /// Helper: build a module, apply CallSiteTypeFlow, return result.
    fn run(mb: ModuleBuilder) -> TransformResult {
        CallSiteTypeFlow.apply(mb.build()).unwrap()
    }

    // ---- Basic narrowing ----

    /// Caller passes String to callee's Dynamic param → narrows to String.
    #[test]
    fn basic_narrowing() {
        let mut mb = ModuleBuilder::new("test");

        // Callee: fn target(x: Dynamic) → Void
        let callee_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut callee = FunctionBuilder::new("target", callee_sig, Visibility::Private);
        callee.ret(None);
        mb.add_function(callee.build());

        // Caller: fn caller() → Void; calls target(string_val)
        let caller_sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut caller = FunctionBuilder::new("caller", caller_sig, Visibility::Private);
        let s = caller.const_string("hello");
        caller.call("target", &[s], Type::Void);
        caller.ret(None);
        mb.add_function(caller.build());

        let result = run(mb);
        assert!(result.changed);

        let target = &result.module.functions[FuncId::new(0)];
        assert_eq!(target.sig.params[0], Type::String);

        // Entry block param and value_types should also be updated.
        let entry = target.entry;
        assert_eq!(target.blocks[entry].params[0].ty, Type::String);
        let val = target.blocks[entry].params[0].value;
        assert_eq!(target.value_types[val], Type::String);
    }

    // ---- Multiple callers agree ----

    /// Two callers both pass Float(64) → narrows to Float(64).
    #[test]
    fn multiple_callers_agree() {
        let mut mb = ModuleBuilder::new("test");

        let callee_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut callee = FunctionBuilder::new("target", callee_sig, Visibility::Private);
        callee.ret(None);
        mb.add_function(callee.build());

        // Caller A
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut caller_a = FunctionBuilder::new("caller_a", sig.clone(), Visibility::Private);
        let v = caller_a.const_float(1.0);
        caller_a.call("target", &[v], Type::Void);
        caller_a.ret(None);
        mb.add_function(caller_a.build());

        // Caller B
        let mut caller_b = FunctionBuilder::new("caller_b", sig, Visibility::Private);
        let v = caller_b.const_float(2.0);
        caller_b.call("target", &[v], Type::Void);
        caller_b.ret(None);
        mb.add_function(caller_b.build());

        let result = run(mb);
        assert!(result.changed);
        let target = &result.module.functions[FuncId::new(0)];
        assert_eq!(target.sig.params[0], Type::Float(64));
    }

    // ---- Multiple callers disagree ----

    /// One passes String, another Float(64) → stays Dynamic.
    #[test]
    fn multiple_callers_disagree() {
        let mut mb = ModuleBuilder::new("test");

        let callee_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut callee = FunctionBuilder::new("target", callee_sig, Visibility::Private);
        callee.ret(None);
        mb.add_function(callee.build());

        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut caller_a = FunctionBuilder::new("caller_a", sig.clone(), Visibility::Private);
        let v = caller_a.const_string("hi");
        caller_a.call("target", &[v], Type::Void);
        caller_a.ret(None);
        mb.add_function(caller_a.build());

        let mut caller_b = FunctionBuilder::new("caller_b", sig, Visibility::Private);
        let v = caller_b.const_float(2.0);
        caller_b.call("target", &[v], Type::Void);
        caller_b.ret(None);
        mb.add_function(caller_b.build());

        let result = run(mb);
        assert!(!result.changed);
        let target = &result.module.functions[FuncId::new(0)];
        assert_eq!(target.sig.params[0], Type::Dynamic);
    }

    // ---- No callers ----

    /// Function with Dynamic params but no call sites → stays Dynamic.
    #[test]
    fn no_callers() {
        let mut mb = ModuleBuilder::new("test");

        let callee_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut callee = FunctionBuilder::new("target", callee_sig, Visibility::Private);
        callee.ret(None);
        mb.add_function(callee.build());

        let result = run(mb);
        assert!(!result.changed);
        let target = &result.module.functions[FuncId::new(0)];
        assert_eq!(target.sig.params[0], Type::Dynamic);
    }

    // ---- Already typed ----

    /// Param is String from TypeInference, caller passes Float(64) → stays String.
    #[test]
    fn already_typed_not_overridden() {
        let mut mb = ModuleBuilder::new("test");

        let callee_sig = FunctionSig {
            params: vec![Type::String],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut callee = FunctionBuilder::new("target", callee_sig, Visibility::Private);
        callee.ret(None);
        mb.add_function(callee.build());

        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut caller = FunctionBuilder::new("caller", sig, Visibility::Private);
        let v = caller.const_float(1.0);
        caller.call("target", &[v], Type::Void);
        caller.ret(None);
        mb.add_function(caller.build());

        let result = run(mb);
        assert!(!result.changed);
        let target = &result.module.functions[FuncId::new(0)];
        assert_eq!(target.sig.params[0], Type::String);
    }

    // ---- MethodCall ----

    /// Method call narrows method's params (skipping receiver at index 0).
    #[test]
    fn method_call_narrows_params() {
        let mut mb = ModuleBuilder::new("test");

        // Method: fn Foo::bar(self: Struct(Foo), x: Dynamic) → Void
        let method_sig = FunctionSig {
            params: vec![Type::Struct("Foo".into()), Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut method =
            FunctionBuilder::new("Foo::bar", method_sig, Visibility::Private);
        method.ret(None);
        mb.add_function(method.build());

        // Caller calls receiver.bar(int_val)
        let sig = FunctionSig {
            params: vec![Type::Struct("Foo".into())],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut caller = FunctionBuilder::new("caller", sig, Visibility::Private);
        let recv = caller.param(0);
        let v = caller.const_int(42);
        caller.call_method(recv, "Foo::bar", &[v], Type::Void);
        caller.ret(None);
        mb.add_function(caller.build());

        let result = run(mb);
        assert!(result.changed);
        let method = &result.module.functions[FuncId::new(0)];
        // param[0] (self) should be untouched, param[1] should be narrowed.
        assert_eq!(method.sig.params[0], Type::Struct("Foo".into()));
        assert_eq!(method.sig.params[1], Type::Int(64));
    }

    // ---- Self-call (recursive) ----

    /// Recursive function doesn't use its own Dynamic params as evidence.
    #[test]
    fn self_call_ignored() {
        let mut mb = ModuleBuilder::new("test");

        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut func = FunctionBuilder::new("recurse", sig, Visibility::Private);
        let p = func.param(0); // Dynamic
        func.call("recurse", &[p], Type::Void);
        func.ret(None);
        mb.add_function(func.build());

        let result = run(mb);
        // Self-call passes Dynamic (which is skipped) and is also a self-call.
        // Either way, no narrowing.
        assert!(!result.changed);
        let f = &result.module.functions[FuncId::new(0)];
        assert_eq!(f.sig.params[0], Type::Dynamic);
    }

    // ---- Idempotent ----

    /// Running the pass twice produces no changes on second run.
    #[test]
    fn idempotent() {
        let mut mb = ModuleBuilder::new("test");

        let callee_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut callee = FunctionBuilder::new("target", callee_sig, Visibility::Private);
        callee.ret(None);
        mb.add_function(callee.build());

        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut caller = FunctionBuilder::new("caller", sig, Visibility::Private);
        let v = caller.const_string("hello");
        caller.call("target", &[v], Type::Void);
        caller.ret(None);
        mb.add_function(caller.build());

        let r1 = CallSiteTypeFlow.apply(mb.build()).unwrap();
        assert!(r1.changed);
        let r2 = CallSiteTypeFlow.apply(r1.module).unwrap();
        assert!(!r2.changed, "second run should report no changes");
    }

    // ---- Dynamic arg prevents narrowing ----

    /// A caller passing Dynamic prevents narrowing — Dynamic means "could be
    /// anything at runtime", so we can't safely narrow to a specific type.
    #[test]
    fn dynamic_arg_prevents_narrowing() {
        let mut mb = ModuleBuilder::new("test");

        let callee_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut callee = FunctionBuilder::new("target", callee_sig, Visibility::Private);
        callee.ret(None);
        mb.add_function(callee.build());

        // Caller A passes String.
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut caller_a = FunctionBuilder::new("caller_a", sig.clone(), Visibility::Private);
        let v = caller_a.const_string("hello");
        caller_a.call("target", &[v], Type::Void);
        caller_a.ret(None);
        mb.add_function(caller_a.build());

        // Caller B passes Dynamic (its own Dynamic param).
        let sig_b = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut caller_b = FunctionBuilder::new("caller_b", sig_b, Visibility::Private);
        let p = caller_b.param(0); // Dynamic — prevents narrowing
        caller_b.call("target", &[p], Type::Void);
        caller_b.ret(None);
        mb.add_function(caller_b.build());

        let result = run(mb);
        assert!(!result.changed);
        let target = &result.module.functions[FuncId::new(0)];
        // Dynamic arg prevents narrowing — stays Dynamic.
        assert_eq!(target.sig.params[0], Type::Dynamic);
    }

    // ---- Multiple params ----

    /// Multiple params: one narrows, another stays Dynamic due to disagreement.
    #[test]
    fn multiple_params_mixed() {
        let mut mb = ModuleBuilder::new("test");

        let callee_sig = FunctionSig {
            params: vec![Type::Dynamic, Type::Dynamic],
            return_ty: Type::Void,
            ..Default::default()
        };
        let mut callee = FunctionBuilder::new("target", callee_sig, Visibility::Private);
        callee.ret(None);
        mb.add_function(callee.build());

        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            ..Default::default()
        };

        // Caller A: target(string, int)
        let mut caller_a = FunctionBuilder::new("caller_a", sig.clone(), Visibility::Private);
        let s = caller_a.const_string("hi");
        let n = caller_a.const_int(1);
        caller_a.call("target", &[s, n], Type::Void);
        caller_a.ret(None);
        mb.add_function(caller_a.build());

        // Caller B: target(string, float) — param 0 agrees, param 1 disagrees
        let mut caller_b = FunctionBuilder::new("caller_b", sig, Visibility::Private);
        let s = caller_b.const_string("bye");
        let f = caller_b.const_float(2.0);
        caller_b.call("target", &[s, f], Type::Void);
        caller_b.ret(None);
        mb.add_function(caller_b.build());

        let result = run(mb);
        assert!(result.changed);
        let target = &result.module.functions[FuncId::new(0)];
        assert_eq!(target.sig.params[0], Type::String); // Agreed.
        assert_eq!(target.sig.params[1], Type::Dynamic); // Disagreed.
    }
}
