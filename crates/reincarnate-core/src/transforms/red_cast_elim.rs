use crate::error::CoreError;
use crate::ir::{Function, Module, Op};
use crate::pipeline::{Transform, TransformResult};

/// Redundant cast elimination — rewrites `Cast(v, ty)` to `Copy(v)` when
/// `value_types[v]` already matches `ty`.
///
/// This runs after type inference refines types so that casts inserted by the
/// frontend (e.g., `as boolean` on a method that already returns Bool) become
/// redundant. Mem2Reg then eliminates the Copy, and DCE cleans up.
pub struct RedundantCastElimination;

/// Eliminate redundant casts in a single function.
/// Returns true if any changes were made.
fn elim_function(func: &mut Function) -> bool {
    let mut changed = false;

    for inst_id in func.insts.keys().collect::<Vec<_>>() {
        if let Op::Cast(value, ref ty, _) = func.insts[inst_id].op {
            if func.value_types[value] == *ty {
                func.insts[inst_id].op = Op::Copy(value);
                changed = true;
            }
        }
    }

    changed
}

impl Transform for RedundantCastElimination {
    fn name(&self) -> &str {
        "redundant-cast-elimination"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;
        for func_id in module.functions.keys().collect::<Vec<_>>() {
            changed |= elim_function(&mut module.functions[func_id]);
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

    // ---- Identity & idempotency tests ----

    /// All casts cross types (Int → Bool) → no changes.
    #[test]
    fn identity_no_change() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let val = fb.const_int(1); // Int(64)
        let cast = fb.cast(val, Type::Bool);
        fb.ret(Some(cast));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = RedundantCastElimination.apply(module).unwrap();
        assert!(!result.changed);
    }

    /// Redundant cast elimination is idempotent.
    #[test]
    fn idempotent_after_transform() {
        use crate::transforms::util::test_helpers::assert_idempotent;
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(1);
        let b = fb.const_int(1);
        let val = fb.cmp(crate::ir::CmpKind::Eq, a, b);
        let cast = fb.cast(val, Type::Bool);
        fb.ret(Some(cast));
        assert_idempotent(&RedundantCastElimination, fb.build());
    }

    /// Redundant cast (Bool → Bool) is rewritten to Copy.
    #[test]
    fn redundant_cast_rewritten_to_copy() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(1);
        let b = fb.const_int(1);
        let val = fb.cmp(crate::ir::CmpKind::Eq, a, b);
        let cast = fb.cast(val, Type::Bool);
        fb.ret(Some(cast));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = RedundantCastElimination.apply(module).unwrap();
        assert!(result.changed);

        let func = &result.module.functions[FuncId::new(0)];
        assert!(
            matches!(func.insts.values().find(|i| i.result == Some(cast)).unwrap().op, Op::Copy(_)),
            "redundant cast should become Copy"
        );
    }

    // ---- Edge case tests ----

    /// Coerce vs AsType — both kinds tested when redundant.
    #[test]
    fn coerce_redundant_rewritten() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let p = fb.param(0);
        let coerced = fb.coerce(p, Type::Int(64));
        fb.ret(Some(coerced));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = RedundantCastElimination.apply(module).unwrap();
        assert!(result.changed, "same-type coerce should be eliminated");
        let func = &result.module.functions[FuncId::new(0)];
        assert!(matches!(
            func.insts.values().find(|i| i.result == Some(coerced)).unwrap().op,
            Op::Copy(_)
        ));
    }

    /// Chain of same-type casts: Cast(Cast(x, Int), Int) → both become Copy.
    #[test]
    fn chain_of_casts() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let p = fb.param(0);
        let c1 = fb.cast(p, Type::Int(64));
        let c2 = fb.cast(c1, Type::Int(64));
        fb.ret(Some(c2));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = RedundantCastElimination.apply(module).unwrap();
        assert!(result.changed);
        let func = &result.module.functions[FuncId::new(0)];
        // Both casts should be Copy.
        let c1_inst = func.insts.values().find(|i| i.result == Some(c1)).unwrap();
        assert!(matches!(c1_inst.op, Op::Copy(_)));
        let c2_inst = func.insts.values().find(|i| i.result == Some(c2)).unwrap();
        assert!(matches!(c2_inst.op, Op::Copy(_)));
    }

    /// Non-redundant cast (Int → Bool) is left unchanged.
    #[test]
    fn non_redundant_cast_unchanged() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let val = fb.const_int(1); // Type::Int(64)
        let cast = fb.cast(val, Type::Bool);
        fb.ret(Some(cast));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let result = RedundantCastElimination.apply(module).unwrap();
        assert!(!result.changed);

        let func = &result.module.functions[FuncId::new(0)];
        assert!(
            matches!(&func.insts.values().find(|i| i.result == Some(cast)).unwrap().op, Op::Cast(_, ty, _) if *ty == Type::Bool),
            "non-redundant cast should remain Cast"
        );
    }
}
