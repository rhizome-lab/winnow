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
