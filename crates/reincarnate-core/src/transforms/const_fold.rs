use std::collections::HashMap;

use crate::error::CoreError;
use crate::ir::inst::CmpKind;
use crate::ir::{Constant, Function, InstId, Module, Op, Type, ValueId};
use crate::pipeline::{Transform, TransformResult};

/// Constant folding transform — evaluates operations with all-constant operands
/// at compile time, replacing them with `Op::Const(result)`.
pub struct ConstantFolding;

/// Build a map from ValueId → Constant for all `Op::Const` instructions.
fn build_const_map(func: &Function) -> HashMap<ValueId, Constant> {
    let mut map = HashMap::new();
    for (_, inst) in func.insts.iter() {
        if let (Op::Const(c), Some(result)) = (&inst.op, inst.result) {
            map.insert(result, c.clone());
        }
    }
    map
}

/// Try to fold a binary arithmetic operation on two constants.
fn fold_binary_arith(
    op_name: &str,
    a: &Constant,
    b: &Constant,
) -> Option<Constant> {
    match (op_name, a, b) {
        ("add", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x.wrapping_add(*y))),
        ("add", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x.wrapping_add(*y))),
        ("add", Constant::Float(x), Constant::Float(y)) => Some(Constant::Float(x + y)),

        ("sub", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x.wrapping_sub(*y))),
        ("sub", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x.wrapping_sub(*y))),
        ("sub", Constant::Float(x), Constant::Float(y)) => Some(Constant::Float(x - y)),

        ("mul", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x.wrapping_mul(*y))),
        ("mul", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x.wrapping_mul(*y))),
        ("mul", Constant::Float(x), Constant::Float(y)) => Some(Constant::Float(x * y)),

        ("div", Constant::Int(_, ), Constant::Int(0)) => None,
        ("div", Constant::UInt(_), Constant::UInt(0)) => None,
        ("div", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x.wrapping_div(*y))),
        ("div", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x / y)),
        ("div", Constant::Float(x), Constant::Float(y)) => Some(Constant::Float(x / y)),

        ("rem", Constant::Int(_), Constant::Int(0)) => None,
        ("rem", Constant::UInt(_), Constant::UInt(0)) => None,
        ("rem", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x.wrapping_rem(*y))),
        ("rem", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x % y)),
        ("rem", Constant::Float(x), Constant::Float(y)) => Some(Constant::Float(x % y)),

        _ => None,
    }
}

/// Try to fold a bitwise binary operation on two constants.
fn fold_binary_bitwise(
    op_name: &str,
    a: &Constant,
    b: &Constant,
) -> Option<Constant> {
    match (op_name, a, b) {
        ("and", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x & y)),
        ("and", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x & y)),

        ("or", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x | y)),
        ("or", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x | y)),

        ("xor", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x ^ y)),
        ("xor", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x ^ y)),

        ("shl", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x.wrapping_shl(*y as u32))),
        ("shl", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x.wrapping_shl(*y as u32))),

        ("shr", Constant::Int(x), Constant::Int(y)) => Some(Constant::Int(x.wrapping_shr(*y as u32))),
        ("shr", Constant::UInt(x), Constant::UInt(y)) => Some(Constant::UInt(x.wrapping_shr(*y as u32))),

        _ => None,
    }
}

/// Try to fold a comparison of two constants.
fn fold_cmp(kind: CmpKind, a: &Constant, b: &Constant) -> Option<Constant> {
    let result = match (a, b) {
        (Constant::Int(x), Constant::Int(y)) => match kind {
            CmpKind::Eq | CmpKind::LooseEq => x == y,
            CmpKind::Ne | CmpKind::LooseNe => x != y,
            CmpKind::Lt => x < y,
            CmpKind::Le => x <= y,
            CmpKind::Gt => x > y,
            CmpKind::Ge => x >= y,
        },
        (Constant::UInt(x), Constant::UInt(y)) => match kind {
            CmpKind::Eq | CmpKind::LooseEq => x == y,
            CmpKind::Ne | CmpKind::LooseNe => x != y,
            CmpKind::Lt => x < y,
            CmpKind::Le => x <= y,
            CmpKind::Gt => x > y,
            CmpKind::Ge => x >= y,
        },
        (Constant::Float(x), Constant::Float(y)) => match kind {
            CmpKind::Eq | CmpKind::LooseEq => x == y,
            CmpKind::Ne | CmpKind::LooseNe => x != y,
            CmpKind::Lt => x < y,
            CmpKind::Le => x <= y,
            CmpKind::Gt => x > y,
            CmpKind::Ge => x >= y,
        },
        (Constant::String(x), Constant::String(y)) => match kind {
            CmpKind::Eq | CmpKind::LooseEq => x == y,
            CmpKind::Ne | CmpKind::LooseNe => x != y,
            CmpKind::Lt => x < y,
            CmpKind::Le => x <= y,
            CmpKind::Gt => x > y,
            CmpKind::Ge => x >= y,
        },
        _ => return None,
    };
    Some(Constant::Bool(result))
}

/// Try to fold a cast from a constant to a target type.
fn fold_cast(c: &Constant, ty: &Type) -> Option<Constant> {
    match (c, ty) {
        // Same-family: truncate/widen within the stored representation.
        (Constant::Int(x), Type::Int(bits)) => {
            let bits = *bits as u32;
            if bits >= 64 {
                Some(Constant::Int(*x))
            } else {
                // Truncate and sign-extend from bit (bits-1).
                let mask = (1i64 << bits) - 1;
                let truncated = *x & mask;
                let sign_bit = 1i64 << (bits - 1);
                if truncated & sign_bit != 0 {
                    Some(Constant::Int(truncated | !mask))
                } else {
                    Some(Constant::Int(truncated))
                }
            }
        }
        (Constant::UInt(x), Type::UInt(bits)) => {
            let bits = *bits as u32;
            if bits >= 64 {
                Some(Constant::UInt(*x))
            } else {
                Some(Constant::UInt(*x & ((1u64 << bits) - 1)))
            }
        }
        (Constant::Float(_), Type::Float(_)) => Some(c.clone()),
        // Cross-family conversions.
        (Constant::Int(x), Type::Float(_)) => Some(Constant::Float(*x as f64)),
        (Constant::Int(x), Type::UInt(_)) => Some(Constant::UInt(*x as u64)),
        (Constant::UInt(x), Type::Float(_)) => Some(Constant::Float(*x as f64)),
        (Constant::UInt(x), Type::Int(_)) => Some(Constant::Int(*x as i64)),
        (Constant::Float(x), Type::Int(_)) => Some(Constant::Int(*x as i64)),
        (Constant::Float(x), Type::UInt(_)) => Some(Constant::UInt(*x as u64)),
        _ => None,
    }
}

/// Try to fold a single instruction given the current constant map.
/// Returns the folded constant if successful.
fn try_fold(op: &Op, consts: &HashMap<ValueId, Constant>) -> Option<Constant> {
    match op {
        // Binary arithmetic
        Op::Add(a, b) => fold_binary_arith("add", consts.get(a)?, consts.get(b)?),
        Op::Sub(a, b) => fold_binary_arith("sub", consts.get(a)?, consts.get(b)?),
        Op::Mul(a, b) => fold_binary_arith("mul", consts.get(a)?, consts.get(b)?),
        Op::Div(a, b) => fold_binary_arith("div", consts.get(a)?, consts.get(b)?),
        Op::Rem(a, b) => fold_binary_arith("rem", consts.get(a)?, consts.get(b)?),

        // Unary negation
        Op::Neg(a) => {
            let c = consts.get(a)?;
            match c {
                Constant::Int(x) => Some(Constant::Int(x.wrapping_neg())),
                Constant::UInt(x) => Some(Constant::Int(-(*x as i64))),
                Constant::Float(x) => Some(Constant::Float(-x)),
                _ => None,
            }
        }

        // Binary bitwise
        Op::BitAnd(a, b) => fold_binary_bitwise("and", consts.get(a)?, consts.get(b)?),
        Op::BitOr(a, b) => fold_binary_bitwise("or", consts.get(a)?, consts.get(b)?),
        Op::BitXor(a, b) => fold_binary_bitwise("xor", consts.get(a)?, consts.get(b)?),
        Op::Shl(a, b) => fold_binary_bitwise("shl", consts.get(a)?, consts.get(b)?),
        Op::Shr(a, b) => fold_binary_bitwise("shr", consts.get(a)?, consts.get(b)?),

        // Unary bitwise not
        Op::BitNot(a) => {
            let c = consts.get(a)?;
            match c {
                Constant::Int(x) => Some(Constant::Int(!x)),
                Constant::UInt(x) => Some(Constant::UInt(!x)),
                _ => None,
            }
        }

        // Comparison
        Op::Cmp(kind, a, b) => fold_cmp(*kind, consts.get(a)?, consts.get(b)?),

        // Logical not
        Op::Not(a) => {
            if let Constant::Bool(v) = consts.get(a)? {
                Some(Constant::Bool(!v))
            } else {
                None
            }
        }

        // Cast
        Op::Cast(a, ty, _) => fold_cast(consts.get(a)?, ty),

        // Copy propagation
        Op::Copy(a) => consts.get(a).cloned(),

        // Pure type-conversion function calls: int(x), uint(x), real(x), string(x)
        Op::Call { func, args } if args.len() == 1 => {
            let c = consts.get(&args[0])?;
            match (func.as_str(), c) {
                ("int", Constant::Int(x)) => Some(Constant::Int(*x)),
                ("int", Constant::UInt(x)) => Some(Constant::Int(*x as i64)),
                ("int", Constant::Float(x)) => Some(Constant::Int(*x as i64)),
                ("uint", Constant::Int(x)) => Some(Constant::UInt(*x as u64)),
                ("uint", Constant::UInt(x)) => Some(Constant::UInt(*x)),
                ("uint", Constant::Float(x)) => Some(Constant::UInt(*x as u64)),
                ("real", Constant::Int(x)) => Some(Constant::Float(*x as f64)),
                ("real", Constant::UInt(x)) => Some(Constant::Float(*x as f64)),
                ("real", Constant::Float(x)) => Some(Constant::Float(*x)),
                ("string", Constant::Int(x)) => Some(Constant::String(x.to_string())),
                ("string", Constant::UInt(x)) => Some(Constant::String(x.to_string())),
                ("string", Constant::Float(x)) => Some(Constant::String(x.to_string())),
                ("string", Constant::String(_)) => Some(c.clone()),
                ("string", Constant::Bool(b)) => {
                    Some(Constant::String(if *b { "1" } else { "0" }.into()))
                }
                _ => None,
            }
        }

        _ => None,
    }
}

/// Peephole: rewrite `Not(Cmp(kind, a, b))` → `Cmp(inverse(kind), a, b)`.
///
/// This turns `!(x >= 1)` into `x < 1` at the IR level, so the emitter
/// doesn't need to handle comparison flipping as a syntax transformation.
fn fold_not_cmp(func: &mut Function) -> bool {
    // Map from ValueId → (CmpKind, lhs, rhs) for all Cmp instructions.
    let mut cmp_defs: HashMap<ValueId, (CmpKind, ValueId, ValueId)> = HashMap::new();
    for (_, inst) in func.insts.iter() {
        if let (Op::Cmp(kind, a, b), Some(result)) = (&inst.op, inst.result) {
            cmp_defs.insert(result, (*kind, *a, *b));
        }
    }

    let updates: Vec<(InstId, CmpKind, ValueId, ValueId)> = func
        .insts
        .keys()
        .filter_map(|inst_id| {
            let inst = &func.insts[inst_id];
            if let Op::Not(inner) = &inst.op {
                let &(kind, a, b) = cmp_defs.get(inner)?;
                return Some((inst_id, kind.inverse(), a, b));
            }
            None
        })
        .collect();

    let changed = !updates.is_empty();
    for (inst_id, inv_kind, a, b) in updates {
        func.insts[inst_id].op = Op::Cmp(inv_kind, a, b);
    }
    changed
}

/// Run constant folding on a single function. Returns true if any changes were made.
fn fold_function(func: &mut Function) -> bool {
    let mut any_changed = false;

    loop {
        let consts = build_const_map(func);
        let mut changed = false;

        // Collect updates: (InstId, result ValueId, folded Constant)
        let updates: Vec<(InstId, ValueId, Constant)> = func
            .insts
            .keys()
            .filter_map(|inst_id| {
                let inst = &func.insts[inst_id];
                let result = inst.result?;
                // Skip instructions that are already constants.
                if matches!(&inst.op, Op::Const(_)) {
                    return None;
                }
                let folded = try_fold(&inst.op, &consts)?;
                Some((inst_id, result, folded))
            })
            .collect();

        for (inst_id, result, constant) in updates {
            let ty = constant.ty();
            func.insts[inst_id].op = Op::Const(constant);
            func.value_types[result] = ty;
            changed = true;
        }

        if !changed {
            break;
        }
        any_changed = true;
    }

    // Peephole: Not(Cmp) → Cmp(inverse). Runs after constant folding
    // since it doesn't create new folding opportunities.
    any_changed |= fold_not_cmp(func);

    // Fold BrIf with a constant condition → unconditional Br.
    any_changed |= fold_brif_constants(func);

    any_changed
}

/// JavaScript-style truthiness for IR constants.
///
/// Returns `Some(true)` if the constant is always truthy, `Some(false)` if
/// always falsy, or `None` if undetermined.
fn is_constant_truthy(c: &Constant) -> Option<bool> {
    match c {
        Constant::Bool(b) => Some(*b),
        Constant::Int(x) => Some(*x != 0),
        Constant::UInt(x) => Some(*x != 0),
        Constant::Float(x) => Some(*x != 0.0 && !x.is_nan()),
        Constant::String(s) => Some(!s.is_empty()),
        Constant::Null => Some(false),
    }
}

/// Fold `BrIf(const_cond, then, else)` → `Br(then)` or `Br(else)`.
///
/// Runs after the main constant-folding loop so that any conditions that were
/// just folded to constants are handled.  The resulting dead branch will be
/// pruned by DCE.
fn fold_brif_constants(func: &mut Function) -> bool {
    let consts = build_const_map(func);

    let updates: Vec<(InstId, Op)> = func
        .insts
        .keys()
        .filter_map(|inst_id| {
            let inst = &func.insts[inst_id];
            if let Op::BrIf {
                cond,
                then_target,
                then_args,
                else_target,
                else_args,
            } = &inst.op
            {
                let c = consts.get(cond)?;
                let truthy = is_constant_truthy(c)?;
                let new_op = if truthy {
                    Op::Br {
                        target: *then_target,
                        args: then_args.clone(),
                    }
                } else {
                    Op::Br {
                        target: *else_target,
                        args: else_args.clone(),
                    }
                };
                Some((inst_id, new_op))
            } else {
                None
            }
        })
        .collect();

    let changed = !updates.is_empty();
    for (inst_id, new_op) in updates {
        func.insts[inst_id].op = new_op;
    }
    changed
}

impl Transform for ConstantFolding {
    fn name(&self) -> &str {
        "constant-folding"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let mut changed = false;
        for func_id in module.functions.keys().collect::<Vec<_>>() {
            changed |= fold_function(&mut module.functions[func_id]);
        }
        Ok(TransformResult {
            module,
            changed,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityRef;
    use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
    use crate::ir::ty::FunctionSig;
    use crate::ir::{FuncId, Visibility};

    fn apply_fold(func: crate::ir::Function) -> crate::ir::Function {
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();
        let result = ConstantFolding.apply(module).unwrap();
        result.module.functions[FuncId::new(0)].clone()
    }

    /// Find the instruction that produces a given value.
    fn find_inst_for(func: &crate::ir::Function, value: ValueId) -> &crate::ir::Inst {
        func.insts
            .iter()
            .find(|(_, inst)| inst.result == Some(value))
            .map(|(_, inst)| inst)
            .expect("no instruction produces this value")
    }

    /// `2 + 3` folds to `Int(5)`.
    #[test]
    fn int_arithmetic() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(2);
        let b = fb.const_int(3);
        let sum = fb.add(a, b);
        fb.ret(Some(sum));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, sum).op, Op::Const(Constant::Int(5))));
        assert_eq!(func.value_types[sum], Type::Int(64));
    }

    /// `1.5 * 2.0` folds to `Float(3.0)`.
    #[test]
    fn float_arithmetic() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Float(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_float(1.5);
        let b = fb.const_float(2.0);
        let product = fb.mul(a, b);
        fb.ret(Some(product));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, product).op, Op::Const(Constant::Float(f)) if *f == 3.0));
    }

    /// `5 < 10` folds to `Bool(true)`.
    #[test]
    fn comparison() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(5);
        let b = fb.const_int(10);
        let cmp = fb.cmp(CmpKind::Lt, a, b);
        fb.ret(Some(cmp));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, cmp).op, Op::Const(Constant::Bool(true))));
    }

    /// `not(true)` folds to `Bool(false)`.
    #[test]
    fn logical_not() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_bool(true);
        let result = fb.not(a);
        fb.ret(Some(result));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, result).op, Op::Const(Constant::Bool(false))));
    }

    /// `5 / 0` stays unfolded (division by zero).
    #[test]
    fn division_by_zero_preserved() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(5);
        let b = fb.const_int(0);
        let div = fb.div(a, b);
        fb.ret(Some(div));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, div).op, Op::Div(_, _)));
    }

    /// `param + 3` stays unfolded (non-constant operand).
    #[test]
    fn non_constant_operand() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let param = fb.param(0);
        let b = fb.const_int(3);
        let sum = fb.add(param, b);
        fb.ret(Some(sum));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, sum).op, Op::Add(_, _)));
    }

    /// `neg(42)` folds to `Int(-42)`.
    #[test]
    fn negation() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(42);
        let result = fb.neg(a);
        fb.ret(Some(result));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, result).op, Op::Const(Constant::Int(-42))));
    }

    /// `Not(Cmp(Ge, param, 1))` folds to `Cmp(Lt, param, 1)`.
    #[test]
    fn not_cmp_folds_to_inverse() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Bool, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let param = fb.param(0);
        let one = fb.const_int(1);
        let cmp = fb.cmp(CmpKind::Ge, param, one);
        let result = fb.not(cmp);
        fb.ret(Some(result));

        let func = apply_fold(fb.build());
        let inst = find_inst_for(&func, result);
        assert!(
            matches!(&inst.op, Op::Cmp(CmpKind::Lt, a, b) if *a == param && *b == one),
            "expected Cmp(Lt, param, 1), got {:?}",
            inst.op
        );
    }

    // ---- Identity & idempotency tests ----

    /// No constants in arithmetic → nothing to fold, changed == false.
    #[test]
    fn identity_no_change() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.param(0);
        let b = fb.param(1);
        let sum = fb.add(a, b);
        fb.ret(Some(sum));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = ConstantFolding.apply(module).unwrap();
        assert!(!result.changed);
    }

    /// Folding is idempotent: second apply reports no change.
    #[test]
    fn idempotent_after_transform() {
        use crate::transforms::util::test_helpers::assert_idempotent;
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(2);
        let b = fb.const_int(3);
        let sum = fb.add(a, b);
        fb.ret(Some(sum));
        assert_idempotent(&ConstantFolding, fb.build());
    }

    /// `0xFF & 0x0F` folds to `Int(15)`.
    #[test]
    fn bitwise_and() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(0xFF);
        let b = fb.const_int(0x0F);
        let result = fb.bit_and(a, b);
        fb.ret(Some(result));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, result).op, Op::Const(Constant::Int(15))));
    }

    // ---- Edge case tests ----

    /// Void function with no arithmetic — nothing to fold.
    #[test]
    fn void_function_no_fold() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        fb.ret(None);

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = ConstantFolding.apply(module).unwrap();
        assert!(!result.changed);
    }

    /// Constants in different blocks all fold.
    #[test]
    fn fold_in_multi_block() {
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);
        let then_block = fb.create_block();
        let else_block = fb.create_block();
        fb.br_if(cond, then_block, &[], else_block, &[]);

        fb.switch_to_block(then_block);
        let a = fb.const_int(2);
        let b = fb.const_int(3);
        let sum = fb.add(a, b);
        fb.ret(Some(sum));

        fb.switch_to_block(else_block);
        let c = fb.const_int(10);
        let d = fb.const_int(20);
        let product = fb.mul(c, d);
        fb.ret(Some(product));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, sum).op, Op::Const(Constant::Int(5))));
        assert!(matches!(&find_inst_for(&func, product).op, Op::Const(Constant::Int(200))));
    }

    /// Chained fold: `(1+2) * 3` folds to `9` in one pass via fixpoint.
    #[test]
    fn chained_fold() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(1);
        let b = fb.const_int(2);
        let sum = fb.add(a, b);
        let c = fb.const_int(3);
        let product = fb.mul(sum, c);
        fb.ret(Some(product));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, product).op, Op::Const(Constant::Int(9))));
    }

    // ---- Adversarial tests ----

    /// Same constant used in foldable and non-foldable contexts.
    #[test]
    fn shared_constant_folded_and_used() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let param = fb.param(0);
        let c = fb.const_int(5);
        let fold_me = fb.add(c, c);     // foldable: 5+5=10
        let keep_me = fb.add(param, c);  // not foldable: param+5
        let result = fb.add(fold_me, keep_me);
        fb.ret(Some(result));

        let func = apply_fold(fb.build());
        assert!(matches!(&find_inst_for(&func, fold_me).op, Op::Const(Constant::Int(10))));
        assert!(matches!(&find_inst_for(&func, keep_me).op, Op::Add(_, _)));
    }

    /// i64::MAX + 1 wraps correctly (wrapping_add).
    #[test]
    fn overflow_wraps() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.const_int(i64::MAX);
        let b = fb.const_int(1);
        let sum = fb.add(a, b);
        fb.ret(Some(sum));

        let func = apply_fold(fb.build());
        // i64::MAX + 1 wraps to i64::MIN.
        assert!(matches!(
            &find_inst_for(&func, sum).op,
            Op::Const(Constant::Int(v)) if *v == i64::MIN
        ));
    }

    /// NaN arithmetic: NaN + 1.0 should fold to NaN.
    #[test]
    fn float_nan_propagation() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Float(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let nan = fb.const_float(f64::NAN);
        let one = fb.const_float(1.0);
        let sum = fb.add(nan, one);
        fb.ret(Some(sum));

        let func = apply_fold(fb.build());
        if let Op::Const(Constant::Float(v)) = &find_inst_for(&func, sum).op {
            assert!(v.is_nan(), "NaN + 1.0 should be NaN");
        } else {
            panic!("expected float constant");
        }
    }

    /// Deeply chained folds: 10-deep chain folds completely.
    #[test]
    fn deeply_chained_folds() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let mut acc = fb.const_int(0);
        for i in 1..=10 {
            let c = fb.const_int(i);
            acc = fb.add(acc, c);
        }
        fb.ret(Some(acc));

        let func = apply_fold(fb.build());
        // 0+1+2+...+10 = 55
        assert!(matches!(
            &find_inst_for(&func, acc).op,
            Op::Const(Constant::Int(55))
        ));
    }
}
