use std::collections::HashMap;

use crate::ir::{BlockId, Op, ValueId};

/// Extract branch targets from a control-flow instruction.
pub fn branch_targets(op: &Op) -> Vec<BlockId> {
    match op {
        Op::Br { target, .. } => vec![*target],
        Op::BrIf {
            then_target,
            else_target,
            ..
        } => vec![*then_target, *else_target],
        Op::Switch {
            cases, default, ..
        } => {
            let mut targets: Vec<BlockId> = cases.iter().map(|(_, t, _)| *t).collect();
            targets.push(default.0);
            targets
        }
        _ => vec![],
    }
}

/// Extract all ValueId operands from an Op.
pub fn value_operands(op: &Op) -> Vec<ValueId> {
    match op {
        Op::Const(_) => vec![],
        Op::Add(a, b)
        | Op::Sub(a, b)
        | Op::Mul(a, b)
        | Op::Div(a, b)
        | Op::Rem(a, b)
        | Op::BitAnd(a, b)
        | Op::BitOr(a, b)
        | Op::BitXor(a, b)
        | Op::Shl(a, b)
        | Op::Shr(a, b) => vec![*a, *b],
        Op::Neg(a) | Op::BitNot(a) | Op::Not(a) | Op::Copy(a) => vec![*a],
        Op::Select {
            cond,
            on_true,
            on_false,
        } => vec![*cond, *on_true, *on_false],
        Op::Cmp(_, a, b) => vec![*a, *b],
        Op::Br { args, .. } => args.clone(),
        Op::BrIf {
            cond,
            then_args,
            else_args,
            ..
        } => {
            let mut ops = vec![*cond];
            ops.extend(then_args);
            ops.extend(else_args);
            ops
        }
        Op::Switch {
            value,
            cases,
            default,
            ..
        } => {
            let mut ops = vec![*value];
            for (_, _, args) in cases {
                ops.extend(args);
            }
            ops.extend(&default.1);
            ops
        }
        Op::Return(v) | Op::Yield(v) => v.iter().copied().collect(),
        Op::Alloc(_) => vec![],
        Op::Load(ptr) => vec![*ptr],
        Op::Store { ptr, value } => vec![*ptr, *value],
        Op::GetField { object, .. } => vec![*object],
        Op::SetField { object, value, .. } => vec![*object, *value],
        Op::GetIndex { collection, index } => vec![*collection, *index],
        Op::SetIndex {
            collection,
            index,
            value,
        } => vec![*collection, *index, *value],
        Op::Call { args, .. } => args.clone(),
        Op::CallIndirect { callee, args } => {
            let mut ops = vec![*callee];
            ops.extend(args);
            ops
        }
        Op::SystemCall { args, .. } => args.clone(),
        Op::Cast(a, _) | Op::TypeCheck(a, _) => vec![*a],
        Op::StructInit { fields, .. } => fields.iter().map(|(_, v)| *v).collect(),
        Op::ArrayInit(elems) | Op::TupleInit(elems) => elems.clone(),
        Op::CoroutineCreate { args, .. } => args.clone(),
        Op::CoroutineResume(v) => vec![*v],
        Op::GlobalRef(_) => vec![],
    }
}

/// Replace ValueIds in an Op using a substitution map.
pub fn substitute_values_in_op(op: &mut Op, subst: &HashMap<ValueId, ValueId>) {
    let sub = |v: &mut ValueId| {
        if let Some(&new) = subst.get(v) {
            *v = new;
        }
    };

    match op {
        Op::Const(_) => {}
        Op::Add(a, b)
        | Op::Sub(a, b)
        | Op::Mul(a, b)
        | Op::Div(a, b)
        | Op::Rem(a, b)
        | Op::BitAnd(a, b)
        | Op::BitOr(a, b)
        | Op::BitXor(a, b)
        | Op::Shl(a, b)
        | Op::Shr(a, b) => {
            sub(a);
            sub(b);
        }
        Op::Neg(a) | Op::BitNot(a) | Op::Not(a) | Op::Copy(a) => sub(a),
        Op::Select {
            cond,
            on_true,
            on_false,
        } => {
            sub(cond);
            sub(on_true);
            sub(on_false);
        }
        Op::Cmp(_, a, b) => {
            sub(a);
            sub(b);
        }
        Op::Br { args, .. } => {
            for a in args {
                sub(a);
            }
        }
        Op::BrIf {
            cond,
            then_args,
            else_args,
            ..
        } => {
            sub(cond);
            for a in then_args {
                sub(a);
            }
            for a in else_args {
                sub(a);
            }
        }
        Op::Switch {
            value,
            cases,
            default,
            ..
        } => {
            sub(value);
            for (_, _, args) in cases {
                for a in args {
                    sub(a);
                }
            }
            for a in &mut default.1 {
                sub(a);
            }
        }
        Op::Return(v) | Op::Yield(v) => {
            if let Some(v) = v {
                sub(v);
            }
        }
        Op::Alloc(_) => {}
        Op::Load(ptr) => sub(ptr),
        Op::Store { ptr, value } => {
            sub(ptr);
            sub(value);
        }
        Op::GetField { object, .. } => sub(object),
        Op::SetField { object, value, .. } => {
            sub(object);
            sub(value);
        }
        Op::GetIndex { collection, index } => {
            sub(collection);
            sub(index);
        }
        Op::SetIndex {
            collection,
            index,
            value,
        } => {
            sub(collection);
            sub(index);
            sub(value);
        }
        Op::Call { args, .. } => {
            for a in args {
                sub(a);
            }
        }
        Op::CallIndirect { callee, args } => {
            sub(callee);
            for a in args {
                sub(a);
            }
        }
        Op::SystemCall { args, .. } => {
            for a in args {
                sub(a);
            }
        }
        Op::Cast(a, _) | Op::TypeCheck(a, _) => sub(a),
        Op::StructInit { fields, .. } => {
            for (_, v) in fields {
                sub(v);
            }
        }
        Op::ArrayInit(elems) | Op::TupleInit(elems) => {
            for e in elems {
                sub(e);
            }
        }
        Op::CoroutineCreate { args, .. } => {
            for a in args {
                sub(a);
            }
        }
        Op::CoroutineResume(v) => sub(v),
        Op::GlobalRef(_) => {}
    }
}
