//! Stress tests — systematically exercise transform passes on varied IR shapes.
//!
//! These tests generate IR programmatically with varying block counts, instruction
//! mixes, and type combinations, then run each pass and verify:
//! 1. No panics
//! 2. Output is well-formed
//! 3. Passes are idempotent (second run reports no changes)

use crate::entity::EntityRef;
use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
use crate::ir::ty::FunctionSig;
use crate::ir::{FuncId, Type, Visibility};
use crate::pipeline::Transform;
use crate::transforms::util::test_helpers::assert_well_formed;
use crate::transforms::{
    BoolLiteralReturn, CfgSimplify, ConstantFolding, ConstraintSolve, DeadCodeElimination,
    Mem2Reg, RedundantCastElimination, TypeInference,
};

/// All types we test with.
const TYPES: &[Type] = &[
    Type::Dynamic,
    Type::Int(64),
    Type::Int(32),
    Type::Float(64),
    Type::Bool,
    Type::String,
];

/// Build a linear chain of N blocks, each doing some arithmetic, returning the result.
fn build_linear_chain(n: usize, ty: Type) -> crate::ir::Function {
    let sig = FunctionSig {
        params: vec![ty.clone()],
        return_ty: ty.clone(),
        ..Default::default()
    };
    let mut fb = FunctionBuilder::new("chain", sig, Visibility::Private);
    let mut current = fb.param(0);

    for _ in 0..n {
        let next_block = fb.create_block();
        fb.br(next_block, &[]);
        fb.switch_to_block(next_block);
        let one = fb.const_int(1);
        current = fb.add(current, one);
    }

    fb.ret(Some(current));
    fb.build()
}

/// Build a diamond CFG with block params, returning a merge of both branches.
fn build_diamond(ty: Type) -> crate::ir::Function {
    let sig = FunctionSig {
        params: vec![Type::Bool, ty.clone()],
        return_ty: ty.clone(),
        ..Default::default()
    };
    let mut fb = FunctionBuilder::new("diamond", sig, Visibility::Private);
    let cond = fb.param(0);
    let val = fb.param(1);

    let then_b = fb.create_block();
    let else_b = fb.create_block();
    let (merge, merge_params) = fb.create_block_with_params(&[ty]);

    fb.br_if(cond, then_b, &[], else_b, &[]);

    fb.switch_to_block(then_b);
    let one = fb.const_int(1);
    let sum = fb.add(val, one);
    fb.br(merge, &[sum]);

    fb.switch_to_block(else_b);
    fb.br(merge, &[val]);

    fb.switch_to_block(merge);
    fb.ret(Some(merge_params[0]));

    fb.build()
}

/// Build a simple loop: entry → header(param) → body → header, exit → ret.
fn build_loop(ty: Type) -> crate::ir::Function {
    let sig = FunctionSig {
        params: vec![Type::Bool, ty.clone()],
        return_ty: ty.clone(),
        ..Default::default()
    };
    let mut fb = FunctionBuilder::new("loopy", sig, Visibility::Private);
    let cond = fb.param(0);
    let init = fb.param(1);

    let (header, header_params) = fb.create_block_with_params(&[ty]);
    let body = fb.create_block();
    let exit = fb.create_block();

    fb.br(header, &[init]);

    fb.switch_to_block(header);
    fb.br_if(cond, body, &[], exit, &[]);

    fb.switch_to_block(body);
    let one = fb.const_int(1);
    let next = fb.add(header_params[0], one);
    fb.br(header, &[next]);

    fb.switch_to_block(exit);
    fb.ret(Some(header_params[0]));

    fb.build()
}

/// Build a function with alloc/store/load pattern.
fn build_alloc_pattern(ty: Type) -> crate::ir::Function {
    let sig = FunctionSig {
        params: vec![ty.clone()],
        return_ty: ty.clone(),
        ..Default::default()
    };
    let mut fb = FunctionBuilder::new("alloc_test", sig, Visibility::Private);
    let val = fb.param(0);
    let ptr = fb.alloc(ty.clone());
    fb.store(ptr, val);
    let loaded = fb.load(ptr, ty);
    fb.ret(Some(loaded));
    fb.build()
}

/// Build a function with redundant casts.
fn build_cast_chain(ty: Type) -> crate::ir::Function {
    let sig = FunctionSig {
        params: vec![ty.clone()],
        return_ty: ty.clone(),
        ..Default::default()
    };
    let mut fb = FunctionBuilder::new("cast_chain", sig, Visibility::Private);
    let val = fb.param(0);
    let c1 = fb.cast(val, ty.clone());
    let c2 = fb.coerce(c1, ty.clone());
    let c3 = fb.cast(c2, ty);
    fb.ret(Some(c3));
    fb.build()
}

/// Build a function that returns 0 or 1 (bool-literal-return candidate).
fn build_bool_return() -> crate::ir::Function {
    let sig = FunctionSig {
        params: vec![Type::Bool],
        return_ty: Type::Dynamic,
        ..Default::default()
    };
    let mut fb = FunctionBuilder::new("bool_ret", sig, Visibility::Public);
    let cond = fb.param(0);
    let then_b = fb.create_block();
    let else_b = fb.create_block();
    fb.br_if(cond, then_b, &[], else_b, &[]);

    fb.switch_to_block(then_b);
    let one = fb.const_int(1);
    fb.ret(Some(one));

    fb.switch_to_block(else_b);
    let zero = fb.const_int(0);
    fb.ret(Some(zero));

    fb.build()
}

/// Build a deeply nested diamond: if { if { if { ... } } }.
fn build_nested_diamond(depth: usize) -> crate::ir::Function {
    let sig = FunctionSig {
        params: vec![Type::Bool],
        return_ty: Type::Int(64),
        ..Default::default()
    };
    let mut fb = FunctionBuilder::new("nested", sig, Visibility::Private);
    let cond = fb.param(0);

    // Build a chain of nested if-else diamonds.
    let mut merge_blocks = Vec::new();

    for _ in 0..depth {
        let then_b = fb.create_block();
        let else_b = fb.create_block();
        let merge = fb.create_block();
        fb.br_if(cond, then_b, &[], else_b, &[]);

        fb.switch_to_block(then_b);
        fb.br(merge, &[]);

        // else block becomes the new "current" for next nesting.
        fb.switch_to_block(else_b);
        fb.br(merge, &[]);

        merge_blocks.push(merge);
        fb.switch_to_block(merge);
    }

    let val = fb.const_int(42);
    fb.ret(Some(val));
    fb.build()
}

/// Apply a pass to a function, check well-formedness, and verify idempotency.
fn stress_pass<T: Transform>(pass: &T, func: crate::ir::Function) {
    let mut mb = ModuleBuilder::new("test");
    mb.add_function(func);
    let module = mb.build();

    // First application.
    let r1 = pass.apply(module).unwrap();
    let func1 = &r1.module.functions[FuncId::new(0)];
    assert_well_formed(func1);

    // Compact to remove dead arena entries before second run.
    let mut module2 = r1.module;
    for func in module2.functions.values_mut() {
        func.compact_insts();
    }

    // Second application — should be idempotent.
    let r2 = pass.apply(module2).unwrap();
    assert!(
        !r2.changed,
        "{} not idempotent on stress input",
        pass.name()
    );
    assert_well_formed(&r2.module.functions[FuncId::new(0)]);
}

/// Apply a pass to a module with two functions (needed for ConstraintSolve).
fn stress_pass_module<T: Transform>(pass: &T, func1: crate::ir::Function, func2: crate::ir::Function) {
    let mut mb = ModuleBuilder::new("test");
    mb.add_function(func1);
    mb.add_function(func2);
    let module = mb.build();

    let r1 = pass.apply(module).unwrap();
    for func in r1.module.functions.values() {
        assert_well_formed(func);
    }

    let mut module2 = r1.module;
    for func in module2.functions.values_mut() {
        func.compact_insts();
    }

    let r2 = pass.apply(module2).unwrap();
    assert!(
        !r2.changed,
        "{} not idempotent on stress input",
        pass.name()
    );
}

// ---- ConstantFolding stress tests ----

#[test]
fn stress_const_fold_linear_chains() {
    let pass = ConstantFolding;
    for n in [1, 5, 10, 20] {
        for ty in TYPES {
            stress_pass(&pass, build_linear_chain(n, ty.clone()));
        }
    }
}

#[test]
fn stress_const_fold_diamonds() {
    let pass = ConstantFolding;
    for ty in TYPES {
        stress_pass(&pass, build_diamond(ty.clone()));
    }
}

#[test]
fn stress_const_fold_nested() {
    let pass = ConstantFolding;
    for depth in [1, 3, 5, 10] {
        stress_pass(&pass, build_nested_diamond(depth));
    }
}

// ---- CfgSimplify stress tests ----

#[test]
fn stress_cfg_simplify_linear_chains() {
    let pass = CfgSimplify;
    for n in [1, 5, 10, 20] {
        for ty in TYPES {
            stress_pass(&pass, build_linear_chain(n, ty.clone()));
        }
    }
}

#[test]
fn stress_cfg_simplify_nested() {
    let pass = CfgSimplify;
    for depth in [1, 3, 5, 10] {
        stress_pass(&pass, build_nested_diamond(depth));
    }
}

// ---- DCE stress tests ----

#[test]
fn stress_dce_linear_chains() {
    let pass = DeadCodeElimination;
    for n in [1, 5, 10, 20] {
        for ty in TYPES {
            stress_pass(&pass, build_linear_chain(n, ty.clone()));
        }
    }
}

#[test]
fn stress_dce_loops() {
    let pass = DeadCodeElimination;
    for ty in TYPES {
        stress_pass(&pass, build_loop(ty.clone()));
    }
}

// ---- Mem2Reg stress tests ----

#[test]
fn stress_mem2reg_alloc_patterns() {
    let pass = Mem2Reg;
    for ty in TYPES {
        stress_pass(&pass, build_alloc_pattern(ty.clone()));
    }
}

// ---- TypeInference stress tests ----

#[test]
fn stress_type_infer_varied_shapes() {
    let pass = TypeInference;
    for ty in TYPES {
        stress_pass(&pass, build_linear_chain(3, ty.clone()));
        stress_pass(&pass, build_diamond(ty.clone()));
        stress_pass(&pass, build_loop(ty.clone()));
        stress_pass(&pass, build_cast_chain(ty.clone()));
    }
}

// ---- ConstraintSolve stress tests ----

#[test]
fn stress_constraint_solve_varied() {
    let pass = ConstraintSolve;
    for ty in TYPES {
        stress_pass_module(
            &pass,
            build_linear_chain(3, ty.clone()),
            build_diamond(ty.clone()),
        );
    }
}

// ---- RedundantCastElim stress tests ----

#[test]
fn stress_red_cast_elim_cast_chains() {
    let pass = RedundantCastElimination;
    for ty in TYPES {
        stress_pass(&pass, build_cast_chain(ty.clone()));
    }
}

// ---- BoolLiteralReturn stress tests ----

#[test]
fn stress_bool_literal_return() {
    let pass = BoolLiteralReturn;
    stress_pass(&pass, build_bool_return());
    // Non-bool functions should be untouched.
    for ty in TYPES {
        stress_pass(&pass, build_linear_chain(3, ty.clone()));
    }
}

// ---- Full pipeline stress ----

#[test]
fn stress_full_pipeline_varied_shapes() {
    use crate::pipeline::PassConfig;
    use crate::transforms::default_pipeline;

    let config = PassConfig::default();
    let shapes: Vec<crate::ir::Function> = vec![
        build_linear_chain(1, Type::Int(64)),
        build_linear_chain(10, Type::Dynamic),
        build_diamond(Type::Int(64)),
        build_diamond(Type::Float(64)),
        build_loop(Type::Int(64)),
        build_loop(Type::Dynamic),
        build_alloc_pattern(Type::Int(64)),
        build_cast_chain(Type::Bool),
        build_bool_return(),
        build_nested_diamond(5),
    ];

    for func in shapes {
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let pipeline = default_pipeline(&config);
        let result = pipeline.run(module).unwrap();
        let func = &result.functions[FuncId::new(0)];
        assert_well_formed(func);
    }
}
