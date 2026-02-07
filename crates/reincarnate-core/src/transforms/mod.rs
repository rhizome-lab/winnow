pub mod cfg_simplify;
pub mod const_fold;
pub mod constraint_solve;
pub mod coroutine_lower;
pub mod dce;
pub mod mem2reg;
pub mod red_cast_elim;
pub mod type_infer;
pub mod util;

pub use cfg_simplify::CfgSimplify;
pub use const_fold::ConstantFolding;
pub use constraint_solve::ConstraintSolve;
pub use coroutine_lower::CoroutineLowering;
pub use dce::DeadCodeElimination;
pub use mem2reg::Mem2Reg;
pub use red_cast_elim::RedundantCastElimination;
pub use type_infer::TypeInference;

use crate::pipeline::{PassConfig, TransformPipeline};

/// Build a transform pipeline based on the given pass configuration.
pub fn default_pipeline(config: &PassConfig) -> TransformPipeline {
    let mut pipeline = TransformPipeline::new();
    if config.type_inference {
        pipeline.add(Box::new(TypeInference));
    }
    if config.constraint_solve {
        pipeline.add(Box::new(ConstraintSolve));
    }
    if config.constant_folding {
        pipeline.add(Box::new(ConstantFolding));
    }
    if config.cfg_simplify {
        pipeline.add(Box::new(CfgSimplify));
    }
    if config.coroutine_lowering {
        pipeline.add(Box::new(CoroutineLowering));
    }
    if config.redundant_cast_elimination {
        pipeline.add(Box::new(RedundantCastElimination));
    }
    if config.mem2reg {
        pipeline.add(Box::new(Mem2Reg));
    }
    if config.dead_code_elimination {
        pipeline.add(Box::new(DeadCodeElimination));
    }
    pipeline.set_fixpoint(config.fixpoint);
    pipeline
}
