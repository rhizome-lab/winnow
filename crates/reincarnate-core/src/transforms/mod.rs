pub mod cfg_simplify;
pub mod const_fold;
pub mod dce;
pub mod type_infer;

pub use cfg_simplify::CfgSimplify;
pub use const_fold::ConstantFolding;
pub use dce::DeadCodeElimination;
pub use type_infer::TypeInference;

use crate::pipeline::{PassConfig, TransformPipeline};

/// Build a transform pipeline based on the given pass configuration.
pub fn default_pipeline(config: &PassConfig) -> TransformPipeline {
    let mut pipeline = TransformPipeline::new();
    if config.type_inference {
        pipeline.add(Box::new(TypeInference));
    }
    if config.constant_folding {
        pipeline.add(Box::new(ConstantFolding));
    }
    if config.cfg_simplify {
        pipeline.add(Box::new(CfgSimplify));
    }
    if config.dead_code_elimination {
        pipeline.add(Box::new(DeadCodeElimination));
    }
    pipeline.set_fixpoint(config.fixpoint);
    pipeline
}
