use crate::error::CoreError;
use crate::ir::Module;

/// Transform trait — a pass that transforms IR modules.
///
/// Examples: type inference, coroutine lowering, dead code elimination,
/// constant folding, inlining.
pub trait Transform {
    /// Name of this transform pass.
    fn name(&self) -> &str;

    /// Apply this transform to a module, returning the transformed module.
    fn apply(&self, module: Module) -> Result<Module, CoreError>;
}

/// An ordered sequence of transforms to apply.
pub struct TransformPipeline {
    transforms: Vec<Box<dyn Transform>>,
}

impl TransformPipeline {
    pub fn new() -> Self {
        Self {
            transforms: Vec::new(),
        }
    }

    pub fn add(&mut self, transform: Box<dyn Transform>) {
        self.transforms.push(transform);
    }

    /// Run all transforms in order on the given module.
    pub fn run(&self, mut module: Module) -> Result<Module, CoreError> {
        for transform in &self.transforms {
            module = transform.apply(module)?;
        }
        Ok(module)
    }
}

impl Default for TransformPipeline {
    fn default() -> Self {
        Self::new()
    }
}
