use crate::error::CoreError;
use crate::ir::Module;

/// Result of applying a transform pass.
pub struct TransformResult {
    pub module: Module,
    /// Whether the pass modified the module.
    pub changed: bool,
}

/// Transform trait — a pass that transforms IR modules.
///
/// Examples: type inference, coroutine lowering, dead code elimination,
/// constant folding, inlining.
pub trait Transform {
    /// Name of this transform pass.
    fn name(&self) -> &str;

    /// Apply this transform to a module, returning the transformed module
    /// and whether any changes were made.
    fn apply(&self, module: Module) -> Result<TransformResult, CoreError>;
}

/// Maximum number of fixpoint iterations before giving up.
const MAX_FIXPOINT_ITERATIONS: usize = 100;

/// An ordered sequence of transforms to apply.
pub struct TransformPipeline {
    transforms: Vec<Box<dyn Transform>>,
    fixpoint: bool,
}

impl TransformPipeline {
    pub fn new() -> Self {
        Self {
            transforms: Vec::new(),
            fixpoint: false,
        }
    }

    pub fn add(&mut self, transform: Box<dyn Transform>) {
        self.transforms.push(transform);
    }

    /// Enable fixpoint iteration: re-run the entire pipeline until no pass
    /// reports changes, or until the iteration cap is reached.
    pub fn set_fixpoint(&mut self, enabled: bool) {
        self.fixpoint = enabled;
    }

    /// Run all transforms in order on the given module.
    ///
    /// When fixpoint mode is enabled, the pipeline repeats until a full
    /// pass over all transforms produces no changes.
    pub fn run(&self, mut module: Module) -> Result<Module, CoreError> {
        if self.fixpoint {
            for _ in 0..MAX_FIXPOINT_ITERATIONS {
                let mut any_changed = false;
                for transform in &self.transforms {
                    let result = transform.apply(module)?;
                    any_changed |= result.changed;
                    module = result.module;
                }
                if !any_changed {
                    break;
                }
            }
        } else {
            for transform in &self.transforms {
                module = transform.apply(module)?.module;
            }
        }
        Ok(module)
    }
}

impl Default for TransformPipeline {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// A mock transform that reports `changed` for its first N calls, then stops.
    struct MockTransform {
        name: &'static str,
        changes_left: AtomicUsize,
    }

    impl MockTransform {
        fn new(name: &'static str, num_changes: usize) -> Self {
            Self {
                name,
                changes_left: AtomicUsize::new(num_changes),
            }
        }
    }

    impl Transform for MockTransform {
        fn name(&self) -> &str {
            self.name
        }

        fn apply(&self, module: Module) -> Result<TransformResult, CoreError> {
            let prev = self.changes_left.fetch_update(
                Ordering::SeqCst,
                Ordering::SeqCst,
                |n| if n > 0 { Some(n - 1) } else { None },
            );
            Ok(TransformResult {
                module,
                changed: prev.is_ok(),
            })
        }
    }

    #[test]
    fn single_pass_no_fixpoint() {
        let module = Module::new("test".into());
        let mut pipeline = TransformPipeline::new();
        pipeline.add(Box::new(MockTransform::new("a", 5)));
        // Without fixpoint, the transform runs exactly once.
        let _result = pipeline.run(module).unwrap();
        let mock = pipeline.transforms[0]
            .as_ref() as *const dyn Transform as *const MockTransform;
        // Safety: we know the concrete type.
        let remaining = unsafe { (*mock).changes_left.load(Ordering::SeqCst) };
        assert_eq!(remaining, 4); // ran once, decremented from 5 to 4
    }

    #[test]
    fn fixpoint_runs_until_stable() {
        let module = Module::new("test".into());
        let mut pipeline = TransformPipeline::new();
        pipeline.add(Box::new(MockTransform::new("a", 3)));
        pipeline.set_fixpoint(true);
        let _result = pipeline.run(module).unwrap();
        let mock = pipeline.transforms[0]
            .as_ref() as *const dyn Transform as *const MockTransform;
        // After 3 changes + 1 stable iteration = 4 calls total. changes_left = 0.
        let remaining = unsafe { (*mock).changes_left.load(Ordering::SeqCst) };
        assert_eq!(remaining, 0);
    }

    #[test]
    fn fixpoint_with_multiple_passes() {
        let module = Module::new("test".into());
        let mut pipeline = TransformPipeline::new();
        // Pass A changes twice, pass B changes once.
        // Iteration 1: A changes (2→1), B changes (1→0) → any_changed=true
        // Iteration 2: A changes (1→0), B stable → any_changed=true
        // Iteration 3: A stable, B stable → done
        pipeline.add(Box::new(MockTransform::new("a", 2)));
        pipeline.add(Box::new(MockTransform::new("b", 1)));
        pipeline.set_fixpoint(true);
        let _result = pipeline.run(module).unwrap();

        let mock_a = pipeline.transforms[0]
            .as_ref() as *const dyn Transform as *const MockTransform;
        let mock_b = pipeline.transforms[1]
            .as_ref() as *const dyn Transform as *const MockTransform;
        let remaining_a = unsafe { (*mock_a).changes_left.load(Ordering::SeqCst) };
        let remaining_b = unsafe { (*mock_b).changes_left.load(Ordering::SeqCst) };
        assert_eq!(remaining_a, 0);
        assert_eq!(remaining_b, 0);
    }
}
