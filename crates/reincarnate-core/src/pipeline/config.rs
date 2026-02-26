/// Configuration for debug dumps during the pipeline.
///
/// When enabled, dumps IR and/or AST to stderr at key points. An optional
/// function filter restricts output to functions whose name contains the
/// given substring.
#[derive(Debug, Clone, Default)]
pub struct DebugConfig {
    /// Dump post-transform IR to stderr before structurization.
    pub dump_ir: bool,
    /// Dump raw AST to stderr before AST-to-AST passes.
    pub dump_ast: bool,
    /// Filter dumps to functions whose name contains this substring.
    pub function_filter: Option<String>,
}

impl DebugConfig {
    /// A config with all dumps disabled.
    pub fn none() -> Self {
        Self::default()
    }

    /// Returns `true` if any dump is enabled and the function name matches
    /// the filter (or no filter is set).
    pub fn should_dump(&self, func_name: &str) -> bool {
        self.function_filter
            .as_ref()
            .is_none_or(|f| func_name.contains(f.as_str()))
    }
}

/// Configuration for which transform passes to run.
///
/// All passes are enabled by default. Disable individual passes by setting
/// their fields to `false`, or use `from_skip_list` with pass name strings.
#[derive(Debug, Clone)]
pub struct PassConfig {
    pub type_inference: bool,
    pub constraint_solve: bool,
    pub constant_folding: bool,
    pub cfg_simplify: bool,
    pub coroutine_lowering: bool,
    pub redundant_cast_elimination: bool,
    pub int_to_bool_promotion: bool,
    pub mem2reg: bool,
    pub dead_code_elimination: bool,
    /// When enabled, the pipeline repeats all passes until none report changes.
    pub fixpoint: bool,
}

impl Default for PassConfig {
    fn default() -> Self {
        Self {
            type_inference: true,
            constraint_solve: true,
            constant_folding: true,
            cfg_simplify: true,
            coroutine_lowering: true,
            redundant_cast_elimination: true,
            int_to_bool_promotion: true,
            mem2reg: true,
            dead_code_elimination: true,
            fixpoint: false,
        }
    }
}

impl PassConfig {
    /// Create a config with all passes enabled except those in the skip list.
    ///
    /// Pass names correspond to `Transform::name()` values:
    /// - `"type-inference"`
    /// - `"constraint-solve"`
    /// - `"constant-folding"`
    /// - `"cfg-simplify"`
    /// - `"coroutine-lowering"`
    /// - `"redundant-cast-elimination"`
    /// - `"int-to-bool-promotion"`
    /// - `"mem2reg"`
    /// - `"dead-code-elimination"`
    /// - `"fixpoint"` — toggles pipeline fixpoint iteration
    pub fn from_skip_list(skip: &[&str]) -> Self {
        let mut config = Self::default();
        for name in skip {
            match *name {
                "type-inference" => config.type_inference = false,
                "constraint-solve" => config.constraint_solve = false,
                "constant-folding" => config.constant_folding = false,
                "cfg-simplify" => config.cfg_simplify = false,
                "coroutine-lowering" => config.coroutine_lowering = false,
                "redundant-cast-elimination" => config.redundant_cast_elimination = false,
                "int-to-bool-promotion" => config.int_to_bool_promotion = false,
                "mem2reg" => config.mem2reg = false,
                "dead-code-elimination" => config.dead_code_elimination = false,
                "fixpoint" => config.fixpoint = false,
                _ => {}
            }
        }
        config
    }
}

/// Configuration for AST lowering optimizations.
///
/// Controls which pattern-matching optimizations are applied when converting
/// structured IR to the high-level AST. Expression inlining and constant
/// propagation are always enabled — these flags control higher-level patterns.
#[derive(Debug, Clone)]
pub struct LoweringConfig {
    /// Convert single-assign if/else branches to ternary expressions.
    pub ternary: bool,
    /// Convert comparison + ternary patterns to `Math.max`/`Math.min`.
    pub minmax: bool,
    /// Convert LogicalOr/And shapes to `||`/`&&` short-circuit expressions.
    pub logical_operators: bool,
    /// Hoist loop conditions into `while (cond)` instead of
    /// `while (true) { if (!cond) break; ... }`.
    pub while_condition_hoisting: bool,
}

impl Default for LoweringConfig {
    /// Default is the optimized preset (all optimizations enabled).
    fn default() -> Self {
        Self::optimized()
    }
}

impl LoweringConfig {
    fn literal() -> Self {
        Self {
            ternary: true,
            minmax: false,
            logical_operators: true,
            while_condition_hoisting: true,
        }
    }

    fn optimized() -> Self {
        Self {
            ternary: true,
            minmax: true,
            logical_operators: true,
            while_condition_hoisting: true,
        }
    }
}

/// A named preset that configures the entire pipeline: both transform passes
/// and AST lowering optimizations.
///
/// - **`literal`**: Faithful 1:1 translation. Skips optimization passes
///   (constant folding, DCE, cfg-simplify, redundant cast elimination) and
///   disables AST-level rewrites like Math.max/min detection. Structural
///   passes (type inference, mem2reg, coroutine lowering) still run because
///   they're needed for correct output.
///
/// - **`optimized`** (default): All transform passes and AST-level
///   optimizations enabled.
pub struct Preset;

impl Preset {
    /// Resolve a preset name into `(PassConfig, LoweringConfig)`.
    ///
    /// `skip_passes` are applied on top of the preset's base `PassConfig`,
    /// allowing fine-grained overrides.
    pub fn resolve(
        name: &str,
        skip_passes: &[&str],
    ) -> Option<(PassConfig, LoweringConfig)> {
        let (mut pass, lowering) = match name {
            "literal" => (
                PassConfig {
                    // Structural passes — needed for correct output.
                    type_inference: true,
                    constraint_solve: true,
                    coroutine_lowering: true,
                    mem2reg: true,
                    // Optimization passes — disabled for literal.
                    constant_folding: false,
                    cfg_simplify: false,
                    redundant_cast_elimination: true,
                    int_to_bool_promotion: true,
                    dead_code_elimination: false,
                    fixpoint: false,
                },
                LoweringConfig::literal(),
            ),
            "optimized" => (PassConfig::default(), LoweringConfig::optimized()),
            _ => return None,
        };

        // Apply --skip-pass overrides on top of the preset.
        for name in skip_passes {
            match *name {
                "type-inference" => pass.type_inference = false,
                "constraint-solve" => pass.constraint_solve = false,
                "constant-folding" => pass.constant_folding = false,
                "cfg-simplify" => pass.cfg_simplify = false,
                "coroutine-lowering" => pass.coroutine_lowering = false,
                "redundant-cast-elimination" => pass.redundant_cast_elimination = false,
                "int-to-bool-promotion" => pass.int_to_bool_promotion = false,
                "mem2reg" => pass.mem2reg = false,
                "dead-code-elimination" => pass.dead_code_elimination = false,
                "fixpoint" => pass.fixpoint = false,
                _ => {}
            }
        }

        Some((pass, lowering))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_enables_all() {
        let config = PassConfig::default();
        assert!(config.type_inference);
        assert!(config.constraint_solve);
        assert!(config.constant_folding);
        assert!(config.cfg_simplify);
        assert!(config.coroutine_lowering);
        assert!(config.dead_code_elimination);
        assert!(!config.fixpoint);
    }

    #[test]
    fn skip_list_disables_passes() {
        let config = PassConfig::from_skip_list(&["constant-folding"]);
        assert!(config.type_inference);
        assert!(!config.constant_folding);
        assert!(config.cfg_simplify);
        assert!(config.coroutine_lowering);
        assert!(config.dead_code_elimination);
    }

    #[test]
    fn skip_list_all() {
        let config = PassConfig::from_skip_list(&[
            "type-inference",
            "constraint-solve",
            "constant-folding",
            "cfg-simplify",
            "coroutine-lowering",
            "redundant-cast-elimination",
            "int-to-bool-promotion",
            "mem2reg",
            "dead-code-elimination",
            "fixpoint",
        ]);
        assert!(!config.type_inference);
        assert!(!config.constraint_solve);
        assert!(!config.constant_folding);
        assert!(!config.cfg_simplify);
        assert!(!config.coroutine_lowering);
        assert!(!config.redundant_cast_elimination);
        assert!(!config.int_to_bool_promotion);
        assert!(!config.mem2reg);
        assert!(!config.dead_code_elimination);
        assert!(!config.fixpoint);
    }

    #[test]
    fn skip_list_unknown_ignored() {
        let config = PassConfig::from_skip_list(&["nonexistent"]);
        assert!(config.type_inference);
        assert!(config.constant_folding);
        assert!(config.cfg_simplify);
    }

    #[test]
    fn preset_optimized() {
        let (pass, lowering) = Preset::resolve("optimized", &[]).unwrap();
        assert!(pass.constant_folding);
        assert!(pass.cfg_simplify);
        assert!(pass.dead_code_elimination);
        assert!(pass.redundant_cast_elimination);
        assert!(lowering.minmax);
        assert!(lowering.ternary);
    }

    #[test]
    fn preset_literal() {
        let (pass, lowering) = Preset::resolve("literal", &[]).unwrap();
        // Structural passes still on.
        assert!(pass.type_inference);
        assert!(pass.mem2reg);
        assert!(pass.coroutine_lowering);
        // Optimization passes off.
        assert!(!pass.constant_folding);
        assert!(!pass.cfg_simplify);
        assert!(!pass.dead_code_elimination);
        assert!(pass.redundant_cast_elimination);
        // Lowering: faithful patterns on, rewrites off.
        assert!(lowering.ternary);
        assert!(lowering.logical_operators);
        assert!(lowering.while_condition_hoisting);
        assert!(!lowering.minmax);
    }

    #[test]
    fn preset_with_skip_overrides() {
        let (pass, _) = Preset::resolve("optimized", &["mem2reg"]).unwrap();
        assert!(!pass.mem2reg);
        assert!(pass.constant_folding);
    }

    #[test]
    fn preset_unknown_returns_none() {
        assert!(Preset::resolve("unknown", &[]).is_none());
    }
}
