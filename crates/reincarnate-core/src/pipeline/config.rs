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
    /// "Literal" preset — faithful 1:1 translation. Ternaries, logical
    /// short-circuit operators, and while-condition hoisting are kept because
    /// they accurately represent what the bytecode does. Only pattern-matching
    /// optimizations that introduce constructs not in the original bytecode
    /// (e.g. Math.max/min) are disabled.
    pub fn literal() -> Self {
        Self {
            ternary: true,
            minmax: false,
            logical_operators: true,
            while_condition_hoisting: true,
        }
    }

    /// "Optimized" preset — all pattern-matching optimizations enabled.
    pub fn optimized() -> Self {
        Self {
            ternary: true,
            minmax: true,
            logical_operators: true,
            while_condition_hoisting: true,
        }
    }

    /// Create from a preset name. Returns `None` for unknown presets.
    pub fn from_preset(name: &str) -> Option<Self> {
        match name {
            "literal" => Some(Self::literal()),
            "optimized" => Some(Self::optimized()),
            _ => None,
        }
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
    fn lowering_optimized_preset() {
        let config = LoweringConfig::optimized();
        assert!(config.ternary);
        assert!(config.minmax);
        assert!(config.logical_operators);
        assert!(config.while_condition_hoisting);
    }

    #[test]
    fn lowering_literal_preset() {
        let config = LoweringConfig::literal();
        assert!(config.ternary);
        assert!(!config.minmax);
        assert!(config.logical_operators);
        assert!(config.while_condition_hoisting);
    }

    #[test]
    fn lowering_default_is_optimized() {
        let config = LoweringConfig::default();
        assert!(config.ternary);
        assert!(config.minmax);
    }

    #[test]
    fn lowering_from_preset() {
        assert!(LoweringConfig::from_preset("literal").is_some());
        assert!(LoweringConfig::from_preset("optimized").is_some());
        assert!(LoweringConfig::from_preset("unknown").is_none());
    }
}
