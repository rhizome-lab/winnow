/// Configuration for which transform passes to run.
///
/// All passes are enabled by default. Disable individual passes by setting
/// their fields to `false`, or use `from_skip_list` with pass name strings.
#[derive(Debug, Clone)]
pub struct PassConfig {
    pub type_inference: bool,
    pub constant_folding: bool,
    pub cfg_simplify: bool,
    pub dead_code_elimination: bool,
    /// When enabled, the pipeline repeats all passes until none report changes.
    pub fixpoint: bool,
}

impl Default for PassConfig {
    fn default() -> Self {
        Self {
            type_inference: true,
            constant_folding: true,
            cfg_simplify: true,
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
    /// - `"constant-folding"`
    /// - `"cfg-simplify"`
    /// - `"dead-code-elimination"`
    /// - `"fixpoint"` — toggles pipeline fixpoint iteration
    pub fn from_skip_list(skip: &[&str]) -> Self {
        let mut config = Self::default();
        for name in skip {
            match *name {
                "type-inference" => config.type_inference = false,
                "constant-folding" => config.constant_folding = false,
                "cfg-simplify" => config.cfg_simplify = false,
                "dead-code-elimination" => config.dead_code_elimination = false,
                "fixpoint" => config.fixpoint = false,
                _ => {}
            }
        }
        config
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_enables_all() {
        let config = PassConfig::default();
        assert!(config.type_inference);
        assert!(config.constant_folding);
        assert!(config.cfg_simplify);
        assert!(config.dead_code_elimination);
        assert!(!config.fixpoint);
    }

    #[test]
    fn skip_list_disables_passes() {
        let config = PassConfig::from_skip_list(&["constant-folding"]);
        assert!(config.type_inference);
        assert!(!config.constant_folding);
        assert!(config.cfg_simplify);
        assert!(config.dead_code_elimination);
    }

    #[test]
    fn skip_list_all() {
        let config = PassConfig::from_skip_list(&[
            "type-inference",
            "constant-folding",
            "cfg-simplify",
            "dead-code-elimination",
            "fixpoint",
        ]);
        assert!(!config.type_inference);
        assert!(!config.constant_folding);
        assert!(!config.cfg_simplify);
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
}
