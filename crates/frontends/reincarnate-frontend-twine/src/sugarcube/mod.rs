//! SugarCube story format parser and IR lowering.
//!
//! SugarCube (v2.x) uses a macro DSL with `<<macro>>` syntax, TwineScript
//! expressions (a JS superset with `is`, `isnot`, `to`, `not` keywords),
//! and `[[link|passage]]` shorthand navigation.

pub mod ast;
pub mod custom_macros;
pub mod macros;
pub mod parser;
pub mod preprocess;
pub mod translate;

use ast::PassageAst;
pub use custom_macros::{extract_custom_macros, CustomMacroDef, CustomMacroRegistry};

/// Parse a SugarCube passage source string into an AST.
///
/// Errors are accumulated in `PassageAst.errors` rather than aborting —
/// a single broken passage must not prevent parsing the other 9,999.
pub fn parse_passage(source: &str) -> PassageAst {
    parser::parse(source, None)
}

/// Parse a SugarCube passage with a custom macro registry.
///
/// Custom macros extracted from `Macro.add()` calls in user scripts are
/// used to correctly classify block vs self-closing macros and handle
/// `skipArgs` semantics.
pub fn parse_passage_with_registry(
    source: &str,
    registry: &CustomMacroRegistry,
) -> PassageAst {
    parser::parse(source, Some(registry))
}
