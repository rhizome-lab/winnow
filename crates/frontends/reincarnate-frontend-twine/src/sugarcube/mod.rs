//! SugarCube story format parser and IR lowering.
//!
//! SugarCube (v2.x) uses a macro DSL with `<<macro>>` syntax, TwineScript
//! expressions (a JS superset with `is`, `isnot`, `to`, `not` keywords),
//! and `[[link|passage]]` shorthand navigation.

pub mod ast;
pub mod expr;
pub mod lexer;
pub mod macros;
pub mod parser;

use ast::PassageAst;

/// Parse a SugarCube passage source string into an AST.
///
/// Errors are accumulated in `PassageAst.errors` rather than aborting —
/// a single broken passage must not prevent parsing the other 9,999.
pub fn parse_passage(source: &str) -> PassageAst {
    parser::parse(source)
}
