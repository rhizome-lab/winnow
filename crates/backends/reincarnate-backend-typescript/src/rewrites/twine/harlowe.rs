//! Harlowe-specific JsExpr → JsExpr rewrites.
//!
//! `Harlowe.H.*` SystemCalls are lowered to `h.method(args...)` MethodCall
//! nodes by the core `lower_output_nodes` pass before reaching the backend.
//! No further rewrites are needed here.

use crate::js_ast::JsExpr;

/// Returns the bare function names that a `Harlowe.H` rewrite will
/// introduce, if any. Since all Harlowe.H methods are method calls on the
/// `h` parameter (not imports), this always returns empty.
pub(super) fn rewrite_introduced_calls(_method: &str) -> &'static [&'static str] {
    &[]
}

/// Try to rewrite a `Harlowe.H.*` SystemCall. Returns None — all rewrites
/// are handled by the core `lower_output_nodes` pass.
pub(super) fn try_rewrite(_method: &str, _args: &mut Vec<JsExpr>) -> Option<JsExpr> {
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn introduced_calls_always_empty() {
        assert!(rewrite_introduced_calls("text").is_empty());
        assert!(rewrite_introduced_calls("br").is_empty());
        assert!(rewrite_introduced_calls("em").is_empty());
        assert!(rewrite_introduced_calls("link").is_empty());
        assert!(rewrite_introduced_calls("color").is_empty());
        assert!(rewrite_introduced_calls("styled").is_empty());
    }
}
