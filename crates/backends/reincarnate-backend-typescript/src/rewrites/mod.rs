//! Engine-specific rewrite modules.
//!
//! Each module converts engine-specific `SystemCall` nodes into native JS
//! constructs during the `lower` pass. The rewrites are scoped: the lowering
//! pass only activates Flash rewrites when a Flash runtime is present.

pub mod flash;
pub mod gamemaker;
pub mod twine;
