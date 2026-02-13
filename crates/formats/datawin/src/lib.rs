//! Reader/writer for GameMaker's compiled data.win format.
//!
//! Three-layer architecture:
//! - **Layer 1** (`reader`/`writer`): Raw chunk I/O — FORM envelope, chunk index
//! - **Layer 2** (`chunks`): Typed parsers for individual chunk formats
//! - **Layer 3** (`datawin`): High-level lazy wrapper with cached accessors

pub mod bytecode;
pub mod chunks;
pub mod cursor;
pub mod datawin;
pub mod error;
pub mod reader;
pub mod string_table;
pub mod version;
pub mod writer;

pub use error::{Error, Result};
pub use reader::ChunkIndex;
pub use version::BytecodeVersion;
