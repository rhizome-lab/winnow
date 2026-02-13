use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("invalid magic: expected {expected:?}, found {found:?}")]
    InvalidMagic { expected: [u8; 4], found: [u8; 4] },

    #[error("unexpected end of data at offset {offset:#x} (need {need} bytes, have {have})")]
    UnexpectedEof {
        offset: usize,
        need: usize,
        have: usize,
    },

    #[error("invalid chunk magic at offset {offset:#x}: {magic:?}")]
    InvalidChunkMagic { offset: usize, magic: [u8; 4] },

    #[error("chunk {magic:?} not found")]
    ChunkNotFound { magic: [u8; 4] },

    #[error("string at offset {offset:#x} is not valid UTF-8: {source}")]
    InvalidString {
        offset: usize,
        source: std::string::FromUtf8Error,
    },

    #[error("invalid string offset {offset:#x}: outside STRG chunk")]
    InvalidStringOffset { offset: usize },

    #[error("unsupported bytecode version {version}")]
    UnsupportedVersion { version: u8 },

    #[error("{context}: {message}")]
    Parse { context: &'static str, message: String },
}

pub type Result<T> = std::result::Result<T, Error>;
