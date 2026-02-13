use crate::cursor::Cursor;
use crate::error::{Error, Result};

/// Magic bytes for the FORM container.
const FORM_MAGIC: [u8; 4] = *b"FORM";

/// A single chunk entry in the file.
#[derive(Debug, Clone, Copy)]
pub struct ChunkEntry {
    /// 4-byte ASCII magic identifying the chunk type.
    pub magic: [u8; 4],
    /// Absolute byte offset of the chunk header (magic field) in the file.
    pub offset: usize,
    /// Size of the chunk's content (excluding the 8-byte header).
    pub size: usize,
}

impl ChunkEntry {
    /// Absolute offset where chunk content begins (after magic + size fields).
    pub fn data_offset(&self) -> usize {
        self.offset + 8
    }

    /// Magic as a string (for display).
    pub fn magic_str(&self) -> &str {
        std::str::from_utf8(&self.magic).unwrap_or("????")
    }
}

/// Index of all top-level chunks in a FORM file.
///
/// This is Layer 1: it only knows about the FORM envelope and chunk boundaries.
/// It does not parse any chunk internals.
pub struct ChunkIndex {
    /// Ordered list of chunks as they appear in the file.
    chunks: Vec<ChunkEntry>,
}

impl ChunkIndex {
    /// Parse the FORM envelope and build a chunk index.
    ///
    /// The `data` slice must be the entire file contents.
    pub fn parse(data: &[u8]) -> Result<Self> {
        let mut cursor = Cursor::new(data);

        // Read FORM header
        let magic = cursor.read_magic()?;
        if magic != FORM_MAGIC {
            return Err(Error::InvalidMagic {
                expected: FORM_MAGIC,
                found: magic,
            });
        }
        let form_size = cursor.read_u32()? as usize;
        let form_end = 8 + form_size;

        // Iterate over chunks
        let mut chunks = Vec::new();
        while cursor.position() < form_end {
            let chunk_offset = cursor.position();

            let chunk_magic = cursor.read_magic()?;
            // Validate chunk magic is ASCII
            if !chunk_magic.iter().all(|&b| b.is_ascii_alphanumeric()) {
                return Err(Error::InvalidChunkMagic {
                    offset: chunk_offset,
                    magic: chunk_magic,
                });
            }

            let chunk_size = cursor.read_u32()? as usize;
            chunks.push(ChunkEntry {
                magic: chunk_magic,
                offset: chunk_offset,
                size: chunk_size,
            });

            // Skip to next chunk
            cursor.seek(chunk_offset + 8 + chunk_size);
        }

        Ok(Self { chunks })
    }

    /// All chunks in file order.
    pub fn chunks(&self) -> &[ChunkEntry] {
        &self.chunks
    }

    /// Number of chunks.
    pub fn len(&self) -> usize {
        self.chunks.len()
    }

    /// Whether the index is empty.
    pub fn is_empty(&self) -> bool {
        self.chunks.is_empty()
    }

    /// Find a chunk by its 4-byte magic. Returns the first match.
    pub fn find(&self, magic: &[u8; 4]) -> Option<&ChunkEntry> {
        self.chunks.iter().find(|c| &c.magic == magic)
    }

    /// Get the raw content bytes for a chunk from the file data.
    pub fn chunk_data<'a>(&self, data: &'a [u8], magic: &[u8; 4]) -> Result<&'a [u8]> {
        let entry = self.find(magic).ok_or(Error::ChunkNotFound { magic: *magic })?;
        let start = entry.data_offset();
        let end = start + entry.size;
        if end > data.len() {
            return Err(Error::UnexpectedEof {
                offset: start,
                need: entry.size,
                have: data.len() - start,
            });
        }
        Ok(&data[start..end])
    }
}
