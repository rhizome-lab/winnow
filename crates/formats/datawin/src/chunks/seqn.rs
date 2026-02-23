use crate::cursor::Cursor;
use crate::error::Result;
use crate::string_table::StringRef;

/// A sequence entry in the SEQN chunk.
#[derive(Debug)]
pub struct SeqnEntry {
    /// Reference to the sequence name string (e.g. "sqIntro").
    pub name: StringRef,
}

/// Parsed SEQN chunk (names only; full sequence data not parsed).
///
/// The GMS2.3+ SEQN chunk format differs from other pointer-list chunks:
/// it has a 4-byte version field before the count.
#[derive(Debug)]
pub struct Seqn {
    pub sequences: Vec<SeqnEntry>,
}

impl Seqn {
    /// Parse the SEQN chunk (name-only).
    pub fn parse(chunk_data: &[u8], data: &[u8]) -> Result<Self> {
        let mut c = Cursor::new(chunk_data);
        // GMS2.3+ SEQN: version u32 precedes the standard count+pointer list.
        let _version = c.read_u32()?;
        let pointers = c.read_pointer_list()?;

        let mut sequences = Vec::with_capacity(pointers.len());
        for ptr in pointers {
            let mut ec = Cursor::new(data);
            ec.seek(ptr as usize);
            let name = StringRef(ec.read_u32()?);
            sequences.push(SeqnEntry { name });
        }

        Ok(Self { sequences })
    }
}
