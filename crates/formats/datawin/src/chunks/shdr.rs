use crate::cursor::Cursor;
use crate::error::Result;
use crate::string_table::StringRef;

/// A shader entry in the SHDR chunk.
#[derive(Debug)]
pub struct ShdrEntry {
    /// Reference to the shader name string (e.g. "shd_Edge").
    pub name: StringRef,
}

/// Parsed SHDR chunk (names only; shader source data not parsed).
#[derive(Debug)]
pub struct Shdr {
    pub shaders: Vec<ShdrEntry>,
}

impl Shdr {
    /// Parse the SHDR chunk (name-only).
    pub fn parse(chunk_data: &[u8], data: &[u8]) -> Result<Self> {
        let mut c = Cursor::new(chunk_data);
        let pointers = c.read_pointer_list()?;

        let mut shaders = Vec::with_capacity(pointers.len());
        for ptr in pointers {
            let mut ec = Cursor::new(data);
            ec.seek(ptr as usize);
            let name = StringRef(ec.read_u32()?);
            shaders.push(ShdrEntry { name });
        }

        Ok(Self { shaders })
    }
}
