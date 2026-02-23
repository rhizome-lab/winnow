use crate::cursor::Cursor;
use crate::error::Result;
use crate::string_table::StringRef;

/// A background/tileset entry in the BGND chunk.
#[derive(Debug)]
pub struct BgndEntry {
    /// Reference to the background name string (e.g. "bg_Sky", "tile_WoodFloor").
    pub name: StringRef,
}

/// Parsed BGND chunk (names only; full tileset data not parsed).
#[derive(Debug)]
pub struct Bgnd {
    pub backgrounds: Vec<BgndEntry>,
}

impl Bgnd {
    /// Parse the BGND chunk (name-only).
    pub fn parse(chunk_data: &[u8], data: &[u8]) -> Result<Self> {
        let mut c = Cursor::new(chunk_data);
        let pointers = c.read_pointer_list()?;

        let mut backgrounds = Vec::with_capacity(pointers.len());
        for ptr in pointers {
            let mut ec = Cursor::new(data);
            ec.seek(ptr as usize);
            let name = StringRef(ec.read_u32()?);
            backgrounds.push(BgndEntry { name });
        }

        Ok(Self { backgrounds })
    }
}
