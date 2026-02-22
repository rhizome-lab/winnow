use crate::cursor::Cursor;
use crate::error::Result;

/// A texture page entry in the TXTR chunk.
#[derive(Debug)]
pub struct TextureEntry {
    /// Raw texture data (typically PNG).
    /// Stored as a byte slice reference offset + length into the file data.
    pub data_offset: u32,
    /// Extra fields present in GMS2 (v17+).
    pub gms2_fields: Option<Gms2TextureFields>,
}

/// Additional texture fields in GMS2 format.
#[derive(Debug)]
pub struct Gms2TextureFields {
    /// Scaled flag or format.
    pub scaled: u32,
    /// Texture generation count/version.
    pub generated: u32,
}

/// Parsed TXTR chunk.
#[derive(Debug)]
pub struct Txtr {
    /// Texture page entries.
    pub textures: Vec<TextureEntry>,
}

impl Txtr {
    /// Parse the TXTR chunk.
    ///
    /// `chunk_data` is the raw chunk content (after the 8-byte header).
    /// `data` is the full file data (for following absolute pointers).
    pub fn parse(chunk_data: &[u8], data: &[u8]) -> Result<Self> {
        let mut c = Cursor::new(chunk_data);
        let pointers = c.read_pointer_list()?;

        // Determine entry size from pointer spacing (if 2+ textures)
        let is_gms2 = if pointers.len() >= 2 {
            (pointers[1] - pointers[0]) > 12
        } else {
            // Check if there are extra fields by trying to read more
            false
        };

        let mut textures = Vec::with_capacity(pointers.len());
        for ptr in &pointers {
            let mut ec = Cursor::new(data);
            ec.seek(*ptr as usize);

            if is_gms2 {
                let _unknown0 = ec.read_u32()?;
                let _unknown1 = ec.read_u32()?;
                let scaled = ec.read_u32()?;
                let generated = ec.read_u32()?;
                let _unknown2 = ec.read_u32()?;
                // Width and height may be here in some versions
                let _width_or_zero = ec.read_u32()?;
                let data_offset = ec.read_u32()?;

                textures.push(TextureEntry {
                    data_offset,
                    gms2_fields: Some(Gms2TextureFields { scaled, generated }),
                });
            } else {
                let _unknown = ec.read_u32()?;
                let data_offset = ec.read_u32()?;

                textures.push(TextureEntry {
                    data_offset,
                    gms2_fields: None,
                });
            }
        }

        Ok(Self { textures })
    }

    /// Extract the raw texture data (typically PNG) for a texture entry.
    ///
    /// Reads from `data_offset` until the start of the next texture or end of data.
    /// The caller should check the PNG magic (`\x89PNG`) or other format header.
    pub fn texture_data<'a>(
        &self,
        index: usize,
        data: &'a [u8],
    ) -> Option<&'a [u8]> {
        let entry = self.textures.get(index)?;
        let start = entry.data_offset as usize;
        if start >= data.len() {
            return None;
        }

        // Find end: next texture's data_offset, or end of file.
        // In GMS2.3+ games with external textures, data_offset points into an
        // external .png file rather than into the data.win blob. The next entry's
        // data_offset may be 0 or smaller than start, making the slice invalid.
        // Return None in that case — the caller will treat the texture as absent.
        let end = self
            .textures
            .get(index + 1)
            .map(|next| next.data_offset as usize)
            .unwrap_or(data.len());

        if end < start || end > data.len() {
            return None;
        }

        Some(&data[start..end])
    }
}
