use std::collections::HashMap;

use crate::cursor::Cursor;
use crate::error::Result;

/// A texture page item in the TPAG chunk.
///
/// Describes a rectangular region on a texture atlas page.
#[derive(Debug, Clone)]
pub struct TexturePageItem {
    /// Source X position on the texture page.
    pub source_x: u16,
    /// Source Y position on the texture page.
    pub source_y: u16,
    /// Source width on the texture page.
    pub source_width: u16,
    /// Source height on the texture page.
    pub source_height: u16,
    /// Target X offset when rendering.
    pub target_x: u16,
    /// Target Y offset when rendering.
    pub target_y: u16,
    /// Target (bounding) width.
    pub target_width: u16,
    /// Target (bounding) height.
    pub target_height: u16,
    /// Render width (original sprite width).
    pub render_width: u16,
    /// Render height (original sprite height).
    pub render_height: u16,
    /// Index into the TXTR chunk (which texture atlas page).
    pub texture_page_id: u16,
}

/// Parsed TPAG chunk.
#[derive(Debug)]
pub struct Tpag {
    /// Texture page items, in order.
    pub items: Vec<TexturePageItem>,
    /// Maps absolute file pointer → 0-based item index.
    ///
    /// GMS1 SPRT entries reference TPAG items by their absolute file offset (pointer),
    /// not by 0-based index. Use this map to convert a raw SPRT `tpag_indices` value
    /// to the corresponding index into `items`.
    pub pointer_to_index: HashMap<u32, u32>,
}

impl Tpag {
    /// Size of a single TPAG entry in bytes.
    const ENTRY_SIZE: usize = 22;

    /// Parse the TPAG chunk.
    ///
    /// `chunk_data` is the raw chunk content (after the 8-byte header).
    /// `data` is the full file data (for following absolute pointers).
    pub fn parse(chunk_data: &[u8], data: &[u8]) -> Result<Self> {
        let mut c = Cursor::new(chunk_data);
        let pointers = c.read_pointer_list()?;

        let mut items = Vec::with_capacity(pointers.len());
        let mut pointer_to_index = HashMap::with_capacity(pointers.len());
        for (idx, ptr) in pointers.iter().enumerate() {
            pointer_to_index.insert(*ptr, idx as u32);
            let mut ec = Cursor::new(data);
            ec.seek(*ptr as usize);

            let source_x = ec.read_u16()?;
            let source_y = ec.read_u16()?;
            let source_width = ec.read_u16()?;
            let source_height = ec.read_u16()?;
            let target_x = ec.read_u16()?;
            let target_y = ec.read_u16()?;
            let target_width = ec.read_u16()?;
            let target_height = ec.read_u16()?;
            let render_width = ec.read_u16()?;
            let render_height = ec.read_u16()?;
            let texture_page_id = ec.read_u16()?;

            items.push(TexturePageItem {
                source_x,
                source_y,
                source_width,
                source_height,
                target_x,
                target_y,
                target_width,
                target_height,
                render_width,
                render_height,
                texture_page_id,
            });
        }

        Ok(Self { items, pointer_to_index })
    }

    /// Entry size in bytes (useful for serialization).
    pub fn entry_size() -> usize {
        Self::ENTRY_SIZE
    }
}
