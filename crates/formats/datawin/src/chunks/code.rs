use std::collections::HashMap;

use crate::cursor::Cursor;
use crate::error::Result;
use crate::string_table::StringRef;
use crate::version::BytecodeVersion;

/// A single code entry in the CODE chunk.
#[derive(Debug)]
pub struct CodeEntry {
    /// Reference to the entry's name string (e.g., "gml_Script_foo").
    pub name: StringRef,
    /// Length of bytecode in bytes.
    pub length: u32,
    /// Number of local variables (BC >= 15).
    pub locals_count: u16,
    /// Number of arguments. Bit 15 is "weird local flag" in some tools.
    pub args_count: u16,
    /// Absolute file offset where this entry's bytecode begins.
    pub bytecode_offset: usize,
}

/// Parsed CODE chunk.
#[derive(Debug)]
pub struct Code {
    pub entries: Vec<CodeEntry>,
}

impl Code {
    /// Parse the CODE chunk.
    ///
    /// `chunk_data` is the raw chunk content (after the 8-byte header).
    /// `chunk_data_offset` is the absolute file offset where `chunk_data` begins.
    /// `version` is needed to select the correct entry format.
    pub fn parse(
        chunk_data: &[u8],
        chunk_data_offset: usize,
        version: BytecodeVersion,
    ) -> Result<Self> {
        let mut c = Cursor::new(chunk_data);
        let ptrs = c.read_pointer_list()?;

        let mut entries = Vec::with_capacity(ptrs.len());

        if version.has_extended_code_entries() {
            // BC >= 15: extended format with locals, args, bytecode offset.
            //
            // In GMS2.3+, child functions (lambdas, struct constructors) share
            // a bytecode blob with their parent. Each entry has:
            //   - `length`: the TOTAL blob length (same for parent + all children)
            //   - `offset_in_blob`: where this entry's code starts within the blob
            //
            // To compute each entry's actual bytecode length, we:
            // 1. Collect all entries with their blob start address and offset
            // 2. Group entries by blob address
            // 3. Sort each group by offset
            // 4. Compute each entry's length as the gap to the next entry's offset
            //    (or blob_length - offset for the last entry in the group)

            // First pass: parse all entry headers.
            struct RawEntry {
                name: StringRef,
                blob_length: u32,
                locals_count: u16,
                args_count: u16,
                bc_blob: usize,
                offset_in_blob: u32,
            }

            let mut raw_entries = Vec::with_capacity(ptrs.len());
            for &ptr in &ptrs {
                let rel = ptr as usize - chunk_data_offset;
                let mut ec = Cursor::new(chunk_data);
                ec.seek(rel);

                let name = StringRef(ec.read_u32()?);
                let blob_length = ec.read_u32()?;
                let locals_count = ec.read_u16()?;
                let args_count = ec.read_u16()?;
                let bc_rel_addr = ec.read_i32()?;
                let offset_in_blob = ec.read_u32()?;

                // bc_rel_addr is relative to the field that contains it.
                // The field is at ptr + 12 (after name:4 + length:4 + locals:2 + args:2).
                let bc_blob = (ptr as i64 + 12 + bc_rel_addr as i64) as usize;

                raw_entries.push(RawEntry {
                    name,
                    blob_length,
                    locals_count,
                    args_count,
                    bc_blob,
                    offset_in_blob,
                });
            }

            // Second pass: group entries by blob address and compute per-entry lengths.
            // Map: blob_address → sorted list of (offset_in_blob, blob_length)
            let mut blob_groups: HashMap<usize, Vec<(u32, u32)>> = HashMap::new();
            for raw in &raw_entries {
                blob_groups
                    .entry(raw.bc_blob)
                    .or_default()
                    .push((raw.offset_in_blob, raw.blob_length));
            }
            // Sort each group by offset and build a lookup: (blob, offset) → actual length
            let mut length_map: HashMap<(usize, u32), u32> = HashMap::new();
            for (&blob, offsets) in &mut blob_groups {
                offsets.sort_unstable();
                offsets.dedup();
                for i in 0..offsets.len() {
                    let (offset, blob_length) = offsets[i];
                    let end = if i + 1 < offsets.len() {
                        offsets[i + 1].0
                    } else {
                        blob_length
                    };
                    length_map.insert((blob, offset), end - offset);
                }
            }

            // Third pass: build final entries with correct lengths.
            for raw in raw_entries {
                let bc_abs = raw.bc_blob + raw.offset_in_blob as usize;
                let child_length = length_map
                    .get(&(raw.bc_blob, raw.offset_in_blob))
                    .copied()
                    .unwrap_or(raw.blob_length - raw.offset_in_blob);

                entries.push(CodeEntry {
                    name: raw.name,
                    length: child_length,
                    locals_count: raw.locals_count,
                    args_count: raw.args_count,
                    bytecode_offset: bc_abs,
                });
            }
        } else {
            // BC < 15: bytecode immediately follows the entry header
            for &ptr in &ptrs {
                let rel = ptr as usize - chunk_data_offset;
                let mut ec = Cursor::new(chunk_data);
                ec.seek(rel);

                let name = StringRef(ec.read_u32()?);
                let length = ec.read_u32()?;
                let bc_abs = ptr as usize + 8; // after name(4) + length(4)

                entries.push(CodeEntry {
                    name,
                    length,
                    locals_count: 0,
                    args_count: 0,
                    bytecode_offset: bc_abs,
                });
            }
        }

        Ok(Self { entries })
    }

    /// Extract bytecode bytes for a specific entry from the full file data.
    pub fn entry_bytecode<'a>(&self, index: usize, data: &'a [u8]) -> Option<&'a [u8]> {
        let entry = self.entries.get(index)?;
        let start = entry.bytecode_offset;
        let end = start + entry.length as usize;
        if end <= data.len() {
            Some(&data[start..end])
        } else {
            None
        }
    }
}
