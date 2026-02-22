use crate::cursor::Cursor;
use crate::error::Result;
use crate::string_table::StringRef;
use crate::version::BytecodeVersion;

/// A variable definition in the VARI chunk.
#[derive(Debug)]
pub struct VariableEntry {
    /// Reference to the variable name string.
    pub name: StringRef,
    /// Instance type for this variable (BC >= 15).
    /// -1 = self, -5 = global, -7 = local, etc.
    pub instance_type: i32,
    /// Variable ID within its scope (BC >= 15).
    pub var_id: i32,
    /// Number of occurrences in bytecode.
    pub occurrences: u32,
    /// Address of the first occurrence, or -1 if none.
    pub first_address: i32,
}

/// Parsed VARI chunk.
#[derive(Debug)]
pub struct Vari {
    /// Number of instance variables with instance_type >= 0.
    /// Only meaningful for BC >= 15.
    pub instance_var_count: u32,
    /// Total number of instance variable IDs used.
    pub instance_var_count_max: u32,
    /// Maximum local variable count across all code entries.
    pub max_local_var_count: u32,
    /// Variable entries.
    pub variables: Vec<VariableEntry>,
}

impl Vari {
    /// Parse the VARI chunk.
    ///
    /// `chunk_data` is the raw chunk content (after the 8-byte header).
    pub fn parse(chunk_data: &[u8], version: BytecodeVersion) -> Result<Self> {
        // A 0-size VARI chunk means the game was compiled with YYC (no bytecode); return empty.
        if chunk_data.is_empty() {
            return Ok(Self {
                instance_var_count: 0,
                instance_var_count_max: 0,
                max_local_var_count: 0,
                variables: Vec::new(),
            });
        }

        let mut c = Cursor::new(chunk_data);

        let (instance_var_count, instance_var_count_max, max_local_var_count) =
            if version.has_extended_vari() {
                // BC >= 15: three u32 header fields
                let a = c.read_u32()?;
                let b = c.read_u32()?;
                let d = c.read_u32()?;
                (a, b, d)
            } else {
                (0, 0, 0)
            };

        // Entry size depends on version
        let entry_size = if version.has_extended_vari() { 20 } else { 12 };
        let entry_count = c.remaining() / entry_size;

        let mut variables = Vec::with_capacity(entry_count);
        for _ in 0..entry_count {
            let name = StringRef(c.read_u32()?);
            let (instance_type, var_id) = if version.has_extended_vari() {
                (c.read_i32()?, c.read_i32()?)
            } else {
                (0, 0)
            };
            let occurrences = c.read_u32()?;
            let first_address = c.read_i32()?;
            variables.push(VariableEntry {
                name,
                instance_type,
                var_id,
                occurrences,
                first_address,
            });
        }

        Ok(Self {
            instance_var_count,
            instance_var_count_max,
            max_local_var_count,
            variables,
        })
    }
}
