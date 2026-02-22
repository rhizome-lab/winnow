use crate::cursor::Cursor;
use crate::error::Result;
use crate::string_table::StringRef;
use crate::version::BytecodeVersion;

/// A function definition in the FUNC chunk.
#[derive(Debug)]
pub struct FunctionEntry {
    /// Reference to the function name string.
    pub name: StringRef,
    /// Number of occurrences (call sites referencing this function).
    pub occurrences: u32,
    /// Address of the first occurrence in bytecode, or -1 if none.
    pub first_address: i32,
}

/// Local variable information for a single code entry.
#[derive(Debug)]
pub struct CodeLocals {
    /// Reference to the code entry name (e.g., "gml_Script_foo").
    pub name: StringRef,
    /// Local variables used in this code entry.
    pub locals: Vec<LocalVar>,
}

/// A local variable within a code entry.
#[derive(Debug)]
pub struct LocalVar {
    /// Local variable index (0-based).
    pub index: u32,
    /// Reference to the variable name string.
    pub name: StringRef,
}

/// Parsed FUNC chunk.
#[derive(Debug)]
pub struct Func {
    /// Function definitions.
    pub functions: Vec<FunctionEntry>,
    /// Per-code-entry local variable information (BC >= 15 only).
    pub code_locals: Vec<CodeLocals>,
}

impl Func {
    /// Parse the FUNC chunk.
    ///
    /// `chunk_data` is the raw chunk content (after the 8-byte header).
    pub fn parse(chunk_data: &[u8], version: BytecodeVersion) -> Result<Self> {
        let mut c = Cursor::new(chunk_data);

        if version.0 <= 14 {
            // BC <= 14: just a flat list of function entries, no pointer list header
            let mut functions = Vec::new();
            while c.remaining() >= 12 {
                let name = StringRef(c.read_u32()?);
                let occurrences = c.read_u32()?;
                let first_address = c.read_i32()?;
                functions.push(FunctionEntry {
                    name,
                    occurrences,
                    first_address,
                });
            }
            return Ok(Self {
                functions,
                code_locals: Vec::new(),
            });
        }

        // BC >= 15: Functions list (count + entries), then CodeLocals list (count + entries).
        // A 0-size FUNC chunk means the game was compiled with YYC (no bytecode); return empty.
        if c.remaining() == 0 {
            return Ok(Self { functions: Vec::new(), code_locals: Vec::new() });
        }

        // Functions: count(u32) + count × 12 bytes
        let func_count = c.read_u32()? as usize;
        let mut functions = Vec::with_capacity(func_count);
        for _ in 0..func_count {
            let name = StringRef(c.read_u32()?);
            let occurrences = c.read_u32()?;
            let first_address = c.read_i32()?;
            functions.push(FunctionEntry {
                name,
                occurrences,
                first_address,
            });
        }

        // CodeLocals: count(u32) + count × variable-size entries
        let mut code_locals = Vec::new();
        if c.remaining() >= 4 {
            let locals_count = c.read_u32()? as usize;
            code_locals.reserve(locals_count);
            for _ in 0..locals_count {
                let var_count = c.read_u32()? as usize;
                let name = StringRef(c.read_u32()?);
                let mut locals = Vec::with_capacity(var_count);
                for _ in 0..var_count {
                    let index = c.read_u32()?;
                    let var_name = StringRef(c.read_u32()?);
                    locals.push(LocalVar {
                        index,
                        name: var_name,
                    });
                }
                code_locals.push(CodeLocals { name, locals });
            }
        }

        Ok(Self {
            functions,
            code_locals,
        })
    }
}
