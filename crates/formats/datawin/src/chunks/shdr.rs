use crate::cursor::Cursor;
use crate::error::Result;
use crate::string_table::StringRef;

/// A shader entry in the SHDR chunk.
#[derive(Debug)]
pub struct ShdrEntry {
    /// Reference to the shader name string (e.g. "shd_Edge").
    pub name: StringRef,
    /// Shader kind (0=GLSL ES, 1=GLSL, 2=HLSL9, etc.)
    pub kind: u32,
    /// GLSL ES compatibility flag.
    pub glsl_es: u32,
    /// Reference to the vertex shader GLSL source string.
    pub vertex: StringRef,
    /// Reference to the fragment shader GLSL source string.
    pub fragment: StringRef,
}

/// Parsed SHDR chunk.
#[derive(Debug)]
pub struct Shdr {
    pub shaders: Vec<ShdrEntry>,
}

impl Shdr {
    /// Parse the SHDR chunk.
    pub fn parse(chunk_data: &[u8], data: &[u8]) -> Result<Self> {
        let mut c = Cursor::new(chunk_data);
        let pointers = c.read_pointer_list()?;

        let mut shaders = Vec::with_capacity(pointers.len());
        for ptr in pointers {
            let mut ec = Cursor::new(data);
            ec.seek(ptr as usize);
            let name = StringRef(ec.read_u32()?);
            let kind = ec.read_u32()?;
            let glsl_es = ec.read_u32()?;
            let vertex = StringRef(ec.read_u32()?);
            let fragment = StringRef(ec.read_u32()?);
            // GMS2 may have attribute/uniform name lists after this point;
            // we stop here since we only need the source strings.
            shaders.push(ShdrEntry { name, kind, glsl_es, vertex, fragment });
        }

        Ok(Self { shaders })
    }
}
