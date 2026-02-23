use std::cell::RefCell;
use std::collections::HashMap;

use crate::chunks::audo::Audo;
use crate::chunks::code::Code;
use crate::chunks::font::Font;
use crate::chunks::func::Func;
use crate::chunks::gen8::Gen8;
use crate::chunks::glob::Glob;
use crate::chunks::lang::Lang;
use crate::chunks::objt::Objt;
use crate::chunks::optn::Optn;
use crate::chunks::room::Room;
use crate::chunks::scpt::Scpt;
use crate::chunks::sond::Sond;
use crate::chunks::sprt::Sprt;
use crate::chunks::tpag::Tpag;
use crate::chunks::txtr::Txtr;
use crate::chunks::vari::Vari;
use crate::error::{Error, Result};
use crate::reader::ChunkIndex;
use crate::string_table::StringTable;
use crate::version::BytecodeVersion;

/// High-level lazy wrapper over a GameMaker data.win file.
///
/// Typed chunk accessors parse on first access and cache the result.
/// The full file data is retained for string resolution and raw chunk access.
pub struct DataWin {
    /// Raw file data.
    data: Vec<u8>,
    /// Chunk index (always parsed eagerly).
    index: ChunkIndex,
    /// Lazily parsed chunks, stored as type-erased boxes keyed by chunk magic.
    cache: RefCell<HashMap<[u8; 4], Box<dyn std::any::Any>>>,
}

impl DataWin {
    /// Parse a data.win file (or a PE exe containing an embedded data.win) from raw bytes.
    ///
    /// If `data` begins with the PE magic `MZ`, the file is a Windows executable with an
    /// embedded GameMaker FORM blob (e.g. `MomodoraRUtM.exe`). In that case, the FORM header
    /// is located by scanning for the `FORM` signature and the data is trimmed to start there.
    ///
    /// Only the FORM envelope and chunk index are parsed eagerly.
    /// Individual chunk contents are parsed lazily on first access.
    pub fn parse(mut data: Vec<u8>) -> Result<Self> {
        // Detect PE-wrapped data.win and strip the PE prefix.
        if data.starts_with(b"MZ") {
            if let Some(offset) = find_embedded_form(&data) {
                data.drain(..offset);
            }
        }
        let index = ChunkIndex::parse(&data)?;
        Ok(Self {
            data,
            index,
            cache: RefCell::new(HashMap::new()),
        })
    }

    /// Raw file data.
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    /// Chunk index.
    pub fn index(&self) -> &ChunkIndex {
        &self.index
    }

    /// Whether a chunk with the given magic exists.
    pub fn has_chunk(&self, magic: &[u8; 4]) -> bool {
        self.index.find(magic).is_some()
    }

    /// Get or parse a chunk, caching the result.
    fn get_or_parse<T: 'static>(&self, magic: &[u8; 4], parse: impl FnOnce() -> Result<T>) -> Result<()> {
        let cache = self.cache.borrow();
        if cache.contains_key(magic) {
            return Ok(());
        }
        drop(cache);
        let value = parse()?;
        self.cache.borrow_mut().insert(*magic, Box::new(value));
        Ok(())
    }

    /// Retrieve a cached chunk reference.
    fn cached<T: 'static>(&self, magic: &[u8; 4]) -> &T {
        let cache = self.cache.borrow();
        let boxed = cache.get(magic).expect("chunk should be cached");
        let ptr = boxed.downcast_ref::<T>().expect("type mismatch") as *const T;
        // SAFETY: The data lives in the HashMap which is owned by self.
        // We never remove entries, and self is borrowed immutably.
        unsafe { &*ptr }
    }

    /// GEN8 metadata (bytecode version, game info, etc.).
    pub fn gen8(&self) -> Result<&Gen8> {
        self.get_or_parse(b"GEN8", || {
            let chunk_data = self.index.chunk_data(&self.data, b"GEN8")?;
            Gen8::parse(chunk_data)
        })?;
        Ok(self.cached(b"GEN8"))
    }

    /// Bytecode version (shortcut for `gen8().bytecode_version`).
    pub fn bytecode_version(&self) -> Result<BytecodeVersion> {
        Ok(self.gen8()?.bytecode_version)
    }

    /// String table (STRG chunk).
    pub fn strings(&self) -> Result<&StringTable> {
        self.get_or_parse(b"STRG", || {
            let entry = self
                .index
                .find(b"STRG")
                .ok_or(Error::ChunkNotFound { magic: *b"STRG" })?;
            let chunk_data = self.index.chunk_data(&self.data, b"STRG")?;
            StringTable::parse(chunk_data, entry.data_offset())
        })?;
        Ok(self.cached(b"STRG"))
    }

    /// Resolve a string reference using the file data.
    pub fn resolve_string(&self, sref: crate::string_table::StringRef) -> Result<String> {
        sref.resolve(&self.data)
    }

    /// CODE chunk (bytecode entries).
    pub fn code(&self) -> Result<&Code> {
        self.get_or_parse(b"CODE", || {
            let entry = self
                .index
                .find(b"CODE")
                .ok_or(Error::ChunkNotFound { magic: *b"CODE" })?;
            let chunk_data = self.index.chunk_data(&self.data, b"CODE")?;
            let version = self.gen8()?.bytecode_version;
            Code::parse(chunk_data, entry.data_offset(), version)
        })?;
        Ok(self.cached(b"CODE"))
    }

    /// FUNC chunk (function definitions + code locals).
    pub fn func(&self) -> Result<&Func> {
        self.get_or_parse(b"FUNC", || {
            let chunk_data = self.index.chunk_data(&self.data, b"FUNC")?;
            let version = self.gen8()?.bytecode_version;
            Func::parse(chunk_data, version)
        })?;
        Ok(self.cached(b"FUNC"))
    }

    /// VARI chunk (variable definitions).
    pub fn vari(&self) -> Result<&Vari> {
        self.get_or_parse(b"VARI", || {
            let chunk_data = self.index.chunk_data(&self.data, b"VARI")?;
            let version = self.gen8()?.bytecode_version;
            Vari::parse(chunk_data, version)
        })?;
        Ok(self.cached(b"VARI"))
    }

    /// OBJT chunk (object definitions with events).
    pub fn objt(&self) -> Result<&Objt> {
        self.get_or_parse(b"OBJT", || {
            let chunk_data = self.index.chunk_data(&self.data, b"OBJT")?;
            let version = self.gen8()?.bytecode_version;
            Objt::parse(chunk_data, &self.data, version)
        })?;
        Ok(self.cached(b"OBJT"))
    }

    /// SCPT chunk (script entries).
    pub fn scpt(&self) -> Result<&Scpt> {
        self.get_or_parse(b"SCPT", || {
            let entry = self
                .index
                .find(b"SCPT")
                .ok_or(Error::ChunkNotFound { magic: *b"SCPT" })?;
            let chunk_data = self.index.chunk_data(&self.data, b"SCPT")?;
            Scpt::parse(chunk_data, entry.data_offset(), &self.data)
        })?;
        Ok(self.cached(b"SCPT"))
    }

    /// OPTN chunk (game options).
    pub fn optn(&self) -> Result<&Optn> {
        self.get_or_parse(b"OPTN", || {
            let chunk_data = self.index.chunk_data(&self.data, b"OPTN")?;
            Optn::parse(chunk_data)
        })?;
        Ok(self.cached(b"OPTN"))
    }

    /// SOND chunk (sound definitions).
    pub fn sond(&self) -> Result<&Sond> {
        self.get_or_parse(b"SOND", || {
            let chunk_data = self.index.chunk_data(&self.data, b"SOND")?;
            Sond::parse(chunk_data, &self.data)
        })?;
        Ok(self.cached(b"SOND"))
    }

    /// SPRT chunk (sprite definitions).
    pub fn sprt(&self) -> Result<&Sprt> {
        self.get_or_parse(b"SPRT", || {
            let chunk_data = self.index.chunk_data(&self.data, b"SPRT")?;
            Sprt::parse(chunk_data, &self.data)
        })?;
        Ok(self.cached(b"SPRT"))
    }

    /// TPAG chunk (texture page items).
    pub fn tpag(&self) -> Result<&Tpag> {
        self.get_or_parse(b"TPAG", || {
            let chunk_data = self.index.chunk_data(&self.data, b"TPAG")?;
            Tpag::parse(chunk_data, &self.data)
        })?;
        Ok(self.cached(b"TPAG"))
    }

    /// TXTR chunk (texture pages).
    pub fn txtr(&self) -> Result<&Txtr> {
        self.get_or_parse(b"TXTR", || {
            let chunk_data = self.index.chunk_data(&self.data, b"TXTR")?;
            Txtr::parse(chunk_data, &self.data)
        })?;
        Ok(self.cached(b"TXTR"))
    }

    /// AUDO chunk (embedded audio data).
    pub fn audo(&self) -> Result<&Audo> {
        self.get_or_parse(b"AUDO", || {
            let entry = self
                .index
                .find(b"AUDO")
                .ok_or(Error::ChunkNotFound { magic: *b"AUDO" })?;
            let chunk_data = self.index.chunk_data(&self.data, b"AUDO")?;
            Audo::parse(chunk_data, entry.data_offset())
        })?;
        Ok(self.cached(b"AUDO"))
    }

    /// FONT chunk (font definitions).
    pub fn font(&self) -> Result<&Font> {
        self.get_or_parse(b"FONT", || {
            let chunk_data = self.index.chunk_data(&self.data, b"FONT")?;
            Font::parse(chunk_data, &self.data)
        })?;
        Ok(self.cached(b"FONT"))
    }

    /// ROOM chunk (room definitions).
    pub fn room(&self) -> Result<&Room> {
        self.get_or_parse(b"ROOM", || {
            let chunk_data = self.index.chunk_data(&self.data, b"ROOM")?;
            Room::parse(chunk_data, &self.data)
        })?;
        Ok(self.cached(b"ROOM"))
    }

    /// GLOB chunk (global script IDs). Returns `None` if not present.
    pub fn glob(&self) -> Result<Option<&Glob>> {
        if !self.has_chunk(b"GLOB") {
            return Ok(None);
        }
        self.get_or_parse(b"GLOB", || {
            let chunk_data = self.index.chunk_data(&self.data, b"GLOB")?;
            Glob::parse(chunk_data)
        })?;
        Ok(Some(self.cached(b"GLOB")))
    }

    /// LANG chunk (language settings). Returns `None` if not present.
    pub fn lang(&self) -> Result<Option<&Lang>> {
        if !self.has_chunk(b"LANG") {
            return Ok(None);
        }
        self.get_or_parse(b"LANG", || {
            let chunk_data = self.index.chunk_data(&self.data, b"LANG")?;
            Lang::parse(chunk_data)
        })?;
        Ok(Some(self.cached(b"LANG")))
    }
}

/// Scan `data` (a Windows PE executable) for an embedded GameMaker FORM blob.
///
/// Searches every `FORM` occurrence and validates that the declared FORM size fits
/// within the remaining file. Returns the byte offset of the first valid FORM header,
/// or `None` if no valid FORM is found.
///
/// A PE file may contain false-positive `FORM` byte sequences (e.g. inside the
/// import table or resource section), so each candidate is validated before accepting.
fn find_embedded_form(data: &[u8]) -> Option<usize> {
    const FORM: &[u8] = b"FORM";
    // Walk every byte looking for the FORM signature. The PE prefix is typically
    // several MB so this linear scan is acceptable (done once at startup).
    for offset in 0..data.len().saturating_sub(7) {
        if &data[offset..offset + 4] != FORM {
            continue;
        }
        // Read the 4-byte little-endian size field immediately after the magic.
        let size_bytes: [u8; 4] = data[offset + 4..offset + 8].try_into().ok()?;
        let form_size = u32::from_le_bytes(size_bytes) as usize;
        // Validate: 8-byte header + content must fit within the file.
        if offset + 8 + form_size <= data.len() {
            return Some(offset);
        }
    }
    None
}

impl std::fmt::Debug for DataWin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DataWin")
            .field("size", &self.data.len())
            .field("chunks", &self.index.len())
            .finish()
    }
}
