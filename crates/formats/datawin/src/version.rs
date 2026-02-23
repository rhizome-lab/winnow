/// Bytecode version extracted from GEN8.
///
/// Known versions:
/// - 13: Early GameMaker: Studio
/// - 14: GameMaker: Studio 1.x (old instruction format)
/// - 15: GameMaker: Studio 1.4.x (new instruction format, VARI extended)
/// - 16: GameMaker: Studio 1.4.9999+ (adds LANG, GLOB chunks)
/// - 17: GameMaker Studio 2.x
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BytecodeVersion(pub u8);

impl BytecodeVersion {
    pub const V14: Self = Self(14);
    pub const V15: Self = Self(15);
    pub const V16: Self = Self(16);
    pub const V17: Self = Self(17);

    /// Whether the new instruction format is used (v15+).
    /// v14 and below use a different opcode numbering and instruction layout.
    pub fn has_new_instruction_format(self) -> bool {
        self.0 >= 15
    }

    /// Whether the VARI chunk has extended headers (instance count, etc).
    pub fn has_extended_vari(self) -> bool {
        self.0 >= 15
    }

    /// Whether CODE entries have the extended header (locals count, bytecode offset).
    pub fn has_extended_code_entries(self) -> bool {
        self.0 >= 15
    }

    /// Whether LANG and GLOB chunks may be present.
    pub fn has_lang_glob(self) -> bool {
        self.0 >= 16
    }

    /// Whether FUNC/VARI `first_address` points to the operand word (4 bytes
    /// into the instruction) rather than the instruction word itself.
    ///
    /// GMS2.x (BC 17+) changed the FUNC chunk so that `first_address` addresses
    /// the Call operand rather than the instruction word. Earlier formats
    /// (BC 15/16) use instruction-word addressing.
    pub fn func_first_address_is_operand(self) -> bool {
        self.0 >= 17
    }
}

impl std::fmt::Display for BytecodeVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
