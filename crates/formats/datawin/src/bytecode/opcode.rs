/// Opcodes for the GameMaker VM bytecode (v15+ numbering).
///
/// All opcodes use the v15+ values internally. When decoding v14 bytecode,
/// old opcodes are translated to these values at the boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum Opcode {
    // Arithmetic/logic (two-operand)
    Conv = 0x07,
    Mul = 0x08,
    Div = 0x09,
    Rem = 0x0A,
    Mod = 0x0B,
    Add = 0x0C,
    Sub = 0x0D,
    And = 0x0E,
    Or = 0x0F,
    Xor = 0x10,

    // Unary
    Neg = 0x11,
    Not = 0x12,

    // Bit shifts
    Shl = 0x13,
    Shr = 0x14,

    // Comparison (uses ComparisonKind byte)
    Cmp = 0x15,

    // Stack
    Pop = 0x45,
    Dup = 0x86,

    // Control flow
    Ret = 0x9C,
    Exit = 0x9D,
    Popz = 0x9E,

    // Branches (23-bit signed offset in bits 0-22)
    B = 0xB6,
    Bt = 0xB7,
    Bf = 0xB8,

    // Environment (with-statement scoping)
    PushEnv = 0xBA,
    PopEnv = 0xBB,

    // Push variants
    Push = 0xC0,
    PushLoc = 0xC1,
    PushGlb = 0xC2,
    PushBltn = 0xC3,
    PushI = 0x84,

    // Call
    Call = 0xD9,
    CallV = 0x99,

    // Break (special/debug)
    Break = 0xFF,
}

impl Opcode {
    /// Decode an opcode byte (v15+).
    pub fn from_u8(v: u8) -> Option<Self> {
        match v {
            0x07 => Some(Self::Conv),
            0x08 => Some(Self::Mul),
            0x09 => Some(Self::Div),
            0x0A => Some(Self::Rem),
            0x0B => Some(Self::Mod),
            0x0C => Some(Self::Add),
            0x0D => Some(Self::Sub),
            0x0E => Some(Self::And),
            0x0F => Some(Self::Or),
            0x10 => Some(Self::Xor),
            0x11 => Some(Self::Neg),
            0x12 => Some(Self::Not),
            0x13 => Some(Self::Shl),
            0x14 => Some(Self::Shr),
            0x15 => Some(Self::Cmp),
            0x45 => Some(Self::Pop),
            0x84 => Some(Self::PushI),
            0x86 => Some(Self::Dup),
            0x99 => Some(Self::CallV),
            0x9C => Some(Self::Ret),
            0x9D => Some(Self::Exit),
            0x9E => Some(Self::Popz),
            0xB6 => Some(Self::B),
            0xB7 => Some(Self::Bt),
            0xB8 => Some(Self::Bf),
            0xBA => Some(Self::PushEnv),
            0xBB => Some(Self::PopEnv),
            0xC0 => Some(Self::Push),
            0xC1 => Some(Self::PushLoc),
            0xC2 => Some(Self::PushGlb),
            0xC3 => Some(Self::PushBltn),
            0xD9 => Some(Self::Call),
            0xFF => Some(Self::Break),
            _ => None,
        }
    }

    /// Translate a v14 opcode to v15+ opcode.
    pub fn from_v14(v: u8) -> Option<Self> {
        match v {
            0x03 => Some(Self::Conv),
            0x04 => Some(Self::Mul),
            0x05 => Some(Self::Div),
            0x06 => Some(Self::Rem),
            0x07 => Some(Self::Mod),
            0x08 => Some(Self::Add),
            0x09 => Some(Self::Sub),
            0x0A => Some(Self::And),
            0x0B => Some(Self::Or),
            0x0C => Some(Self::Xor),
            0x0D => Some(Self::Neg),
            0x0E => Some(Self::Not),
            0x0F => Some(Self::Shl),
            0x10 => Some(Self::Shr),
            // v14 has separate compare ops that all map to Cmp
            0x11..=0x16 => Some(Self::Cmp),
            0x41 => Some(Self::Pop),
            0x82 => Some(Self::Dup),
            0x9D => Some(Self::Ret),
            0x9E => Some(Self::Exit),
            0x9F => Some(Self::Popz),
            0xB7 => Some(Self::B),
            0xB8 => Some(Self::Bt),
            0xB9 => Some(Self::Bf),
            0xBB => Some(Self::PushEnv),
            0xBC => Some(Self::PopEnv),
            0xC0 => Some(Self::Push),
            0xDA => Some(Self::Call),
            0xFF => Some(Self::Break),
            _ => None,
        }
    }

    /// Whether this instruction reads a second word (or more) as operand data.
    pub fn has_extra_data(self, type1: u8) -> bool {
        match self {
            // Push with non-Int16 type has extra data
            Self::Push | Self::PushLoc | Self::PushGlb | Self::PushBltn => type1 != 0xF,
            Self::PushI => type1 != 0xF, // PushI with Int16 uses inline value
            // Pop with variable types has extra data
            Self::Pop => true,
            // Call always has extra data (function ID)
            Self::Call => true,
            _ => false,
        }
    }

    /// How many extra 32-bit words this instruction consumes.
    pub fn extra_words(self, type1: u8) -> usize {
        match self {
            Self::Push | Self::PushLoc | Self::PushGlb | Self::PushBltn => match type1 {
                0x0 => 2, // Double (8 bytes)
                0x3 => 2, // Int64 (8 bytes)
                0xF => 0, // Int16 (inline)
                _ => 1,   // Int32, Float, String, Variable, Bool
            },
            Self::PushI => match type1 {
                0xF => 0, // Int16 inline
                _ => 1,   // Int32 etc.
            },
            Self::Pop => {
                // Pop with variable destination: 1 extra word
                // Pop with non-variable: 0
                if type1 == 0x5 {
                    1 // Variable ref
                } else {
                    0
                }
            }
            Self::Call => 1, // Function reference
            _ => 0,
        }
    }
}
