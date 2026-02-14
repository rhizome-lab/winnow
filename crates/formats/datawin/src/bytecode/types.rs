/// Data type for instruction operands (4-bit field).
///
/// For branch instructions, the "type" nibbles are really part of the branch
/// offset encoding and may hold values outside the known set.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataType {
    Double,
    Float,
    Int32,
    Int64,
    Bool,
    Variable,
    String,
    Int16,
    /// Raw 4-bit value not matching any known type (e.g. branch offset bits).
    Raw(u8),
}

impl DataType {
    pub fn from_u8(v: u8) -> Self {
        match v {
            0x0 => Self::Double,
            0x1 => Self::Float,
            0x2 => Self::Int32,
            0x3 => Self::Int64,
            0x4 => Self::Bool,
            0x5 => Self::Variable,
            0x6 => Self::String,
            0xF => Self::Int16,
            _ => Self::Raw(v),
        }
    }

    pub fn as_u8(self) -> u8 {
        match self {
            Self::Double => 0x0,
            Self::Float => 0x1,
            Self::Int32 => 0x2,
            Self::Int64 => 0x3,
            Self::Bool => 0x4,
            Self::Variable => 0x5,
            Self::String => 0x6,
            Self::Int16 => 0xF,
            Self::Raw(v) => v,
        }
    }
}

/// Comparison kind for Cmp instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ComparisonKind {
    Less = 1,
    LessEqual = 2,
    Equal = 3,
    NotEqual = 4,
    GreaterEqual = 5,
    Greater = 6,
}

impl ComparisonKind {
    pub fn from_u8(v: u8) -> Option<Self> {
        match v {
            1 => Some(Self::Less),
            2 => Some(Self::LessEqual),
            3 => Some(Self::Equal),
            4 => Some(Self::NotEqual),
            5 => Some(Self::GreaterEqual),
            6 => Some(Self::Greater),
            _ => None,
        }
    }
}

/// Instance type for variable access.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i16)]
pub enum InstanceType {
    /// Current instance (`self`).
    Own = -1,
    /// Other instance in collision event.
    Other = -2,
    /// All instances.
    All = -3,
    /// No instance (object reference follows).
    Noone = -4,
    /// Global scope.
    Global = -5,
    /// Built-in variable.
    Builtin = -6,
    /// Local scope.
    Local = -7,
    /// Stack-top instance (GMS2).
    Stacktop = -9,
    /// Static variable (GMS2.3+).
    Static = -15,
    /// Argument variable.
    Arg = -16,
}

impl InstanceType {
    pub fn from_i16(v: i16) -> Option<Self> {
        match v {
            -1 => Some(Self::Own),
            -2 => Some(Self::Other),
            -3 => Some(Self::All),
            -4 => Some(Self::Noone),
            -5 => Some(Self::Global),
            -6 => Some(Self::Builtin),
            -7 => Some(Self::Local),
            -9 => Some(Self::Stacktop),
            -15 => Some(Self::Static),
            -16 => Some(Self::Arg),
            _ => None,
        }
    }
}

/// Variable reference in bytecode.
///
/// The second word of a variable instruction encodes both the variable ID
/// and a linked-list pointer for patching.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VariableRef {
    /// Variable index (into VARI chunk).
    pub variable_id: u32,
    /// Reference type bits.
    pub ref_type: u8,
}

impl VariableRef {
    pub fn from_raw(raw: u32) -> Self {
        Self {
            variable_id: raw & 0x00FF_FFFF,
            ref_type: ((raw >> 24) & 0xF8) as u8,
        }
    }

    /// Encode back to the raw u32 format.
    pub fn to_raw(self) -> u32 {
        (self.variable_id & 0x00FF_FFFF) | ((self.ref_type as u32) << 24)
    }
}
