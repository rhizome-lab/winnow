use crate::bytecode::opcode::Opcode;
use crate::bytecode::types::{ComparisonKind, DataType, VariableRef};
use crate::error::{Error, Result};

/// A decoded instruction.
#[derive(Debug, Clone)]
pub struct Instruction {
    /// Byte offset within the code entry's bytecode.
    pub offset: usize,
    /// The operation.
    pub opcode: Opcode,
    /// First type field (bits 16-19 of the instruction word).
    pub type1: DataType,
    /// Second type field (bits 20-23 of the instruction word).
    pub type2: DataType,
    /// The operand data.
    pub operand: Operand,
}

/// Operand data for an instruction.
#[derive(Debug, Clone)]
pub enum Operand {
    /// No operand.
    None,
    /// 16-bit integer value (inline in the instruction word).
    Int16(i16),
    /// 32-bit integer value.
    Int32(i32),
    /// 64-bit integer value.
    Int64(i64),
    /// 64-bit float value.
    Double(f64),
    /// 32-bit float value.
    Float(f32),
    /// Boolean value.
    Bool(bool),
    /// String reference (index into STRG).
    StringIndex(u32),
    /// Variable reference.
    Variable {
        var_ref: VariableRef,
        instance: i16,
    },
    /// Branch target (byte offset from current instruction).
    Branch(i32),
    /// Comparison kind.
    Comparison(ComparisonKind),
    /// Function call: function_id and argument count.
    Call { function_id: u32, argc: u16 },
    /// Dup: extra parameter in lower 16 bits.
    Dup(u16),
    /// Break: signal type in lower 16 bits.
    Break(u16),
}

/// Decode bytecode for a single code entry.
///
/// `bytecode` is the raw bytecode bytes for this code entry.
/// Returns the list of decoded instructions.
pub fn decode(bytecode: &[u8]) -> Result<Vec<Instruction>> {
    let mut instructions = Vec::new();
    let mut pos = 0;

    while pos < bytecode.len() {
        let inst_offset = pos;

        if pos + 4 > bytecode.len() {
            return Err(Error::Parse {
                context: "bytecode",
                message: format!(
                    "truncated instruction at offset {:#x} ({} bytes remaining)",
                    pos,
                    bytecode.len() - pos
                ),
            });
        }

        let word = u32::from_le_bytes([
            bytecode[pos],
            bytecode[pos + 1],
            bytecode[pos + 2],
            bytecode[pos + 3],
        ]);
        pos += 4;

        let opcode_byte = ((word >> 24) & 0xFF) as u8;
        let type2_raw = ((word >> 20) & 0xF) as u8;
        let type1_raw = ((word >> 16) & 0xF) as u8;
        let val16 = (word & 0xFFFF) as u16;

        let opcode = Opcode::from_u8(opcode_byte).ok_or_else(|| Error::Parse {
            context: "bytecode",
            message: format!("unknown opcode {:#04x} at offset {:#x}", opcode_byte, inst_offset),
        })?;

        let type1 = DataType::from_u8(type1_raw);
        let type2 = DataType::from_u8(type2_raw);

        let operand = decode_operand(opcode, type1, type2, val16, word, bytecode, &mut pos)?;

        instructions.push(Instruction {
            offset: inst_offset,
            opcode,
            type1,
            type2,
            operand,
        });
    }

    Ok(instructions)
}

fn read_u32(bytecode: &[u8], pos: &mut usize) -> Result<u32> {
    if *pos + 4 > bytecode.len() {
        return Err(Error::Parse {
            context: "bytecode",
            message: format!("truncated operand at offset {:#x}", pos),
        });
    }
    let v = u32::from_le_bytes([
        bytecode[*pos],
        bytecode[*pos + 1],
        bytecode[*pos + 2],
        bytecode[*pos + 3],
    ]);
    *pos += 4;
    Ok(v)
}

fn read_i32(bytecode: &[u8], pos: &mut usize) -> Result<i32> {
    Ok(read_u32(bytecode, pos)? as i32)
}

fn read_f64(bytecode: &[u8], pos: &mut usize) -> Result<f64> {
    if *pos + 8 > bytecode.len() {
        return Err(Error::Parse {
            context: "bytecode",
            message: format!("truncated f64 at offset {:#x}", pos),
        });
    }
    let v = f64::from_le_bytes([
        bytecode[*pos],
        bytecode[*pos + 1],
        bytecode[*pos + 2],
        bytecode[*pos + 3],
        bytecode[*pos + 4],
        bytecode[*pos + 5],
        bytecode[*pos + 6],
        bytecode[*pos + 7],
    ]);
    *pos += 8;
    Ok(v)
}

fn read_i64(bytecode: &[u8], pos: &mut usize) -> Result<i64> {
    if *pos + 8 > bytecode.len() {
        return Err(Error::Parse {
            context: "bytecode",
            message: format!("truncated i64 at offset {:#x}", pos),
        });
    }
    let v = i64::from_le_bytes([
        bytecode[*pos],
        bytecode[*pos + 1],
        bytecode[*pos + 2],
        bytecode[*pos + 3],
        bytecode[*pos + 4],
        bytecode[*pos + 5],
        bytecode[*pos + 6],
        bytecode[*pos + 7],
    ]);
    *pos += 8;
    Ok(v)
}

fn read_f32(bytecode: &[u8], pos: &mut usize) -> Result<f32> {
    if *pos + 4 > bytecode.len() {
        return Err(Error::Parse {
            context: "bytecode",
            message: format!("truncated f32 at offset {:#x}", pos),
        });
    }
    let v = f32::from_le_bytes([
        bytecode[*pos],
        bytecode[*pos + 1],
        bytecode[*pos + 2],
        bytecode[*pos + 3],
    ]);
    *pos += 4;
    Ok(v)
}

fn decode_operand(
    opcode: Opcode,
    type1: DataType,
    _type2: DataType,
    val16: u16,
    word: u32,
    bytecode: &[u8],
    pos: &mut usize,
) -> Result<Operand> {
    match opcode {
        // Push variants: read operand based on type1
        Opcode::Push | Opcode::PushLoc | Opcode::PushGlb | Opcode::PushBltn => {
            decode_push_operand(type1, val16, bytecode, pos)
        }

        Opcode::PushI => match type1 {
            DataType::Int16 => Ok(Operand::Int16(val16 as i16)),
            DataType::Int32 => Ok(Operand::Int32(read_i32(bytecode, pos)?)),
            _ => Ok(Operand::Int16(val16 as i16)),
        },

        // Pop: type1 determines if we read a variable ref
        Opcode::Pop => {
            let instance = val16 as i16;
            let var_raw = read_u32(bytecode, pos)?;
            Ok(Operand::Variable {
                var_ref: VariableRef::from_raw(var_raw),
                instance,
            })
        }

        // Branch instructions: 23-bit signed offset in bits 22-0 of the word.
        // Bit 23 is NOT part of the offset (it encodes comparison/type info).
        Opcode::B | Opcode::Bt | Opcode::Bf | Opcode::PushEnv | Opcode::PopEnv => {
            let raw23 = word & 0x007F_FFFF;
            // Sign-extend from 23 bits
            let offset = if raw23 & 0x40_0000 != 0 {
                (raw23 | 0xFF80_0000) as i32
            } else {
                raw23 as i32
            };
            // Offset is in 4-byte units
            Ok(Operand::Branch(offset * 4))
        }

        // Comparison: comparison kind in bits 15-8
        Opcode::Cmp => {
            let cmp_byte = ((word >> 8) & 0xFF) as u8;
            let kind = ComparisonKind::from_u8(cmp_byte).ok_or_else(|| Error::Parse {
                context: "bytecode",
                message: format!("unknown comparison kind {}", cmp_byte),
            })?;
            Ok(Operand::Comparison(kind))
        }

        // Call: argc in val16, function_id in next word
        Opcode::Call => {
            let function_id = read_u32(bytecode, pos)?;
            Ok(Operand::Call {
                function_id,
                argc: val16,
            })
        }

        // CallV: argc in val16
        Opcode::CallV => Ok(Operand::Call {
            function_id: 0,
            argc: val16,
        }),

        Opcode::Dup => Ok(Operand::Dup(val16)),
        Opcode::Break => Ok(Operand::Break(val16)),

        // All others: no operand
        _ => Ok(Operand::None),
    }
}

fn decode_push_operand(
    type1: DataType,
    val16: u16,
    bytecode: &[u8],
    pos: &mut usize,
) -> Result<Operand> {
    match type1 {
        DataType::Double => Ok(Operand::Double(read_f64(bytecode, pos)?)),
        DataType::Float => Ok(Operand::Float(read_f32(bytecode, pos)?)),
        DataType::Int32 => Ok(Operand::Int32(read_i32(bytecode, pos)?)),
        DataType::Int64 => Ok(Operand::Int64(read_i64(bytecode, pos)?)),
        DataType::Bool => Ok(Operand::Bool(read_u32(bytecode, pos)? != 0)),
        DataType::String => Ok(Operand::StringIndex(read_u32(bytecode, pos)?)),
        DataType::Variable => {
            let instance = val16 as i16;
            let var_raw = read_u32(bytecode, pos)?;
            Ok(Operand::Variable {
                var_ref: VariableRef::from_raw(var_raw),
                instance,
            })
        }
        DataType::Int16 => Ok(Operand::Int16(val16 as i16)),
        DataType::Raw(v) => Err(Error::Parse {
            context: "bytecode",
            message: format!("unknown push data type {v:#x}"),
        }),
    }
}
