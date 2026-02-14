use crate::bytecode::decode::{Instruction, Operand};
use crate::bytecode::opcode::Opcode;

/// Encode a list of instructions back into bytecode.
///
/// This is the inverse of `decode::decode`. Each instruction is encoded as
/// a 32-bit primary word (opcode + types + inline data) followed by zero
/// or more 32-bit operand words.
pub fn encode(instructions: &[Instruction]) -> Vec<u8> {
    let mut out = Vec::new();

    for inst in instructions {
        encode_instruction(inst, &mut out);
    }

    out
}

fn encode_instruction(inst: &Instruction, out: &mut Vec<u8>) {
    let opcode = inst.opcode as u8;
    let type1 = inst.type1.as_u8();
    let type2 = inst.type2.as_u8();

    // Build the primary 32-bit word.
    // Layout: [bits 31-24: opcode] [bits 23-20: type2] [bits 19-16: type1] [bits 15-0: val16]
    //
    // For branches: [bits 31-24: opcode] [bit 23: type flag] [bits 22-0: 23-bit signed offset / 4]
    // For Cmp: [bits 31-24: opcode] [bits 23-20: type2] [bits 19-16: type1] [bits 15-8: cmp_kind] [bits 7-0: 0]

    match &inst.operand {
        Operand::Branch(byte_offset) => {
            // Branch offset is a 23-bit signed value in 4-byte units (bits 0-22).
            // Bit 23 is not part of the offset; it's preserved via the type fields
            // (type2 occupies bits 20-23, so its high bit is bit 23).
            let offset_words = byte_offset / 4;
            let lower16 = (offset_words as u32) & 0x0000_FFFF;
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16)
                | lower16;
            out.extend_from_slice(&word.to_le_bytes());
        }

        Operand::Comparison(kind) => {
            let kind_byte = *kind as u8;
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16)
                | ((kind_byte as u32) << 8);
            out.extend_from_slice(&word.to_le_bytes());
        }

        Operand::Int16(v) => {
            let val16 = *v as u16;
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16)
                | (val16 as u32);
            out.extend_from_slice(&word.to_le_bytes());
        }

        Operand::Call { function_id, argc } => {
            if inst.opcode == Opcode::CallV {
                // CallV: argc in val16, no extra word
                let word = ((opcode as u32) << 24)
                    | ((type2 as u32) << 20)
                    | ((type1 as u32) << 16)
                    | (*argc as u32);
                out.extend_from_slice(&word.to_le_bytes());
            } else {
                // Call: argc in val16, function_id in next word
                let word = ((opcode as u32) << 24)
                    | ((type2 as u32) << 20)
                    | ((type1 as u32) << 16)
                    | (*argc as u32);
                out.extend_from_slice(&word.to_le_bytes());
                out.extend_from_slice(&function_id.to_le_bytes());
            }
        }

        Operand::Variable { var_ref, instance } => {
            let val16 = *instance as u16;
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16)
                | (val16 as u32);
            out.extend_from_slice(&word.to_le_bytes());

            out.extend_from_slice(&var_ref.to_raw().to_le_bytes());
        }

        Operand::Dup(val) => {
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16)
                | (*val as u32);
            out.extend_from_slice(&word.to_le_bytes());
        }

        Operand::Break(val) => {
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16)
                | (*val as u32);
            out.extend_from_slice(&word.to_le_bytes());
        }

        Operand::None => {
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16);
            out.extend_from_slice(&word.to_le_bytes());
        }

        // Extended operands: primary word has val16=0, then extra data words
        Operand::Int32(v) => {
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16);
            out.extend_from_slice(&word.to_le_bytes());
            out.extend_from_slice(&(*v as u32).to_le_bytes());
        }

        Operand::Int64(v) => {
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16);
            out.extend_from_slice(&word.to_le_bytes());
            out.extend_from_slice(&v.to_le_bytes());
        }

        Operand::Double(v) => {
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16);
            out.extend_from_slice(&word.to_le_bytes());
            out.extend_from_slice(&v.to_le_bytes());
        }

        Operand::Float(v) => {
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16);
            out.extend_from_slice(&word.to_le_bytes());
            out.extend_from_slice(&v.to_le_bytes());
        }

        Operand::Bool(v) => {
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16);
            out.extend_from_slice(&word.to_le_bytes());
            let bool_val: u32 = if *v { 1 } else { 0 };
            out.extend_from_slice(&bool_val.to_le_bytes());
        }

        Operand::StringIndex(idx) => {
            let word = ((opcode as u32) << 24)
                | ((type2 as u32) << 20)
                | ((type1 as u32) << 16);
            out.extend_from_slice(&word.to_le_bytes());
            out.extend_from_slice(&idx.to_le_bytes());
        }
    }
}

/// Verify that encoding instructions produces the original bytecode.
///
/// Returns `true` if the round-trip is byte-identical.
pub fn verify_round_trip(original: &[u8], instructions: &[Instruction]) -> bool {
    let encoded = encode(instructions);
    original == encoded.as_slice()
}
