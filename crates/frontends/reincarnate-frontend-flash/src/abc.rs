//! ABC parsing wrapper: raw bytecode → Vec<Op> with byte offset tracking.

use swf::avm2::read::Reader;
use swf::avm2::types::Op;
use swf::extensions::ReadSwfExt;

/// A parsed opcode together with the byte offset where it starts in the method body.
#[derive(Debug)]
pub struct LocatedOp {
    pub op: Op,
    /// Byte offset of this op within the method body's `code` array.
    pub offset: usize,
}

/// Parse a method body's raw bytecode into located ops.
///
/// Returns a list of ops with their byte offsets, plus a mapping from
/// byte offset → op index for resolving branch targets.
pub fn parse_bytecode(code: &[u8]) -> Result<Vec<LocatedOp>, String> {
    let mut reader = Reader::new(code);
    let mut ops = Vec::new();

    loop {
        let offset = reader.as_slice().as_ptr() as usize - code.as_ptr() as usize;
        if offset >= code.len() {
            break;
        }
        match reader.read_op() {
            Ok(op) => {
                ops.push(LocatedOp { op, offset });
            }
            Err(e) => {
                return Err(format!("failed to parse op at offset {offset}: {e}"));
            }
        }
    }

    Ok(ops)
}

/// Build a mapping from byte offset → op index.
pub fn offset_to_index_map(ops: &[LocatedOp]) -> std::collections::HashMap<usize, usize> {
    ops.iter()
        .enumerate()
        .map(|(i, loc)| (loc.offset, i))
        .collect()
}

/// Resolve a relative branch offset to an op index.
///
/// AVM2 branch offsets are relative to the byte position of the *next* instruction.
/// `current_op_idx` is the index of the branch instruction in the ops list.
pub fn resolve_branch_target(
    ops: &[LocatedOp],
    offset_map: &std::collections::HashMap<usize, usize>,
    current_op_idx: usize,
    relative_offset: i32,
) -> Option<usize> {
    // The next instruction's byte offset
    let next_offset = if current_op_idx + 1 < ops.len() {
        ops[current_op_idx + 1].offset
    } else {
        // After last instruction — use total code length
        return None;
    };
    let target_byte = (next_offset as i64 + relative_offset as i64) as usize;
    offset_map.get(&target_byte).copied()
}

/// Resolve a LookupSwitch offset, which is relative to the switch instruction itself.
pub fn resolve_switch_target(
    ops: &[LocatedOp],
    offset_map: &std::collections::HashMap<usize, usize>,
    switch_op_idx: usize,
    relative_offset: i32,
) -> Option<usize> {
    let switch_byte = ops[switch_op_idx].offset;
    let target_byte = (switch_byte as i64 + relative_offset as i64) as usize;
    offset_map.get(&target_byte).copied()
}
