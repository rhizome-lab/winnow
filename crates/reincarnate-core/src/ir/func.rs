use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

use crate::define_entity;
use crate::entity::PrimaryMap;

use super::block::{Block, BlockId};
use super::coroutine::CoroutineInfo;
use super::inst::{Inst, InstId, Op};
use super::ty::{FunctionSig, Type};
use super::value::ValueId;

define_entity!(FuncId);

/// Visibility of a function or global.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Private,
    Protected,
}

/// How a variable is captured in a closure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CaptureMode {
    /// Snapshot the value at closure-creation time.
    ByValue,
    /// Capture by reference (mutable binding shared with the outer scope).
    ByRef,
}

/// A capture parameter declared on a closure function.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CaptureParam {
    pub name: String,
    pub ty: Type,
    pub mode: CaptureMode,
}

/// What kind of method a function represents.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum MethodKind {
    #[default]
    Free,
    Constructor,
    Instance,
    Static,
    Getter,
    Setter,
    Closure,
}

/// A function in the IR.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub sig: FunctionSig,
    pub visibility: Visibility,
    /// Namespace segments (e.g. `["classes", "Scenes", "Areas", "Bog"]`).
    #[serde(default)]
    pub namespace: Vec<String>,
    /// Owning class short name (e.g. `"Phouka"`).
    #[serde(default)]
    pub class: Option<String>,
    /// What kind of method this function represents.
    #[serde(default)]
    pub method_kind: MethodKind,
    pub blocks: PrimaryMap<BlockId, Block>,
    pub insts: PrimaryMap<InstId, Inst>,
    pub value_types: PrimaryMap<ValueId, Type>,
    /// Entry block — always the first block.
    pub entry: BlockId,
    /// If this function is a coroutine, metadata about it.
    pub coroutine: Option<CoroutineInfo>,
    /// Optional debug names for values (from source-level variable names).
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub value_names: HashMap<ValueId, String>,
    /// Capture parameters for closure functions (empty for non-closures).
    ///
    /// In the entry block, capture params are appended after the regular
    /// `sig.params`. Use [`FunctionBuilder::capture_param`] to access them.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub capture_params: Vec<CaptureParam>,
}

impl Function {
    /// Remove dead instructions from the arena.
    ///
    /// After transforms like Mem2Reg and DCE, instructions removed from blocks
    /// remain in the `insts` arena. This compacts the arena so only live
    /// instructions remain, allowing downstream consumers to safely iterate it.
    pub fn compact_insts(&mut self) {
        let mut live: HashSet<InstId> = HashSet::new();
        for block in self.blocks.values() {
            for &inst_id in &block.insts {
                live.insert(inst_id);
            }
        }

        if live.len() == self.insts.len() {
            return;
        }

        let mut new_insts = PrimaryMap::new();
        let mut remap: HashMap<InstId, InstId> = HashMap::new();
        for (old_id, inst) in self.insts.iter() {
            if live.contains(&old_id) {
                let new_id = new_insts.push(inst.clone());
                remap.insert(old_id, new_id);
            }
        }

        for block in self.blocks.values_mut() {
            for inst_id in &mut block.insts {
                *inst_id = remap[inst_id];
            }
        }

        self.insts = new_insts;
    }

    /// Move all `Op::Alloc` instructions to the entry block.
    ///
    /// In most supported source languages (GML, Harlowe, Flash AS3), variable
    /// declarations have function scope — a variable set inside an `if` branch
    /// is visible for the rest of the function. But when a frontend emits
    /// `Op::Alloc` inside a nested block (e.g. inside an if-branch), the
    /// emitted TypeScript `let _x` is block-scoped, causing TS2304 errors for
    /// references outside that block.
    ///
    /// This pass corrects that by moving every `Op::Alloc` to the front of
    /// the entry block, where it emits as a function-level declaration.
    /// Downstream passes (Mem2Reg, structurizer, emitter) are unaffected:
    /// Mem2Reg only cares about Alloc/Store/Load relationships, not position,
    /// and moving an Alloc earlier never breaks data-flow since it produces a
    /// slot rather than consuming a value.
    pub fn hoist_allocs(&mut self) {
        let entry = self.entry;
        let block_ids: Vec<BlockId> = self.blocks.keys().collect();
        let mut allocs_to_hoist: Vec<InstId> = vec![];

        for bid in block_ids {
            if bid == entry {
                continue;
            }
            // Collect alloc positions from this block (in order).
            let positions: Vec<usize> = self.blocks[bid]
                .insts
                .iter()
                .enumerate()
                .filter_map(|(i, &iid)| {
                    matches!(self.insts[iid].op, Op::Alloc(_)).then_some(i)
                })
                .collect();
            // Remove in reverse to preserve indices.
            for pos in positions.into_iter().rev() {
                allocs_to_hoist.push(self.blocks[bid].insts.remove(pos));
            }
        }

        if allocs_to_hoist.is_empty() {
            return;
        }

        // allocs_to_hoist is reverse-order (last block's last alloc first).
        allocs_to_hoist.reverse();

        // Prepend all hoisted allocs to the entry block.
        let entry_insts = &mut self.blocks[entry].insts;
        let existing = std::mem::take(entry_insts);
        entry_insts.extend(allocs_to_hoist);
        entry_insts.extend(existing);
    }
}
