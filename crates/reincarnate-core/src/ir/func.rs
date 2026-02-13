use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

use crate::define_entity;
use crate::entity::PrimaryMap;

use super::block::{Block, BlockId};
use super::coroutine::CoroutineInfo;
use super::inst::{Inst, InstId};
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
}
