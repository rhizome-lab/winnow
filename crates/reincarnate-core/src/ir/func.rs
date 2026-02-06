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
}

/// A function in the IR.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub sig: FunctionSig,
    pub visibility: Visibility,
    pub blocks: PrimaryMap<BlockId, Block>,
    pub insts: PrimaryMap<InstId, Inst>,
    pub value_types: PrimaryMap<ValueId, Type>,
    /// Entry block — always the first block.
    pub entry: BlockId,
    /// If this function is a coroutine, metadata about it.
    pub coroutine: Option<CoroutineInfo>,
}
