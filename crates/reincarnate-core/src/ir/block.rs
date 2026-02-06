use serde::{Deserialize, Serialize};

use crate::define_entity;

use super::inst::InstId;
use super::ty::Type;
use super::value::ValueId;

define_entity!(BlockId);

/// A block parameter (replaces phi nodes).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockParam {
    pub value: ValueId,
    pub ty: Type,
}

/// A basic block in the IR.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    pub params: Vec<BlockParam>,
    pub insts: Vec<InstId>,
}
