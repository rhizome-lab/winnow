use serde::{Deserialize, Serialize};

use crate::define_entity;

use super::ty::Type;

define_entity!(ValueId);

/// A compile-time constant.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Constant {
    Null,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    String(String),
}

impl Constant {
    /// Infer the type of this constant.
    pub fn ty(&self) -> Type {
        match self {
            Constant::Null => Type::Option(Box::new(Type::Dynamic)),
            Constant::Bool(_) => Type::Bool,
            Constant::Int(_) => Type::Int(64),
            Constant::UInt(_) => Type::UInt(64),
            Constant::Float(_) => Type::Float(64),
            Constant::String(_) => Type::String,
        }
    }
}
