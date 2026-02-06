use serde::{Deserialize, Serialize};

use crate::entity::PrimaryMap;

use super::func::{FuncId, Function, Visibility};
use super::ty::Type;

/// A struct definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub visibility: Visibility,
}

/// An enum variant.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<Type>,
}

/// An enum definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<EnumVariant>,
    pub visibility: Visibility,
}

/// A global variable.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Global {
    pub name: String,
    pub ty: Type,
    pub visibility: Visibility,
    pub mutable: bool,
}

/// An import from another module.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub alias: Option<String>,
}

/// A module — the top-level compilation unit.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub functions: PrimaryMap<FuncId, Function>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
    pub globals: Vec<Global>,
    pub imports: Vec<Import>,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            functions: PrimaryMap::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            globals: Vec::new(),
            imports: Vec::new(),
        }
    }
}
