use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::entity::PrimaryMap;

use crate::project::{ExternalMethodSig, ExternalTypeDef};

use super::value::Constant;
use super::func::{FuncId, Function, Visibility};
use super::ty::Type;

/// Describes how the application is started.
///
/// Engine-agnostic: each frontend maps its own entry mechanism to the
/// appropriate variant.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EntryPoint {
    /// Construct this class to start the application.
    /// Flash document class, Java Applet, RPG Maker Scene_Boot, etc.
    ConstructClass(String),
    /// Call this function to start the application.
    /// VB6 Sub Main, Director startMovie, Ren'Py label start, etc.
    CallFunction(FuncId),
}

/// A struct definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDef {
    pub name: String,
    #[serde(default)]
    pub namespace: Vec<String>,
    pub fields: Vec<(String, Type, Option<Constant>)>,
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
    /// Optional compile-time default value (from script trait Slot/Const).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub init: Option<Constant>,
}

/// An import from another module.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Import {
    pub module: String,
    pub name: String,
    pub alias: Option<String>,
}

/// An import of an external runtime type (e.g. a Flash stdlib class).
///
/// Populated by frontends so the backend can emit import statements without
/// engine-specific namespace parsing.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExternalImport {
    /// Short name used in generated code (e.g. `"MovieClip"`).
    pub short_name: String,
    /// Module path relative to the runtime directory root
    /// (e.g. `"flash/display"`, `"flash/runtime"`).
    pub module_path: String,
}

/// Groups a struct (fields) with its methods into a class.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassDef {
    /// Short class name (e.g. `"Phouka"`).
    pub name: String,
    /// Namespace segments (e.g. `["classes", "Scenes", "Areas", "Bog"]`).
    pub namespace: Vec<String>,
    /// Index into `Module::structs`.
    pub struct_index: usize,
    /// Method `FuncId`s belonging to this class.
    pub methods: Vec<FuncId>,
    /// Superclass qualified name, if any.
    pub super_class: Option<String>,
    pub visibility: Visibility,
    /// Static (class-level) fields from Slot/Const traits on the Class object.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub static_fields: Vec<(String, Type, Option<Constant>)>,
    /// Whether this class is an interface (emitted as `abstract class`).
    #[serde(default)]
    pub is_interface: bool,
    /// Interfaces implemented by this class (short names).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub interfaces: Vec<String>,
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
    #[serde(default)]
    pub classes: Vec<ClassDef>,
    /// How to start the application (set by frontends that know the answer).
    #[serde(default)]
    pub entry_point: Option<EntryPoint>,
    /// External runtime imports, keyed by qualified name (e.g.
    /// `"flash.display::MovieClip"`).  Populated by frontends so the backend
    /// can emit import statements without engine-specific parsing.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub external_imports: BTreeMap<String, ExternalImport>,
    /// External type definitions from the runtime package.
    /// Populated by the CLI before running transforms so that type inference
    /// and constraint solving can resolve fields/methods on external types.
    /// Skipped during serialization to avoid bloating IR JSON output.
    #[serde(default, skip_serializing)]
    pub external_type_defs: BTreeMap<String, ExternalTypeDef>,
    /// External function signatures from the runtime package.
    /// Maps function name → signature for free functions (not methods on types).
    /// Used by type inference and constraint solving to infer return types.
    #[serde(default, skip_serializing)]
    pub external_function_sigs: BTreeMap<String, ExternalMethodSig>,
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
            classes: Vec::new(),
            entry_point: None,
            external_imports: BTreeMap::new(),
            external_type_defs: BTreeMap::new(),
            external_function_sigs: BTreeMap::new(),
        }
    }
}
