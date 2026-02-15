use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

/// Describes an engine-specific runtime package.
///
/// Loaded from `runtime.json` inside the runtime source directory.
/// The backend uses this to generate imports and scaffold code
/// without hardcoding engine-specific paths.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuntimeConfig {
    /// System call name → module path mapping for import generation.
    /// e.g. `"Flash.Object"` → `{ path: "flash/object" }`
    pub system_modules: BTreeMap<String, SystemModule>,
    /// Scaffold configuration for the main entry point file.
    pub scaffold: ScaffoldConfig,
    /// Per-class-file imports (e.g. class registration helpers).
    /// When `None`, no class preamble is emitted.
    #[serde(default)]
    pub class_preamble: Option<ImportGroup>,
    /// External type definitions (runtime-provided classes/interfaces).
    /// Keyed by short type name (e.g. `"DisplayObject"`).
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub type_definitions: BTreeMap<String, ExternalTypeDef>,
    /// External function signatures (free functions, not methods on types).
    /// Keyed by function name (e.g. `"irandom_range"`).
    /// Used by transforms to infer return types for calls to runtime-provided
    /// functions that aren't methods on any class.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub function_signatures: BTreeMap<String, ExternalMethodSig>,
    /// Per-module export lists for import validation.
    /// Maps module path (e.g. `"flash/display"`) to the names it exports.
    /// Used to warn when generated imports reference names a module doesn't export.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub module_exports: BTreeMap<String, Vec<String>>,
    /// Maps runtime-provided free functions to their module paths.
    /// Used by the emitter to generate import statements for `Call` ops
    /// that reference runtime stdlib functions (e.g. `floor` → `gamemaker/math`).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub function_modules: Vec<ImportGroup>,
}

/// A single system module mapping.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemModule {
    /// Submodule path relative to runtime root (e.g. `"flash/object"`).
    pub path: String,
}

/// Configuration for the generated entry point (`main.ts`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScaffoldConfig {
    /// Additional imports for the entry point file.
    #[serde(default)]
    pub imports: Vec<ImportGroup>,
    /// Code after `new ClassName()` for ConstructClass entries
    /// (e.g. `"stage.addChild(app);"`).
    #[serde(default)]
    pub construct_class_init: Option<String>,
    /// Wrapper function for ConstructClass entries.
    /// When set, emits `const app = fn(ClassName);` instead of
    /// `const app = new ClassName();`.
    /// The function is responsible for construction and any post-init
    /// (e.g. adding to the display list).
    #[serde(default)]
    pub construct_class_fn: Option<String>,
    /// Code called each frame in the game loop (e.g. `"flashTick();"`).
    #[serde(default)]
    pub tick: Option<String>,
    /// Custom entry code replacing the standard rAF loop.
    /// When set, this template is emitted instead of the timing/rAF loop.
    /// Supports `{classes}` placeholder for an array of all emitted class names.
    #[serde(default)]
    pub entry: Option<String>,
    /// Additional data file imports for the entry point.
    /// These are emitted after runtime imports (e.g. data/rooms, data/sprites).
    #[serde(default)]
    pub data_imports: Vec<ImportGroup>,
    /// npm dependencies to include in the generated package.json.
    /// e.g. `{ "jquery": "^3.7.0" }`.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub dependencies: BTreeMap<String, String>,
}

/// A group of named imports from a single path.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportGroup {
    pub names: Vec<String>,
    pub path: String,
}

/// Describes an external (runtime-provided) type's members.
///
/// Used by transforms to continue type inference through external types
/// (e.g. `DisplayObject.visible → Bool`) and by the backend to validate
/// member accesses against known runtime APIs.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExternalTypeDef {
    /// Parent type name, if any (e.g. `"EventDispatcher"` for `DisplayObject`).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extends: Option<String>,
    /// When true, instances may have arbitrary dynamic fields beyond those
    /// listed in `fields`/`methods`. Suppresses "has no member" warnings.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub open: bool,
    /// Instance fields: field name → type notation string.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub fields: BTreeMap<String, String>,
    /// Instance methods: method name → signature.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub methods: BTreeMap<String, ExternalMethodSig>,
}

/// Signature of an external method.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExternalMethodSig {
    /// Parameter types as type notation strings.
    #[serde(default)]
    pub params: Vec<String>,
    /// Return type as a type notation string.
    pub returns: String,
}
