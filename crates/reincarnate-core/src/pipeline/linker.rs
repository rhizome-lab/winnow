use std::collections::{HashMap, HashSet};

use crate::error::CoreError;
use crate::ir::func::Visibility;
use crate::ir::Module;

/// Kind of exported symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Struct,
    Enum,
    Global,
}

/// A resolved symbol in the global symbol table.
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub module: String,
    pub kind: SymbolKind,
}

/// Global symbol table built from all modules in a project.
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    /// module_name → {symbol_name → Symbol}
    exports: HashMap<String, HashMap<String, Symbol>>,
}

impl SymbolTable {
    /// Look up a symbol exported from a specific module.
    pub fn resolve(&self, module: &str, name: &str) -> Option<&Symbol> {
        self.exports.get(module).and_then(|m| m.get(name))
    }

    /// Iterate all exported symbols across all modules.
    pub fn all_symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.exports.values().flat_map(|m| m.values())
    }
}

/// Cross-module linker: builds a global symbol table and validates imports.
pub struct Linker;

impl Linker {
    /// Build a symbol table from all modules and validate that every import
    /// resolves to a public symbol. Returns the symbol table on success.
    pub fn link(modules: &[Module]) -> Result<SymbolTable, Vec<CoreError>> {
        let table = Self::build_symbol_table(modules);
        let errors = Self::validate_imports(modules, &table);
        if errors.is_empty() {
            Ok(table)
        } else {
            Err(errors)
        }
    }

    /// Build the global symbol table from all module exports.
    fn build_symbol_table(modules: &[Module]) -> SymbolTable {
        let mut exports: HashMap<String, HashMap<String, Symbol>> = HashMap::new();

        for module in modules {
            let mod_exports = exports.entry(module.name.clone()).or_default();

            for (_id, func) in module.functions.iter() {
                if func.visibility == Visibility::Public {
                    mod_exports.insert(
                        func.name.clone(),
                        Symbol {
                            name: func.name.clone(),
                            module: module.name.clone(),
                            kind: SymbolKind::Function,
                        },
                    );
                }
            }

            for def in &module.structs {
                if def.visibility == Visibility::Public {
                    mod_exports.insert(
                        def.name.clone(),
                        Symbol {
                            name: def.name.clone(),
                            module: module.name.clone(),
                            kind: SymbolKind::Struct,
                        },
                    );
                }
            }

            for def in &module.enums {
                if def.visibility == Visibility::Public {
                    mod_exports.insert(
                        def.name.clone(),
                        Symbol {
                            name: def.name.clone(),
                            module: module.name.clone(),
                            kind: SymbolKind::Enum,
                        },
                    );
                }
            }

            for global in &module.globals {
                if global.visibility == Visibility::Public {
                    mod_exports.insert(
                        global.name.clone(),
                        Symbol {
                            name: global.name.clone(),
                            module: module.name.clone(),
                            kind: SymbolKind::Global,
                        },
                    );
                }
            }
        }

        SymbolTable { exports }
    }

    /// Validate that every import in every module resolves to a public symbol.
    fn validate_imports(modules: &[Module], table: &SymbolTable) -> Vec<CoreError> {
        let module_names: HashSet<&str> = modules.iter().map(|m| m.name.as_str()).collect();
        let mut errors = Vec::new();

        for module in modules {
            for import in &module.imports {
                let resolved = module_names.contains(import.module.as_str())
                    && table.resolve(&import.module, &import.name).is_some();
                if !resolved {
                    errors.push(CoreError::UnresolvedImport {
                        module: import.module.clone(),
                        name: import.name.clone(),
                    });
                }
            }
        }

        errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
    use crate::ir::{FunctionSig, Global, Import, StructDef, Type, Visibility};

    #[test]
    fn link_valid_import() {
        let mut mb_a = ModuleBuilder::new("mod_a");
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("helper", sig, Visibility::Public);
        fb.ret(None);
        mb_a.add_function(fb.build());
        let mod_a = mb_a.build();

        let mut mb_b = ModuleBuilder::new("mod_b");
        mb_b.add_import(Import {
            module: "mod_a".into(),
            name: "helper".into(),
            alias: None,
        });
        let mod_b = mb_b.build();

        let result = Linker::link(&[mod_a, mod_b]);
        assert!(result.is_ok());
        let table = result.unwrap();
        let sym = table.resolve("mod_a", "helper").unwrap();
        assert_eq!(sym.kind, SymbolKind::Function);
    }

    #[test]
    fn link_unresolved_import_missing_symbol() {
        let mb_a = ModuleBuilder::new("mod_a");
        let mod_a = mb_a.build();

        let mut mb_b = ModuleBuilder::new("mod_b");
        mb_b.add_import(Import {
            module: "mod_a".into(),
            name: "nonexistent".into(),
            alias: None,
        });
        let mod_b = mb_b.build();

        let result = Linker::link(&[mod_a, mod_b]);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].to_string().contains("nonexistent"));
    }

    #[test]
    fn link_unresolved_import_missing_module() {
        let mut mb = ModuleBuilder::new("mod_b");
        mb.add_import(Import {
            module: "ghost".into(),
            name: "foo".into(),
            alias: None,
        });
        let mod_b = mb.build();

        let result = Linker::link(&[mod_b]);
        assert!(result.is_err());
        assert!(result.unwrap_err()[0].to_string().contains("ghost"));
    }

    #[test]
    fn link_private_symbol_not_exported() {
        let mut mb_a = ModuleBuilder::new("mod_a");
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("secret", sig, Visibility::Private);
        fb.ret(None);
        mb_a.add_function(fb.build());
        let mod_a = mb_a.build();

        let mut mb_b = ModuleBuilder::new("mod_b");
        mb_b.add_import(Import {
            module: "mod_a".into(),
            name: "secret".into(),
            alias: None,
        });
        let mod_b = mb_b.build();

        let result = Linker::link(&[mod_a, mod_b]);
        assert!(result.is_err());
    }

    #[test]
    fn link_struct_and_global_exports() {
        let mut mb_a = ModuleBuilder::new("mod_a");
        mb_a.add_struct(StructDef {
            name: "Point".into(),
            fields: vec![("x".into(), Type::Float(64)), ("y".into(), Type::Float(64))],
            visibility: Visibility::Public,
        });
        mb_a.add_global(Global {
            name: "ORIGIN".into(),
            ty: Type::Struct("Point".into()),
            visibility: Visibility::Public,
            mutable: false,
        });
        let mod_a = mb_a.build();

        let mut mb_b = ModuleBuilder::new("mod_b");
        mb_b.add_import(Import {
            module: "mod_a".into(),
            name: "Point".into(),
            alias: None,
        });
        mb_b.add_import(Import {
            module: "mod_a".into(),
            name: "ORIGIN".into(),
            alias: None,
        });
        let mod_b = mb_b.build();

        let result = Linker::link(&[mod_a, mod_b]);
        assert!(result.is_ok());
        let table = result.unwrap();
        assert_eq!(table.resolve("mod_a", "Point").unwrap().kind, SymbolKind::Struct);
        assert_eq!(table.resolve("mod_a", "ORIGIN").unwrap().kind, SymbolKind::Global);
    }

    #[test]
    fn link_no_modules() {
        let result = Linker::link(&[]);
        assert!(result.is_ok());
    }

    #[test]
    fn link_no_imports() {
        let mut mb = ModuleBuilder::new("standalone");
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("main", sig, Visibility::Public);
        fb.ret(None);
        mb.add_function(fb.build());
        let module = mb.build();

        let result = Linker::link(&[module]);
        assert!(result.is_ok());
    }

    #[test]
    fn symbol_table_all_symbols() {
        let mut mb = ModuleBuilder::new("mod_a");
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
        };
        let mut fb = FunctionBuilder::new("f1", sig.clone(), Visibility::Public);
        fb.ret(None);
        mb.add_function(fb.build());
        let mut fb2 = FunctionBuilder::new("f2", sig, Visibility::Public);
        fb2.ret(None);
        mb.add_function(fb2.build());
        let mod_a = mb.build();

        let table = Linker::link(&[mod_a]).unwrap();
        let names: HashSet<&str> = table.all_symbols().map(|s| s.name.as_str()).collect();
        assert!(names.contains("f1"));
        assert!(names.contains("f2"));
        assert_eq!(names.len(), 2);
    }
}
