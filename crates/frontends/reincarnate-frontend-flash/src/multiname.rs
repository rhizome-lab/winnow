//! Multiname and namespace resolution from AVM2 constant pools.

use swf::avm2::types::{ConstantPool, Index, Multiname, Namespace};

/// A resolved name from an AVM2 multiname.
#[derive(Debug, Clone)]
pub enum ResolvedName {
    /// Fully resolved at compile time: `"ns::name"` or just `"name"`.
    Static(String),
    /// Name is known but namespace comes from the runtime stack.
    RuntimeQualified(String),
    /// Both name and namespace come from the runtime stack.
    RuntimeLate,
}

/// Look up a string in the constant pool by index.
pub fn pool_string(pool: &ConstantPool, index: &Index<String>) -> String {
    let i = index.0 as usize;
    if i == 0 || i > pool.strings.len() {
        return String::new();
    }
    String::from_utf8_lossy(&pool.strings[i - 1]).into_owned()
}

/// Resolve a namespace to its string representation.
pub fn resolve_namespace(pool: &ConstantPool, ns: &Namespace) -> String {
    let idx = match ns {
        Namespace::Namespace(i)
        | Namespace::Package(i)
        | Namespace::PackageInternal(i)
        | Namespace::Protected(i)
        | Namespace::Explicit(i)
        | Namespace::StaticProtected(i)
        | Namespace::Private(i) => i,
    };
    pool_string(pool, idx)
}

/// Look up a namespace from the constant pool by index.
fn pool_namespace<'a>(pool: &'a ConstantPool, index: &Index<Namespace>) -> Option<&'a Namespace> {
    let i = index.0 as usize;
    if i == 0 || i > pool.namespaces.len() {
        return None;
    }
    Some(&pool.namespaces[i - 1])
}

/// Look up a multiname from the constant pool by index.
pub fn pool_multiname<'a>(pool: &'a ConstantPool, index: &Index<Multiname>) -> Option<&'a Multiname> {
    let i = index.0 as usize;
    if i == 0 || i > pool.multinames.len() {
        return None;
    }
    Some(&pool.multinames[i - 1])
}

/// Resolve a multiname to a string suitable for IR names.
pub fn resolve_multiname(pool: &ConstantPool, mn: &Multiname) -> ResolvedName {
    match mn {
        Multiname::QName { namespace, name } | Multiname::QNameA { namespace, name } => {
            let name_str = pool_string(pool, name);
            if let Some(ns) = pool_namespace(pool, namespace) {
                let ns_str = resolve_namespace(pool, ns);
                if ns_str.is_empty() {
                    ResolvedName::Static(name_str)
                } else {
                    ResolvedName::Static(format!("{ns_str}::{name_str}"))
                }
            } else {
                ResolvedName::Static(name_str)
            }
        }
        Multiname::RTQName { name } | Multiname::RTQNameA { name } => {
            ResolvedName::RuntimeQualified(pool_string(pool, name))
        }
        Multiname::RTQNameL | Multiname::RTQNameLA => ResolvedName::RuntimeLate,
        Multiname::Multiname {
            namespace_set,
            name,
        }
        | Multiname::MultinameA {
            namespace_set,
            name,
        } => {
            let name_str = pool_string(pool, name);
            let i = namespace_set.0 as usize;
            if i > 0 && i <= pool.namespace_sets.len() {
                let ns_set = &pool.namespace_sets[i - 1];
                if let Some(first_ns_idx) = ns_set.first() {
                    if let Some(ns) = pool_namespace(pool, first_ns_idx) {
                        let ns_str = resolve_namespace(pool, ns);
                        if !ns_str.is_empty() {
                            return ResolvedName::Static(format!("{ns_str}::{name_str}"));
                        }
                    }
                }
            }
            ResolvedName::Static(name_str)
        }
        Multiname::MultinameL { .. } | Multiname::MultinameLA { .. } => ResolvedName::RuntimeLate,
        Multiname::TypeName {
            base_type,
            parameters,
        } => {
            let base = if let Some(base_mn) = pool_multiname(pool, base_type) {
                match resolve_multiname(pool, base_mn) {
                    ResolvedName::Static(s) => s,
                    _ => "?".to_string(),
                }
            } else {
                "?".to_string()
            };
            let params: Vec<String> = parameters
                .iter()
                .map(|p| {
                    if let Some(pmn) = pool_multiname(pool, p) {
                        match resolve_multiname(pool, pmn) {
                            ResolvedName::Static(s) => s,
                            _ => "?".to_string(),
                        }
                    } else {
                        "?".to_string()
                    }
                })
                .collect();
            ResolvedName::Static(format!("{}.<{}>", base, params.join(", ")))
        }
    }
}

/// Resolve a multiname index to a static string, falling back to `"?"`.
pub fn resolve_multiname_index(pool: &ConstantPool, index: &Index<Multiname>) -> String {
    if let Some(mn) = pool_multiname(pool, index) {
        match resolve_multiname(pool, mn) {
            ResolvedName::Static(s) => s,
            ResolvedName::RuntimeQualified(s) => format!("rt:{s}"),
            ResolvedName::RuntimeLate => "rt:?".to_string(),
        }
    } else {
        "*".to_string()
    }
}

/// Resolve a multiname index to an IR `Type`.
pub fn resolve_type(pool: &ConstantPool, index: &Index<Multiname>) -> reincarnate_core::ir::Type {
    use reincarnate_core::ir::Type;

    if index.0 == 0 {
        return Type::Dynamic;
    }

    let name = resolve_multiname_index(pool, index);
    match name.as_str() {
        "int" => Type::Int(32),
        "uint" => Type::UInt(32),
        "Number" => Type::Float(64),
        "Boolean" => Type::Bool,
        "String" => Type::String,
        "void" => Type::Void,
        "*" | "Object" => Type::Dynamic,
        "Array" => Type::Array(Box::new(Type::Dynamic)),
        _ => Type::Struct(name),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use swf::avm2::types::{ConstantPool, Index, Multiname, Namespace};

    fn empty_pool() -> ConstantPool {
        ConstantPool {
            ints: vec![],
            uints: vec![],
            doubles: vec![],
            strings: vec![],
            namespaces: vec![],
            namespace_sets: vec![],
            multinames: vec![],
        }
    }

    #[test]
    fn resolve_qname_public() {
        let mut pool = empty_pool();
        pool.strings.push(b"".to_vec()); // index 1: empty namespace
        pool.strings.push(b"myFunc".to_vec()); // index 2
        pool.namespaces
            .push(Namespace::Package(Index::new(1))); // index 1

        let mn = Multiname::QName {
            namespace: Index::new(1),
            name: Index::new(2),
        };

        match resolve_multiname(&pool, &mn) {
            ResolvedName::Static(s) => assert_eq!(s, "myFunc"),
            other => panic!("expected Static, got {other:?}"),
        }
    }

    #[test]
    fn resolve_qname_with_namespace() {
        let mut pool = empty_pool();
        pool.strings.push(b"flash.display".to_vec()); // index 1
        pool.strings.push(b"Sprite".to_vec()); // index 2
        pool.namespaces
            .push(Namespace::Package(Index::new(1))); // index 1

        let mn = Multiname::QName {
            namespace: Index::new(1),
            name: Index::new(2),
        };

        match resolve_multiname(&pool, &mn) {
            ResolvedName::Static(s) => assert_eq!(s, "flash.display::Sprite"),
            other => panic!("expected Static, got {other:?}"),
        }
    }

    #[test]
    fn resolve_rtqname() {
        let mut pool = empty_pool();
        pool.strings.push(b"dynProp".to_vec()); // index 1

        let mn = Multiname::RTQName {
            name: Index::new(1),
        };

        match resolve_multiname(&pool, &mn) {
            ResolvedName::RuntimeQualified(s) => assert_eq!(s, "dynProp"),
            other => panic!("expected RuntimeQualified, got {other:?}"),
        }
    }

    #[test]
    fn resolve_type_mapping() {
        use reincarnate_core::ir::Type;

        let mut pool = empty_pool();
        pool.strings.push(b"int".to_vec()); // 1
        pool.strings.push(b"Number".to_vec()); // 2
        pool.strings.push(b"Boolean".to_vec()); // 3
        pool.strings.push(b"String".to_vec()); // 4
        pool.strings.push(b"void".to_vec()); // 5
        pool.strings.push(b"MyClass".to_vec()); // 6

        // QNames with empty namespace (public)
        pool.namespaces
            .push(Namespace::Package(Index::new(0))); // ns 1 with empty string

        pool.multinames.push(Multiname::QName {
            namespace: Index::new(1),
            name: Index::new(1),
        }); // mn 1 = int
        pool.multinames.push(Multiname::QName {
            namespace: Index::new(1),
            name: Index::new(2),
        }); // mn 2 = Number
        pool.multinames.push(Multiname::QName {
            namespace: Index::new(1),
            name: Index::new(3),
        }); // mn 3 = Boolean
        pool.multinames.push(Multiname::QName {
            namespace: Index::new(1),
            name: Index::new(4),
        }); // mn 4 = String
        pool.multinames.push(Multiname::QName {
            namespace: Index::new(1),
            name: Index::new(5),
        }); // mn 5 = void
        pool.multinames.push(Multiname::QName {
            namespace: Index::new(1),
            name: Index::new(6),
        }); // mn 6 = MyClass

        assert_eq!(resolve_type(&pool, &Index::new(0)), Type::Dynamic); // index 0 = *
        assert_eq!(resolve_type(&pool, &Index::new(1)), Type::Int(32));
        assert_eq!(resolve_type(&pool, &Index::new(2)), Type::Float(64));
        assert_eq!(resolve_type(&pool, &Index::new(3)), Type::Bool);
        assert_eq!(resolve_type(&pool, &Index::new(4)), Type::String);
        assert_eq!(resolve_type(&pool, &Index::new(5)), Type::Void);
        assert_eq!(
            resolve_type(&pool, &Index::new(6)),
            Type::Struct("MyClass".to_string())
        );
    }
}
