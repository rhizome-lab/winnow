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

/// AVM2 namespace kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NsKind {
    Package,
    PackageInternal,
    Protected,
    Private,
    Explicit,
    StaticProtected,
    Namespace,
}

/// A fully resolved qualified name with structured namespace info.
#[derive(Debug, Clone)]
pub struct QualifiedName {
    /// Package segments split by `.` (e.g. `["flash", "display"]`).
    pub namespace: Vec<String>,
    /// Bare name (e.g. `"Sprite"`).
    pub name: String,
    /// What kind of AVM2 namespace this came from.
    pub ns_kind: NsKind,
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

/// Get the `NsKind` of an AVM2 namespace.
pub fn namespace_kind(ns: &Namespace) -> NsKind {
    match ns {
        Namespace::Package(_) => NsKind::Package,
        Namespace::PackageInternal(_) => NsKind::PackageInternal,
        Namespace::Protected(_) => NsKind::Protected,
        Namespace::Private(_) => NsKind::Private,
        Namespace::Explicit(_) => NsKind::Explicit,
        Namespace::StaticProtected(_) => NsKind::StaticProtected,
        Namespace::Namespace(_) => NsKind::Namespace,
    }
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

/// How a multiname's runtime components should be handled.
#[derive(Debug, Clone)]
pub enum MultinameKind {
    /// Fully resolved at compile time — no extra stack pops.
    Named(String),
    /// Name is known but a namespace was popped from the runtime stack (discard it).
    RuntimeNs(String),
    /// The property name/index comes from the runtime stack (pop 1).
    RuntimeName,
    /// Both name and namespace come from the runtime stack (pop 2: name first, then namespace).
    RuntimeBoth,
}

/// Classify a multiname for property access: how many values to pop from the
/// runtime stack, and whether the access is named or indexed.
pub fn classify_multiname(pool: &ConstantPool, index: &Index<Multiname>) -> MultinameKind {
    let Some(mn) = pool_multiname(pool, index) else {
        return MultinameKind::Named("*".to_string());
    };
    match mn {
        Multiname::QName { .. }
        | Multiname::QNameA { .. }
        | Multiname::Multiname { .. }
        | Multiname::MultinameA { .. }
        | Multiname::TypeName { .. } => {
            MultinameKind::Named(resolve_multiname_index(pool, index))
        }
        Multiname::RTQName { name } | Multiname::RTQNameA { name } => {
            MultinameKind::RuntimeNs(pool_string(pool, name))
        }
        Multiname::RTQNameL | Multiname::RTQNameLA => MultinameKind::RuntimeBoth,
        Multiname::MultinameL { .. } | Multiname::MultinameLA { .. } => {
            MultinameKind::RuntimeName
        }
    }
}

/// Resolve a multiname index to a structured `QualifiedName`.
///
/// Returns `None` for runtime-late or unresolvable names.
pub fn resolve_multiname_structured(
    pool: &ConstantPool,
    index: &Index<Multiname>,
) -> Option<QualifiedName> {
    let mn = pool_multiname(pool, index)?;
    match mn {
        Multiname::QName { namespace, name } | Multiname::QNameA { namespace, name } => {
            let name_str = pool_string(pool, name);
            let (ns_str, ns_kind) = if let Some(ns) = pool_namespace(pool, namespace) {
                (resolve_namespace(pool, ns), namespace_kind(ns))
            } else {
                (String::new(), NsKind::Package)
            };
            let namespace_segments = if ns_str.is_empty() {
                Vec::new()
            } else {
                ns_str.split('.').map(String::from).collect()
            };
            Some(QualifiedName {
                namespace: namespace_segments,
                name: name_str,
                ns_kind,
            })
        }
        _ => None,
    }
}

/// Convert a resolved class/type name string to an IR `Type`.
///
/// This is the core name→type mapping shared by `resolve_type` (multiname index)
/// and runtime type resolution (e.g. `IsTypeLate` where the type comes from the stack).
pub fn type_from_name(name: &str) -> reincarnate_core::ir::Type {
    use reincarnate_core::ir::Type;
    match name {
        "int" => Type::Int(32),
        "uint" => Type::UInt(32),
        "Number" => Type::Float(64),
        "Boolean" => Type::Bool,
        "String" => Type::String,
        "void" => Type::Void,
        "*" | "Object" => Type::Dynamic,
        "Array" => Type::Array(Box::new(Type::Dynamic)),
        "flash.utils::Dictionary" | "Dictionary" => {
            Type::Map(Box::new(Type::Dynamic), Box::new(Type::Dynamic))
        }
        _ => Type::Struct(name.to_string()),
    }
}

/// Resolve a multiname index to an IR `Type`.
pub fn resolve_type(pool: &ConstantPool, index: &Index<Multiname>) -> reincarnate_core::ir::Type {
    use reincarnate_core::ir::Type;

    if index.0 == 0 {
        return Type::Dynamic;
    }

    // Inspect the raw multiname for generic types (Vector.<T>) before
    // flattening to a string.
    if let Some(Multiname::TypeName {
        base_type,
        parameters,
    }) = pool_multiname(pool, index)
    {
        let base_name = resolve_multiname_index(pool, base_type);
        if base_name == "__AS3__.vec::Vector" || base_name == "Vector" {
            let elem = parameters
                .first()
                .map(|p| resolve_type(pool, p))
                .unwrap_or(Type::Dynamic);
            return Type::Array(Box::new(elem));
        }
    }

    let name = resolve_multiname_index(pool, index);
    type_from_name(&name)
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
