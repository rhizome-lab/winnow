use serde::{Deserialize, Serialize};

/// Trait for entity references — typed `u32` indices into arenas.
pub trait EntityRef: Copy + Eq + std::hash::Hash + std::fmt::Debug {
    fn new(index: u32) -> Self;
    fn index(self) -> u32;
}

/// Define a typed entity reference (a newtype over `u32`).
///
/// ```ignore
/// define_entity!(FuncId);
/// ```
#[macro_export]
macro_rules! define_entity {
    ($name:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, serde::Serialize, serde::Deserialize)]
        pub struct $name(u32);

        impl $crate::entity::EntityRef for $name {
            fn new(index: u32) -> Self {
                Self(index)
            }
            fn index(self) -> u32 {
                self.0
            }
        }
    };
}

/// Indexed arena: append-only storage keyed by entity references.
///
/// Serializes as a plain `Vec<V>` — the key type is phantom.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct PrimaryMap<K: EntityRef, V> {
    elems: Vec<V>,
    #[serde(skip)]
    _phantom: std::marker::PhantomData<K>,
}

impl<K: EntityRef, V> Default for PrimaryMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: EntityRef, V> PrimaryMap<K, V> {
    pub fn new() -> Self {
        Self {
            elems: Vec::new(),
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn push(&mut self, value: V) -> K {
        let key = K::new(self.elems.len() as u32);
        self.elems.push(value);
        key
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.elems.get(key.index() as usize)
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.elems.get_mut(key.index() as usize)
    }

    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        self.elems
            .iter()
            .enumerate()
            .map(|(i, v)| (K::new(i as u32), v))
    }

    pub fn keys(&self) -> impl Iterator<Item = K> {
        (0..self.elems.len() as u32).map(K::new)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.elems.iter()
    }
}

impl<K: EntityRef, V> std::ops::Index<K> for PrimaryMap<K, V> {
    type Output = V;
    fn index(&self, key: K) -> &V {
        &self.elems[key.index() as usize]
    }
}

impl<K: EntityRef, V> std::ops::IndexMut<K> for PrimaryMap<K, V> {
    fn index_mut(&mut self, key: K) -> &mut V {
        &mut self.elems[key.index() as usize]
    }
}

/// Sparse secondary storage keyed by entity references.
/// Unlike `PrimaryMap`, entries can be absent.
#[derive(Debug, Clone)]
pub struct SecondaryMap<K: EntityRef, V> {
    elems: Vec<Option<V>>,
    _phantom: std::marker::PhantomData<K>,
}

impl<K: EntityRef, V> Default for SecondaryMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: EntityRef, V> SecondaryMap<K, V> {
    pub fn new() -> Self {
        Self {
            elems: Vec::new(),
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        let idx = key.index() as usize;
        if idx >= self.elems.len() {
            self.elems.resize_with(idx + 1, || None);
        }
        self.elems[idx] = Some(value);
    }

    pub fn get(&self, key: K) -> Option<&V> {
        self.elems
            .get(key.index() as usize)
            .and_then(|v| v.as_ref())
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.elems
            .get_mut(key.index() as usize)
            .and_then(|v| v.as_mut())
    }

    pub fn remove(&mut self, key: K) -> Option<V> {
        let idx = key.index() as usize;
        if idx < self.elems.len() {
            self.elems[idx].take()
        } else {
            None
        }
    }

    pub fn contains_key(&self, key: K) -> bool {
        self.get(key).is_some()
    }
}
