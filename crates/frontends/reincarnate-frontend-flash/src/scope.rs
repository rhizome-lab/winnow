//! Scope chain tracking for AVM2 translation.
//!
//! AVM2 maintains a scope stack parallel to the operand stack.
//! `PushScope` and `PushWith` add entries; `PopScope` removes them.
//! `GetScopeObject`, `FindProperty`, and `FindPropStrict` read from it.

use reincarnate_core::ir::ValueId;

/// Tracks the scope chain during bytecode translation.
#[derive(Debug, Clone, Default)]
pub struct ScopeStack {
    values: Vec<ValueId>,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, value: ValueId) {
        self.values.push(value);
    }

    pub fn pop(&mut self) -> Option<ValueId> {
        self.values.pop()
    }

    pub fn get(&self, index: usize) -> Option<ValueId> {
        self.values.get(index).copied()
    }

    pub fn depth(&self) -> usize {
        self.values.len()
    }

    pub fn values(&self) -> &[ValueId] {
        &self.values
    }

    pub fn clear(&mut self) {
        self.values.clear();
    }
}
