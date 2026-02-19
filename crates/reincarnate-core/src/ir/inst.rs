use serde::{Deserialize, Serialize};

use crate::define_entity;

use super::block::BlockId;
use super::ty::Type;
use super::value::{Constant, ValueId};

define_entity!(InstId);

/// Source location span for diagnostics.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub file: String,
    pub line: u32,
    pub col: u32,
}

/// An IR instruction: an operation with an optional result value.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Inst {
    pub op: Op,
    /// The value produced by this instruction, if any.
    pub result: Option<ValueId>,
    /// Source location for diagnostics.
    pub span: Option<Span>,
}

/// Distinguishes the two semantics of `Op::Cast`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CastKind {
    /// AS3 `as` operator: type-check-or-null.
    AsType,
    /// Runtime coercion (Coerce/Convert opcodes).
    Coerce,
}

/// Comparison kind for relational operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CmpKind {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    /// JavaScript loose equality (`==`). Used by SugarCube where expressions
    /// are raw JS and the author chose `==` over `===`.
    LooseEq,
    /// JavaScript loose inequality (`!=`).
    LooseNe,
}

impl CmpKind {
    /// Return the inverse comparison (e.g. Lt ↔ Ge, Eq ↔ Ne).
    pub fn inverse(self) -> Self {
        match self {
            CmpKind::Eq => CmpKind::Ne,
            CmpKind::Ne => CmpKind::Eq,
            CmpKind::Lt => CmpKind::Ge,
            CmpKind::Ge => CmpKind::Lt,
            CmpKind::Gt => CmpKind::Le,
            CmpKind::Le => CmpKind::Gt,
            CmpKind::LooseEq => CmpKind::LooseNe,
            CmpKind::LooseNe => CmpKind::LooseEq,
        }
    }
}

/// IR operations.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Op {
    // -- Constants --
    /// Load a compile-time constant.
    Const(Constant),

    // -- Arithmetic --
    Add(ValueId, ValueId),
    Sub(ValueId, ValueId),
    Mul(ValueId, ValueId),
    Div(ValueId, ValueId),
    Rem(ValueId, ValueId),
    Neg(ValueId),

    // -- Bitwise --
    BitAnd(ValueId, ValueId),
    BitOr(ValueId, ValueId),
    BitXor(ValueId, ValueId),
    BitNot(ValueId),
    Shl(ValueId, ValueId),
    Shr(ValueId, ValueId),

    // -- Comparison --
    Cmp(CmpKind, ValueId, ValueId),

    // -- Logic --
    Not(ValueId),
    /// Conditional select: `cond ? on_true : on_false`
    Select {
        cond: ValueId,
        on_true: ValueId,
        on_false: ValueId,
    },

    // -- Control flow --
    /// Unconditional branch.
    Br {
        target: BlockId,
        args: Vec<ValueId>,
    },
    /// Conditional branch.
    BrIf {
        cond: ValueId,
        then_target: BlockId,
        then_args: Vec<ValueId>,
        else_target: BlockId,
        else_args: Vec<ValueId>,
    },
    /// Multi-way switch.
    Switch {
        value: ValueId,
        cases: Vec<(Constant, BlockId, Vec<ValueId>)>,
        default: (BlockId, Vec<ValueId>),
    },
    /// Return from function.
    Return(Option<ValueId>),

    // -- Memory / fields --
    /// Allocate a local variable.
    Alloc(Type),
    /// Load from a pointer/reference.
    Load(ValueId),
    /// Store to a pointer/reference.
    Store {
        ptr: ValueId,
        value: ValueId,
    },
    /// Get a field from a struct/object.
    GetField {
        object: ValueId,
        field: String,
    },
    /// Set a field on a struct/object.
    SetField {
        object: ValueId,
        field: String,
        value: ValueId,
    },
    /// Get an element from an array/map by index/key.
    GetIndex {
        collection: ValueId,
        index: ValueId,
    },
    /// Set an element in an array/map by index/key.
    SetIndex {
        collection: ValueId,
        index: ValueId,
        value: ValueId,
    },

    // -- Calls --
    /// Direct function call.
    Call {
        func: String,
        args: Vec<ValueId>,
    },
    /// Create a closure: packages a function with captured outer-scope values.
    /// `captures` are bound to the function's capture params (in declaration order).
    MakeClosure {
        func: String,
        captures: Vec<ValueId>,
    },
    /// Indirect call through a value (function pointer / closure).
    CallIndirect {
        callee: ValueId,
        args: Vec<ValueId>,
    },
    /// System trait method call — string-based, resolved at codegen.
    SystemCall {
        system: String,
        method: String,
        args: Vec<ValueId>,
    },
    /// Method call on a receiver: `receiver.method(args...)`.
    MethodCall {
        receiver: ValueId,
        method: String,
        args: Vec<ValueId>,
    },

    // -- Type operations --
    /// Cast a value to a type.
    Cast(ValueId, Type, CastKind),
    /// Runtime type check (returns bool).
    TypeCheck(ValueId, Type),

    // -- Aggregate construction --
    /// Construct a struct.
    StructInit {
        name: String,
        fields: Vec<(String, ValueId)>,
    },
    /// Construct an array.
    ArrayInit(Vec<ValueId>),
    /// Construct a tuple.
    TupleInit(Vec<ValueId>),

    // -- Coroutines --
    /// Yield a value from a coroutine.
    Yield(Option<ValueId>),
    /// Create a coroutine from a function reference.
    CoroutineCreate {
        func: String,
        args: Vec<ValueId>,
    },
    /// Resume a coroutine, returning the yielded value.
    CoroutineResume(ValueId),

    // -- Misc --
    /// Reference to a global variable.
    GlobalRef(String),
    /// Spread operator: marks a value for spreading in arrays/objects/calls.
    Spread(ValueId),
    /// Phi-like copy (used internally during SSA construction, prefer block args).
    Copy(ValueId),
}
