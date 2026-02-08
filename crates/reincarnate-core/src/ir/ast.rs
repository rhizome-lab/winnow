//! High-level AST types for backend code generation.
//!
//! The AST sits between the SSA-based IR and target-language source code.
//! A shared lowering pass (`lower_ast`) converts `Shape + Function` into
//! `Vec<Stmt>`, handling expression inlining, dead code elimination, and
//! control-flow pattern detection once. Each backend only pretty-prints
//! the AST to its target syntax.

use super::func::{MethodKind, Visibility};
use super::inst::CmpKind;
use super::ty::Type;
use super::value::Constant;

/// A high-level expression (no side effects except calls).
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literal constant.
    Literal(Constant),
    /// Named variable reference.
    Var(String),
    /// Binary operation: `lhs op rhs`.
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    /// Unary operation: `op expr`.
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    /// Comparison: `lhs cmp rhs`.
    Cmp {
        kind: CmpKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    /// Field access: `object.field`.
    Field {
        object: Box<Expr>,
        field: String,
    },
    /// Index access: `collection[index]`.
    Index {
        collection: Box<Expr>,
        index: Box<Expr>,
    },
    /// Direct function/method call: `func(args...)`.
    Call {
        func: String,
        args: Vec<Expr>,
    },
    /// Indirect call: `callee(args...)`.
    CallIndirect {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    /// System call: `system.method(args...)`.
    SystemCall {
        system: String,
        method: String,
        args: Vec<Expr>,
    },
    /// Ternary: `cond ? then_val : else_val`.
    Ternary {
        cond: Box<Expr>,
        then_val: Box<Expr>,
        else_val: Box<Expr>,
    },
    /// Short-circuit OR: `lhs || rhs`.
    LogicalOr {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    /// Short-circuit AND: `lhs && rhs`.
    LogicalAnd {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    /// Type cast.
    Cast {
        expr: Box<Expr>,
        ty: Type,
    },
    /// Runtime type check (returns bool).
    TypeCheck {
        expr: Box<Expr>,
        ty: Type,
    },
    /// Array literal: `[elements...]`.
    ArrayInit(Vec<Expr>),
    /// Struct/object literal: `{ field: value, ... }`.
    StructInit {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    /// Tuple literal: `(elements...)`.
    TupleInit(Vec<Expr>),
    /// Global variable reference.
    GlobalRef(String),
    /// Coroutine create.
    CoroutineCreate {
        func: String,
        args: Vec<Expr>,
    },
    /// Coroutine resume.
    CoroutineResume(Box<Expr>),
    /// Yield expression.
    Yield(Option<Box<Expr>>),
    /// Logical NOT: `!expr` (formatting hint — distinct from bitwise NOT).
    Not(Box<Expr>),
}

/// Binary arithmetic/bitwise operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    BitNot,
}

/// A high-level statement.
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Variable declaration: `let/const name [: type] [= init];`
    VarDecl {
        name: String,
        ty: Option<Type>,
        init: Option<Expr>,
        mutable: bool,
    },
    /// Assignment: `target = value;`
    Assign {
        target: Expr,
        value: Expr,
    },
    /// Expression statement (side-effecting call, void return).
    Expr(Expr),
    /// If/else.
    If {
        cond: Expr,
        then_body: Vec<Stmt>,
        else_body: Vec<Stmt>,
    },
    /// While loop with condition.
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    /// For loop: `for (init; cond; update) { body }`.
    For {
        init: Vec<Stmt>,
        cond: Expr,
        update: Vec<Stmt>,
        body: Vec<Stmt>,
    },
    /// Infinite loop (`while (true) { ... }`).
    Loop {
        body: Vec<Stmt>,
    },
    /// Return.
    Return(Option<Expr>),
    /// Break.
    Break,
    /// Continue.
    Continue,
    /// Labeled break to an outer loop.
    LabeledBreak {
        depth: usize,
    },
    /// Dispatch (fallback for irreducible CFGs).
    Dispatch {
        blocks: Vec<(usize, Vec<Stmt>)>,
        entry: usize,
    },
}

/// A function lowered to AST form.
#[derive(Debug, Clone)]
pub struct AstFunction {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_ty: Type,
    pub body: Vec<Stmt>,
    pub is_generator: bool,
    pub visibility: Visibility,
    pub method_kind: MethodKind,
}
