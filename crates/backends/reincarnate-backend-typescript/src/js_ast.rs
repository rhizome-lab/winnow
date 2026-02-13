//! Backend-local JavaScript/TypeScript AST types.
//!
//! These types sit between the engine-agnostic core AST (`Stmt`/`Expr`) and
//! TypeScript source output. The mechanical `lower` pass produces a 1:1
//! `JsStmt`/`JsExpr` tree with `SystemCall` nodes preserved as-is. A separate
//! engine-specific rewrite pass (e.g. `rewrites::flash`) then resolves those
//! `SystemCall` nodes into concrete JS constructs (`new`, `typeof`, `throw`,
//! `super`, etc.). The printer handles the final tree with zero engine knowledge.

use reincarnate_core::ir::ast::BinOp;
use reincarnate_core::ir::{CastKind, CmpKind, Constant, MethodKind, Type, UnaryOp, Visibility};

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

/// A JavaScript/TypeScript expression.
#[derive(Debug, Clone)]
pub enum JsExpr {
    /// Literal constant.
    Literal(Constant),
    /// Variable reference.
    Var(String),
    /// `this` reference (resolved from self parameter during lowering).
    This,
    /// Binary operation: `lhs op rhs`.
    Binary {
        op: BinOp,
        lhs: Box<JsExpr>,
        rhs: Box<JsExpr>,
    },
    /// Unary operation: `op expr`.
    Unary {
        op: UnaryOp,
        expr: Box<JsExpr>,
    },
    /// Comparison: `lhs cmp rhs`.
    Cmp {
        kind: CmpKind,
        lhs: Box<JsExpr>,
        rhs: Box<JsExpr>,
    },
    /// Field access: `object.field`.
    Field {
        object: Box<JsExpr>,
        field: String,
    },
    /// Index access: `collection[index]`.
    Index {
        collection: Box<JsExpr>,
        index: Box<JsExpr>,
    },
    /// Function/method call: `callee(args...)`.
    ///
    /// Unified representation — the callee is an expression:
    /// - Method call: `Field(object, method)` as callee
    /// - Function call: `Var(name)` as callee
    /// - Indirect call: any expression as callee
    Call {
        callee: Box<JsExpr>,
        args: Vec<JsExpr>,
    },
    /// Ternary: `cond ? then_val : else_val`.
    Ternary {
        cond: Box<JsExpr>,
        then_val: Box<JsExpr>,
        else_val: Box<JsExpr>,
    },
    /// Short-circuit OR: `lhs || rhs`.
    LogicalOr {
        lhs: Box<JsExpr>,
        rhs: Box<JsExpr>,
    },
    /// Short-circuit AND: `lhs && rhs`.
    LogicalAnd {
        lhs: Box<JsExpr>,
        rhs: Box<JsExpr>,
    },
    /// Type cast: `expr as Type`.
    Cast {
        expr: Box<JsExpr>,
        ty: Type,
        kind: CastKind,
    },
    /// Runtime type check (`instanceof` / `typeof ===`).
    TypeCheck {
        expr: Box<JsExpr>,
        ty: Type,
    },
    /// Array literal: `[elements...]`.
    ArrayInit(Vec<JsExpr>),
    /// Object literal: `{ key: value, ... }`.
    ObjectInit(Vec<(String, JsExpr)>),
    /// Tuple literal (emitted as array): `[elements...]`.
    TupleInit(Vec<JsExpr>),
    /// Logical NOT: `!expr`.
    Not(Box<JsExpr>),
    /// Post-increment: `expr++`.
    PostIncrement(Box<JsExpr>),
    /// Generator create (call to generator function).
    GeneratorCreate {
        func: String,
        args: Vec<JsExpr>,
    },
    /// Generator resume: `expr.next()`.
    GeneratorResume(Box<JsExpr>),
    /// Yield expression.
    Yield(Option<Box<JsExpr>>),

    // --- JS-specific constructs (lowered from SystemCalls) ---
    /// `new Ctor(args)`.
    New {
        callee: Box<JsExpr>,
        args: Vec<JsExpr>,
    },
    /// `typeof expr`.
    TypeOf(Box<JsExpr>),
    /// `key in object`.
    In {
        key: Box<JsExpr>,
        object: Box<JsExpr>,
    },
    /// `delete object[key]`.
    Delete {
        object: Box<JsExpr>,
        key: Box<JsExpr>,
    },
    /// `super(args)` — super constructor call in expression context.
    SuperCall(Vec<JsExpr>),
    /// `super.method(args)`.
    SuperMethodCall {
        method: String,
        args: Vec<JsExpr>,
    },
    /// `super.prop` — super property read.
    SuperGet(String),
    /// `super.prop = value` — super property write in expression context.
    SuperSet {
        prop: String,
        value: Box<JsExpr>,
    },
    /// Activation object: `({})`.
    Activation,
    /// Arrow function: `(params) => { body }`.
    ArrowFunction {
        params: Vec<(String, Type)>,
        return_ty: Type,
        body: Vec<JsStmt>,
        has_rest_param: bool,
        /// Optional type assertion: `(arrow) as <cast_as>`.
        cast_as: Option<String>,
    },

    // --- Fallback ---
    /// Unmapped system call (passthrough to runtime module).
    SystemCall {
        system: String,
        method: String,
        args: Vec<JsExpr>,
    },
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

/// A JavaScript/TypeScript statement.
#[derive(Debug, Clone)]
pub enum JsStmt {
    /// Variable declaration: `let/const name [: type] [= init];`.
    VarDecl {
        name: String,
        ty: Option<Type>,
        init: Option<JsExpr>,
        mutable: bool,
    },
    /// Assignment: `target = value;`.
    Assign {
        target: JsExpr,
        value: JsExpr,
    },
    /// Compound assignment: `target op= value;`.
    CompoundAssign {
        target: JsExpr,
        op: BinOp,
        value: JsExpr,
    },
    /// Expression statement.
    Expr(JsExpr),
    /// If/else.
    If {
        cond: JsExpr,
        then_body: Vec<JsStmt>,
        else_body: Vec<JsStmt>,
    },
    /// While loop.
    While {
        cond: JsExpr,
        body: Vec<JsStmt>,
    },
    /// For loop (init promoted out, emitted as while).
    For {
        init: Vec<JsStmt>,
        cond: JsExpr,
        update: Vec<JsStmt>,
        body: Vec<JsStmt>,
    },
    /// Infinite loop.
    Loop {
        body: Vec<JsStmt>,
    },
    /// For-of loop.
    ForOf {
        binding: String,
        declare: bool,
        iterable: JsExpr,
        body: Vec<JsStmt>,
    },
    /// Return.
    Return(Option<JsExpr>),
    /// Break.
    Break,
    /// Continue.
    Continue,
    /// Labeled break.
    LabeledBreak {
        depth: usize,
    },
    /// Dispatch (irreducible CFG fallback).
    Dispatch {
        blocks: Vec<(usize, Vec<JsStmt>)>,
        entry: usize,
    },

    // --- JS-specific statements ---
    /// `throw expr;`.
    Throw(JsExpr),
}

// ---------------------------------------------------------------------------
// Function
// ---------------------------------------------------------------------------

/// A function lowered to JS AST form, ready for printing.
#[derive(Debug, Clone)]
pub struct JsFunction {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_ty: Type,
    pub body: Vec<JsStmt>,
    pub is_generator: bool,
    pub visibility: Visibility,
    pub method_kind: MethodKind,
    /// Whether the last parameter is a rest/variadic parameter (`...args`).
    pub has_rest_param: bool,
}
