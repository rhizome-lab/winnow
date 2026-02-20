//! AST node types for Harlowe passage content.
//!
//! The AST represents parsed Harlowe markup: `(macro:)` calls with `[hook]`
//! content blocks, `[[links]]`, variable interpolation (`$var`), inline HTML,
//! and plain text. Expression nodes use a separate `Expr`/`ExprKind` hierarchy
//! for Harlowe's custom expression language.

use std::fmt;

/// Byte offset span in the passage source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn empty(pos: usize) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }
}

/// A parse error accumulated during passage parsing.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}..{}] {}",
            self.span.start, self.span.end, self.message
        )
    }
}

/// Result of parsing a single passage.
#[derive(Debug)]
pub struct PassageAst {
    pub body: Vec<Node>,
    pub errors: Vec<ParseError>,
}

/// A single AST node in a passage body.
#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    /// Plain text content (including whitespace).
    Text(String),
    /// A Harlowe macro invocation: `(name: args)`.
    Macro(MacroNode),
    /// An optional `[hook]` content block attached to a macro or changer.
    Hook(Vec<Node>),
    /// A `[[link]]` or `[[text->passage]]` navigation link.
    Link(LinkNode),
    /// Inline variable reference: `$name` or `_name` (implicit print).
    VarInterp(String),
    /// Opening HTML tag: `<tag attrs...>` — pushes element onto stack.
    HtmlOpen {
        tag: String,
        attrs: Vec<(String, String)>,
    },
    /// Closing HTML tag: `</tag>` — pops element from stack.
    HtmlClose(String),
    /// Void/self-closing HTML element: `<br>`, `<img .../>`.
    HtmlVoid {
        tag: String,
        attrs: Vec<(String, String)>,
    },
    /// Inline markup: `**bold**`, `*italic*`, `''bold''`, `//italic//`, etc.
    Markup { tag: String, body: Vec<Node> },
    /// Apply a changer from a variable to a hook: `$var[hook]` or `_var[hook]`.
    ChangerApply { name: String, hook: Vec<Node> },
    /// A line break (literal newline).
    LineBreak,
    /// Named hook definition: `|name>[content]`.
    /// Wraps `content` in `<tw-hook name="name">` for later DOM targeting via `?name`.
    NamedHook { name: String, body: Vec<Node> },
}

/// A Harlowe macro invocation: `(name: args)[hook]`.
#[derive(Debug, Clone)]
pub struct MacroNode {
    /// The macro name (e.g. `set`, `if`, `color`, `link`).
    pub name: String,
    /// Parsed arguments/expressions for the macro.
    pub args: Vec<Expr>,
    /// Optional hook content attached with `[...]` immediately after.
    pub hook: Option<Vec<Node>>,
    /// For `(if:)` chains: the else-if and else clauses that follow.
    pub clauses: Vec<IfClause>,
    /// Byte offset span of the whole macro invocation (including hook/clauses).
    pub span: Span,
}

/// An else-if or else clause in a Harlowe `(if:)` chain.
#[derive(Debug, Clone)]
pub struct IfClause {
    /// `"else-if"` or `"else"`.
    pub kind: String,
    /// The condition expression (None for `(else:)`).
    pub cond: Option<Expr>,
    /// The hook body.
    pub body: Vec<Node>,
}

/// A `[[link]]` navigation node.
#[derive(Debug, Clone)]
pub struct LinkNode {
    /// Display text (may be same as target for `[[passage]]` form).
    pub text: String,
    /// Target passage name.
    pub passage: String,
}

// ── Expressions (Harlowe) ──────────────────────────────────────────────

/// A Harlowe expression with span.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A numeric literal.
    Number(f64),
    /// A string literal (`"hello"` or `'world'`).
    Str(String),
    /// A boolean literal (`true` or `false`).
    Bool(bool),
    /// An identifier (e.g. `red`, `green`, `fade` — used as values/keywords).
    Ident(String),
    /// A story variable: `$name`.
    StoryVar(String),
    /// A temporary variable: `_name`.
    TempVar(String),
    /// `it` — refers to the most recently-set variable's value.
    It,
    /// Binary operation: `x + y`, `x is y`, `x contains y`, etc.
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Unary operation: `not x`, `-x`.
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    /// Function/macro call within an expression: `(random: 1, 10)`.
    Call {
        name: String,
        args: Vec<Expr>,
    },
    /// Possessive access: `$arr's 1st`, `$obj's name`.
    Possessive {
        object: Box<Expr>,
        property: Box<Expr>,
    },
    /// `of` access: `1st of $arr`, `name of $obj`.
    Of {
        property: Box<Expr>,
        object: Box<Expr>,
    },
    /// Grouped expression: `(expr)` when not a macro call.
    Paren(Box<Expr>),
    /// Time literal: `2s`, `500ms`.
    TimeLiteral(f64),
    /// Color literal: `red`, `green`, `#FF0000`, `magenta+white`.
    ColorLiteral(String),
    /// Ordinal accessor: `1st`, `2nd`, `3rd`, `last`.
    Ordinal(Ordinal),
    /// Assignment target in `(set:)`: `$var to expr`.
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    /// Named hook selector: `?name`.
    /// Lowered to a CSS selector string `tw-hook[name='name']` for DOM targeting.
    HookSelector(String),
    /// Lambda expression: `each _var` or `each _var where condition`.
    /// Used as the first argument to `(for:)`.
    Lambda {
        var: String,
        filter: Option<Box<Expr>>,
    },
    /// `via` transform lambda: `via expr` — transforms each element.
    /// Uses `it`/`its` inside `expr` to refer to the current element.
    ViaLambda(Box<Expr>),
    /// Fold lambda: `each _item making _acc via expr`.
    /// Used as the first argument to `(folded:)`.
    /// The emitted callback takes `(item, acc)` and returns the new accumulator.
    FoldLambda {
        item_var: String,
        acc_var: String,
        body: Box<Expr>,
    },
    /// Spread expression: `...$arr` or `...(expr)`.
    /// Expands an iterable into the surrounding argument list.
    Spread(Box<Expr>),
    /// Dynamic macro call: `($var: args)` — callee is a story/temp var holding a custom macro.
    /// Lowered to `CallIndirect { callee, args }` in the IR.
    DynCall {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    /// Inline `(macro:)` definition with its hook body captured in expression position.
    /// E.g. `(macro: dm-type _x)[body text]` when the whole expression appears as an arg
    /// to another macro. The hook body is stored as raw source text and re-parsed during
    /// lowering so that the main passage parser's `[hook]` handling is not bypassed.
    MacroDef {
        params: Vec<Expr>,
        hook_source: String,
    },
    /// Error placeholder for malformed expressions.
    Error(String),
}

/// Binary operators in Harlowe expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Is,
    IsNot,
    Lt,
    Lte,
    Gt,
    Gte,

    // Logical
    And,
    Or,

    // Membership
    Contains,
    IsIn,
    IsNotIn,

    // String/changer composition
    Plus, // `+` on changers composes them (reuses Add for arithmetic)
}

/// Unary operators in Harlowe expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// `not`
    Not,
    /// `-` (negation)
    Neg,
}

/// Ordinal accessor (`1st`, `2nd`, `3rd`, `last`, `2ndlast`, `1stto4th`, etc.).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ordinal {
    /// 1-based forward index: `1st` = Nth(1), `2nd` = Nth(2), etc.
    Nth(u32),
    /// Reverse index: `2ndlast` = NthLast(2) — second from the end.
    NthLast(u32),
    /// `last` — last element (shorthand for NthLast(1)).
    Last,
    /// `length` — the number of elements.
    Length,
    /// Range slice: `1stto4th`, `2ndto2ndlast`, `lasttolast`.
    Range { from: RangeEnd, to: RangeEnd },
}

/// One endpoint of a range slice ordinal.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RangeEnd {
    /// 1-based forward position.
    Nth(u32),
    /// Reverse position from end: `2ndlast` = NthLast(2).
    NthLast(u32),
    /// The last element (`-1`).
    Last,
}
