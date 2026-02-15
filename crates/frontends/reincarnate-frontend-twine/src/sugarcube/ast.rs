//! AST node types for SugarCube passage content.
//!
//! The AST represents parsed SugarCube markup: macros, links, variable
//! interpolation, inline HTML, and plain text. Expression nodes use a
//! separate `Expr`/`ExprKind` hierarchy for TwineScript (JS superset).

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
    /// A SugarCube macro invocation.
    Macro(MacroNode),
    /// A `[[link]]` or `[[text|passage]]` navigation link.
    Link(LinkNode),
    /// Inline variable reference: `$name`, `_temp`, or `$obj.prop`.
    VarInterp(Expr),
    /// Inline HTML element (preserved as raw text for now).
    Html(String),
    /// An `[img[src]]` or `[img[src][link]]` inline image.
    Image {
        src: String,
        link: Option<String>,
    },
    /// A `/* ... */` or `<!-- ... -->` comment.
    Comment(String),
    /// A line break (`<br>` or literal newline that SugarCube preserves).
    LineBreak,
}

/// A SugarCube macro invocation (`<<name args>>...body...<</name>>`).
#[derive(Debug, Clone)]
pub struct MacroNode {
    /// The macro name (e.g. `if`, `set`, `link`, `widget`).
    pub name: String,
    /// Parsed arguments/expression for the macro.
    pub args: MacroArgs,
    /// For block macros: the clauses (main body + elseif/else/case/default).
    /// Empty for self-closing macros.
    pub clauses: Vec<MacroClause>,
}

/// Arguments to a macro, dispatched by macro kind.
#[derive(Debug, Clone)]
pub enum MacroArgs {
    /// No arguments (e.g. `<<else>>`, `<<silently>>`).
    None,
    /// A single expression (e.g. `<<if expr>>`, `<<print expr>>`, `<<= expr>>`).
    Expr(Expr),
    /// An assignment list (e.g. `<<set $x to 1, $y to 2>>`).
    AssignList(Vec<Expr>),
    /// C-style for: `<<for init; cond; update>>`.
    ForCStyle {
        init: Option<Box<Expr>>,
        cond: Option<Box<Expr>>,
        update: Option<Box<Expr>>,
    },
    /// Range for: `<<for _var range start end>>` (rare, SC2 addition).
    ForRange {
        var: String,
        start: Box<Expr>,
        end: Box<Expr>,
    },
    /// For-in/of: `<<for _val, _key range collection>>`.
    ForIn {
        value_var: String,
        key_var: Option<String>,
        collection: Box<Expr>,
    },
    /// Link target + optional setter: `<<link "text" "passage">>`.
    LinkArgs {
        text: LinkText,
        passage: Option<Box<Expr>>,
    },
    /// Widget definition: `<<widget "name">>`.
    WidgetDef { name: String },
    /// Switch expression: `<<switch expr>>`.
    Switch(Expr),
    /// Case value(s): `<<case val1 val2>>`.
    CaseValues(Vec<Expr>),
    /// Raw unparsed argument string (for unknown/custom macros).
    Raw(String),
}

/// A clause in a block macro (the body between opening and closing/next clause).
#[derive(Debug, Clone)]
pub struct MacroClause {
    /// The clause kind: `"if"`, `"elseif"`, `"else"`, `"case"`, `"default"`, etc.
    pub kind: String,
    /// The clause condition/expression (if any).
    pub args: MacroArgs,
    /// The body nodes of this clause.
    pub body: Vec<Node>,
}

/// A `[[link]]` navigation node.
#[derive(Debug, Clone)]
pub struct LinkNode {
    /// Display text (may be same as target for `[[passage]]` form).
    pub text: LinkText,
    /// Target passage name or expression.
    pub target: LinkTarget,
    /// Optional setter expressions inside `[$code]` after the link.
    pub setters: Vec<Expr>,
}

/// Display text for a link.
#[derive(Debug, Clone)]
pub enum LinkText {
    /// Plain string text.
    Plain(String),
    /// A parsed expression (e.g. backtick template).
    Expr(Box<Expr>),
}

/// Target passage for a link.
#[derive(Debug, Clone)]
pub enum LinkTarget {
    /// A literal passage name.
    Name(String),
    /// A dynamic expression (e.g. `$variable`).
    Expr(Box<Expr>),
}

// ── Expressions (TwineScript) ──────────────────────────────────────────

/// A TwineScript expression with span.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A literal value (bool, number, null, undefined).
    Literal(Literal),
    /// A string literal (`"hello"`, `'world'`).
    Str(String),
    /// An identifier (variable name without `$`/`_` prefix).
    Ident(String),
    /// A SugarCube story variable: `$name` or `$name.prop.chain`.
    StoryVar(String),
    /// A SugarCube temporary variable: `_name` or `_name.prop.chain`.
    TempVar(String),
    /// Property access: `expr.name`.
    Member {
        object: Box<Expr>,
        property: String,
    },
    /// Computed property access: `expr[index]`.
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    /// Function/method call: `expr(args)`.
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    /// `new Constructor(args)`.
    New {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    /// Unary operation: `!x`, `-x`, `++x`, `--x`, `typeof x`, etc.
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    /// Postfix operation: `x++`, `x--`.
    Postfix {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    /// Binary operation: `x + y`, `x is y`, etc.
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Ternary: `cond ? then : else`.
    Ternary {
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    /// Assignment: `$x to 1`, `$x = 1`.
    Assign {
        op: Option<CompoundOp>,
        target: Box<Expr>,
        value: Box<Expr>,
    },
    /// Comma-separated expression (multiple assignments in `<<set>>`).
    Comma(Vec<Expr>),
    /// Array literal: `[1, 2, 3]`.
    Array(Vec<Expr>),
    /// Object literal: `{a: 1, b: 2}`.
    Object(Vec<(Expr, Expr)>),
    /// Template literal: `` `text ${expr} more` ``.
    Template {
        parts: Vec<TemplatePart>,
    },
    /// Arrow function: `(args) => expr` or `(args) => { ... }`.
    Arrow {
        params: Vec<String>,
        body: Box<Expr>,
    },
    /// `delete expr`.
    Delete(Box<Expr>),
    /// `typeof expr`.
    TypeOf(Box<Expr>),
    /// `clone expr` (SugarCube-specific).
    Clone(Box<Expr>),
    /// `def expr` — tests if variable is defined (SugarCube-specific).
    Def(Box<Expr>),
    /// `ndef expr` — tests if variable is NOT defined (SugarCube-specific).
    Ndef(Box<Expr>),
    /// Grouped expression: `(expr)`.
    Paren(Box<Expr>),
    /// Error placeholder for malformed expressions.
    Error(String),
}

/// A template literal part.
#[derive(Debug, Clone)]
pub enum TemplatePart {
    /// Raw text segment.
    Str(String),
    /// Interpolated expression (`${...}`).
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Literal {
    /// `true` or `false`.
    Bool(bool),
    /// Integer or float (stored as f64 for JS semantics).
    Number(NumberLit),
    /// `null`.
    Null,
    /// `undefined`.
    Undefined,
}

/// A number literal (stored as bits for Debug/Clone/PartialEq).
#[derive(Clone, Copy)]
pub struct NumberLit {
    bits: u64,
}

impl NumberLit {
    pub fn new(v: f64) -> Self {
        Self { bits: v.to_bits() }
    }

    pub fn value(self) -> f64 {
        f64::from_bits(self.bits)
    }
}

impl fmt::Debug for NumberLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

impl PartialEq for NumberLit {
    fn eq(&self, other: &Self) -> bool {
        self.bits == other.bits
    }
}

impl Eq for NumberLit {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// `!` or `not`
    Not,
    /// `-` (negation)
    Neg,
    /// `+` (numeric coercion)
    Pos,
    /// `~` (bitwise NOT)
    BitNot,
    /// `++` (pre/post increment)
    Inc,
    /// `--` (pre/post decrement)
    Dec,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp, // **

    // Comparison (desugared from SugarCube keywords)
    Eq,        // == (or `eq`)
    Neq,       // != (or `neq`)
    StrictEq,  // === (or `is`)
    StrictNeq, // !== (or `isnot`)
    Lt,        // < (or `lt`)
    Lte,       // <= (or `lte`)
    Gt,        // > (or `gt`)
    Gte,       // >= (or `gte`)

    // Logical (desugared from SugarCube keywords)
    And, // && (or `and`)
    Or,  // || (or `or`)

    // Nullish coalescing
    NullishCoalesce, // ??

    // Bitwise
    BitAnd, // &
    BitOr,  // |
    BitXor, // ^
    Shl,    // <<
    Shr,    // >>
    UShr,   // >>>

    // Membership
    In,         // `in`
    InstanceOf, // `instanceof`
}

/// Compound assignment operators (`+=`, `-=`, etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompoundOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    UShr,
    NullishCoalesce,
}

