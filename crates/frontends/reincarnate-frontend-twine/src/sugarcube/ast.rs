//! AST node types for SugarCube passage content.
//!
//! The AST represents parsed SugarCube markup: macros, links, variable
//! interpolation, inline HTML, and plain text. Expressions are stored as
//! raw source byte ranges — actual parsing happens at translation time
//! via oxc.

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
    /// The full passage source text. Expressions reference byte ranges into this.
    pub source: String,
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
    /// Opening HTML tag: `<tag attrs...>` — pushes element onto stack.
    HtmlOpen {
        tag: String,
        attrs: Vec<(String, String)>,
        /// Dynamic `@attr="expr"` attributes (kept as raw expression strings).
        dynamic_attrs: Vec<(String, String)>,
    },
    /// Closing HTML tag: `</tag>` — pops element from stack.
    HtmlClose(String),
    /// Void/self-closing HTML element: `<br>`, `<img .../>`.
    HtmlVoid {
        tag: String,
        attrs: Vec<(String, String)>,
        /// Dynamic `@attr="expr"` attributes.
        dynamic_attrs: Vec<(String, String)>,
    },
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
        passage: Option<LinkTarget>,
    },
    /// Widget definition: `<<widget "name">>`.
    WidgetDef { name: String },
    /// Switch expression: `<<switch expr>>`.
    Switch(Expr),
    /// Case value(s): `<<case val1 val2>>`.
    CaseValues(Vec<CaseArg>),
    /// Raw unparsed argument string (for unknown/custom macros).
    Raw(String),
}

/// A single token in `<<case val1 val2>>` argument list.
///
/// Mirrors SugarCube's `parseArgs()` tokenizer categories. Case values are NOT
/// TwineScript expressions — they are compared with `===` against the switch value.
/// Keywords like `lte`, `is`, etc. are barewords here, not operators.
#[derive(Debug, Clone)]
pub enum CaseArg {
    /// `$name` or `_name` — SugarCube/temp variable reference.
    Variable(Expr),
    /// `` `expr` `` — backtick-wrapped TwineScript expression.
    BacktickExpr(Expr),
    /// `"text"` or `'text'` — quoted string literal.
    StringLit(Expr),
    /// `null` literal.
    Null,
    /// `undefined` literal.
    Undefined,
    /// `true` literal.
    True,
    /// `false` literal.
    False,
    /// `NaN` literal.
    Nan,
    /// Numeric literal.
    Number(f64),
    /// Any other bareword — treated as a string value (not desugared).
    Bareword(String),
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

/// A TwineScript expression — stored as a raw source byte range.
///
/// The actual expression text is `&source[start..end]` where `source` is the
/// passage source from `PassageAst::source`. Parsing happens at translation
/// time via the oxc JavaScript parser.
#[derive(Debug, Clone)]
pub struct Expr {
    /// Byte offset of the expression start in the passage source.
    pub start: usize,
    /// Byte offset of the expression end in the passage source.
    pub end: usize,
}

impl Expr {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// Extract the expression source text from the passage source.
    pub fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start..self.end]
    }
}
