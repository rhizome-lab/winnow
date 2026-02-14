//! Lexer for TwineScript expressions inside SugarCube macros.
//!
//! TwineScript is a JavaScript superset with additional keyword operators:
//! `is` (===), `isnot` (!==), `to` (=), `not` (!), `and` (&&), `or` (||),
//! `eq` (==), `neq` (!=), `lt` (<), `lte` (<=), `gt` (>), `gte` (>=),
//! `def` (defined test), `ndef` (not-defined test).
//!
//! The `ExprLexer` tokenizes a substring of passage source (the content
//! between `<<macro` and `>>`) into fine-grained tokens. It tracks
//! bracket/paren/brace depth so that `>>` at depth 0 terminates the
//! macro rather than being parsed as a right-shift operator.

use super::ast::Span;

/// A single token produced by the expression lexer.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // ── Literals ───────────────────────────────────────────────────
    /// A numeric literal (integer or float, including hex/octal/binary).
    Number(f64),
    /// A string literal (single or double quoted, escape sequences resolved).
    Str(String),
    /// A template literal (backtick). Contains raw segments between `${...}`.
    /// The lexer emits the whole template as one token; the expression parser
    /// breaks it into parts.
    TemplateStart(String),
    TemplateMiddle(String),
    TemplateEnd(String),
    /// A bare template with no interpolation.
    TemplateFull(String),

    // ── Identifiers / Keywords ─────────────────────────────────────
    Ident(String),

    // SugarCube keyword operators
    KwIs,       // `is` → ===
    KwIsnot,    // `isnot` → !==
    KwTo,       // `to` → =
    KwNot,      // `not` → !
    KwAnd,      // `and` → &&
    KwOr,       // `or` → ||
    KwEq,       // `eq` → ==
    KwNeq,      // `neq` → !=
    KwLt,       // `lt` → <
    KwLte,      // `lte` → <=
    KwGt,       // `gt` → >
    KwGte,      // `gte` → >=
    KwDef,      // `def` (defined)
    KwNdef,     // `ndef` (not defined)

    // JS keywords
    KwTrue,
    KwFalse,
    KwNull,
    KwUndefined,
    KwNew,
    KwDelete,
    KwTypeof,
    KwInstanceof,
    KwIn,
    KwClone,    // SugarCube extension
    KwVoid,

    // ── Variables ──────────────────────────────────────────────────
    /// `$varname` (story variable).
    StoryVar(String),
    /// `_varname` (temporary variable).
    TempVar(String),

    // ── Punctuation / Operators ────────────────────────────────────
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }
    Dot,        // .
    Comma,      // ,
    Semicolon,  // ;
    Colon,      // :
    Question,   // ?
    Arrow,      // =>

    // Arithmetic
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    StarStar,   // **

    // Comparison
    EqEq,       // ==
    NotEq,      // !=
    EqEqEq,     // ===
    NotEqEq,    // !==
    Less,       // <
    LessEq,     // <=
    Greater,    // >
    GreaterEq,  // >=

    // Logical
    AmpAmp,     // &&
    PipePipe,   // ||
    Bang,       // !
    QuestionQuestion, // ??

    // Bitwise
    Amp,        // &
    Pipe,       // |
    Caret,      // ^
    Tilde,      // ~
    LessLess,   // <<
    GreaterGreater,   // >>
    GreaterGreaterGreater, // >>>

    // Assignment
    Eq,         // =
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    PercentEq,  // %=
    StarStarEq, // **=
    AmpEq,      // &=
    PipeEq,     // |=
    CaretEq,    // ^=
    LessLessEq, // <<=
    GreaterGreaterEq, // >>=
    GreaterGreaterGreaterEq, // >>>=
    QuestionQuestionEq, // ??=

    // Increment/Decrement
    PlusPlus,   // ++
    MinusMinus, // --

    // Spread
    DotDotDot,  // ...

    // ── Special ────────────────────────────────────────────────────
    /// End of the expression (reached `>>` at depth 0, or end of input).
    Eof,
}

/// Lexer for TwineScript expressions.
///
/// Operates on a slice of the passage source. The `base_offset` is added
/// to all span positions so they refer to the original passage source.
pub struct ExprLexer<'a> {
    src: &'a str,
    bytes: &'a [u8],
    pos: usize,
    base_offset: usize,
    /// Nesting depth of brackets/parens/braces.
    depth: i32,
    /// Whether we've emitted EOF.
    done: bool,
    /// Peeked token, if any.
    peeked: Option<Token>,
    /// Template literal nesting depth.
    template_depth: i32,
}

impl<'a> ExprLexer<'a> {
    /// Create a new expression lexer over `src` starting at position `base_offset`.
    pub fn new(src: &'a str, base_offset: usize) -> Self {
        Self {
            src,
            bytes: src.as_bytes(),
            pos: 0,
            base_offset,
            depth: 0,
            done: false,
            peeked: None,
            template_depth: 0,
        }
    }

    /// Peek at the next token without consuming it.
    pub fn peek(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_token());
        }
        self.peeked.as_ref().unwrap()
    }

    /// Consume and return the next token.
    pub fn next_tok(&mut self) -> Token {
        if let Some(tok) = self.peeked.take() {
            tok
        } else {
            self.next_token()
        }
    }

    /// Return the remaining unconsumed source (for raw arg extraction).
    pub fn remaining(&self) -> &'a str {
        let pos = if let Some(ref tok) = self.peeked {
            tok.span.start - self.base_offset
        } else {
            self.pos
        };
        &self.src[pos..]
    }

    /// Return the current absolute position in the passage source.
    pub fn position(&self) -> usize {
        if let Some(ref tok) = self.peeked {
            tok.span.start
        } else {
            self.base_offset + self.pos
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.bytes.len()
    }

    fn ch(&self) -> u8 {
        self.bytes[self.pos]
    }

    fn ch_at(&self, offset: usize) -> u8 {
        let i = self.pos + offset;
        if i < self.bytes.len() {
            self.bytes[i]
        } else {
            0
        }
    }

    fn advance(&mut self) -> u8 {
        let c = self.bytes[self.pos];
        self.pos += 1;
        c
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.bytes.len() {
            match self.bytes[self.pos] {
                b' ' | b'\t' | b'\r' | b'\n' => self.pos += 1,
                _ => break,
            }
        }
    }

    fn span(&self, start: usize) -> Span {
        Span::new(self.base_offset + start, self.base_offset + self.pos)
    }

    fn next_token(&mut self) -> Token {
        if self.done {
            return Token {
                kind: TokenKind::Eof,
                span: Span::empty(self.base_offset + self.pos),
            };
        }

        self.skip_whitespace();

        if self.at_end() {
            self.done = true;
            return Token {
                kind: TokenKind::Eof,
                span: Span::empty(self.base_offset + self.pos),
            };
        }

        let start = self.pos;
        let c = self.ch();

        match c {
            // `>>` at depth 0 terminates the macro
            b'>' if self.ch_at(1) == b'>' && self.depth <= 0 => {
                self.done = true;
                Token {
                    kind: TokenKind::Eof,
                    span: Span::empty(self.base_offset + self.pos),
                }
            }

            // String literals
            b'"' | b'\'' => self.scan_string(),

            // Template literals
            b'`' => self.scan_template_start(),

            // Numbers
            b'0'..=b'9' => self.scan_number(),

            // Variables: $name or _name
            b'$' => self.scan_story_var(),
            b'_' if self.is_var_start() => self.scan_temp_var(),

            // Identifiers and keywords
            _ if is_ident_start(c) => self.scan_ident_or_keyword(),

            // Punctuation / operators
            b'(' => {
                self.advance();
                self.depth += 1;
                Token { kind: TokenKind::LParen, span: self.span(start) }
            }
            b')' => {
                self.advance();
                self.depth -= 1;
                Token { kind: TokenKind::RParen, span: self.span(start) }
            }
            b'[' => {
                self.advance();
                self.depth += 1;
                Token { kind: TokenKind::LBracket, span: self.span(start) }
            }
            b']' => {
                self.advance();
                self.depth -= 1;
                Token { kind: TokenKind::RBracket, span: self.span(start) }
            }
            b'{' => {
                self.advance();
                self.depth += 1;
                Token { kind: TokenKind::LBrace, span: self.span(start) }
            }
            b'}' => {
                if self.template_depth > 0 {
                    // This `}` closes a `${...}` interpolation — resume template scanning.
                    self.advance();
                    self.depth -= 1;
                    return self.scan_template_continuation();
                }
                self.advance();
                self.depth -= 1;
                Token { kind: TokenKind::RBrace, span: self.span(start) }
            }
            b'.' => {
                if self.ch_at(1) == b'.' && self.ch_at(2) == b'.' {
                    self.pos += 3;
                    Token { kind: TokenKind::DotDotDot, span: self.span(start) }
                } else if self.ch_at(1).is_ascii_digit() {
                    self.scan_number()
                } else {
                    self.advance();
                    Token { kind: TokenKind::Dot, span: self.span(start) }
                }
            }
            b',' => { self.advance(); Token { kind: TokenKind::Comma, span: self.span(start) } }
            b';' => { self.advance(); Token { kind: TokenKind::Semicolon, span: self.span(start) } }
            b':' => { self.advance(); Token { kind: TokenKind::Colon, span: self.span(start) } }
            b'~' => { self.advance(); Token { kind: TokenKind::Tilde, span: self.span(start) } }

            b'?' => {
                self.advance();
                if !self.at_end() && self.ch() == b'?' {
                    self.advance();
                    if !self.at_end() && self.ch() == b'=' {
                        self.advance();
                        Token { kind: TokenKind::QuestionQuestionEq, span: self.span(start) }
                    } else {
                        Token { kind: TokenKind::QuestionQuestion, span: self.span(start) }
                    }
                } else {
                    Token { kind: TokenKind::Question, span: self.span(start) }
                }
            }

            b'+' => {
                self.advance();
                if !self.at_end() && self.ch() == b'+' {
                    self.advance();
                    Token { kind: TokenKind::PlusPlus, span: self.span(start) }
                } else if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::PlusEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Plus, span: self.span(start) }
                }
            }
            b'-' => {
                self.advance();
                if !self.at_end() && self.ch() == b'-' {
                    self.advance();
                    Token { kind: TokenKind::MinusMinus, span: self.span(start) }
                } else if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::MinusEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Minus, span: self.span(start) }
                }
            }
            b'*' => {
                self.advance();
                if !self.at_end() && self.ch() == b'*' {
                    self.advance();
                    if !self.at_end() && self.ch() == b'=' {
                        self.advance();
                        Token { kind: TokenKind::StarStarEq, span: self.span(start) }
                    } else {
                        Token { kind: TokenKind::StarStar, span: self.span(start) }
                    }
                } else if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::StarEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Star, span: self.span(start) }
                }
            }
            b'/' => {
                // Check for // or /* comments
                if self.ch_at(1) == b'/' {
                    // Line comment — skip to end of line
                    self.pos += 2;
                    while !self.at_end() && self.ch() != b'\n' {
                        self.pos += 1;
                    }
                    return self.next_token();
                } else if self.ch_at(1) == b'*' {
                    // Block comment — skip to */
                    self.pos += 2;
                    while !self.at_end() {
                        if self.ch() == b'*' && self.ch_at(1) == b'/' {
                            self.pos += 2;
                            break;
                        }
                        self.pos += 1;
                    }
                    return self.next_token();
                }
                self.advance();
                if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::SlashEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Slash, span: self.span(start) }
                }
            }
            b'%' => {
                self.advance();
                if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::PercentEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Percent, span: self.span(start) }
                }
            }
            b'=' => {
                self.advance();
                if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    if !self.at_end() && self.ch() == b'=' {
                        self.advance();
                        Token { kind: TokenKind::EqEqEq, span: self.span(start) }
                    } else {
                        Token { kind: TokenKind::EqEq, span: self.span(start) }
                    }
                } else if !self.at_end() && self.ch() == b'>' {
                    self.advance();
                    Token { kind: TokenKind::Arrow, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Eq, span: self.span(start) }
                }
            }
            b'!' => {
                self.advance();
                if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    if !self.at_end() && self.ch() == b'=' {
                        self.advance();
                        Token { kind: TokenKind::NotEqEq, span: self.span(start) }
                    } else {
                        Token { kind: TokenKind::NotEq, span: self.span(start) }
                    }
                } else {
                    Token { kind: TokenKind::Bang, span: self.span(start) }
                }
            }
            b'<' => {
                self.advance();
                if !self.at_end() && self.ch() == b'<' {
                    self.advance();
                    if !self.at_end() && self.ch() == b'=' {
                        self.advance();
                        Token { kind: TokenKind::LessLessEq, span: self.span(start) }
                    } else {
                        Token { kind: TokenKind::LessLess, span: self.span(start) }
                    }
                } else if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::LessEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Less, span: self.span(start) }
                }
            }
            b'>' => {
                // At depth > 0, `>` is comparison, `>>` is right-shift
                self.advance();
                if !self.at_end() && self.ch() == b'>' {
                    self.advance();
                    if !self.at_end() && self.ch() == b'>' {
                        self.advance();
                        if !self.at_end() && self.ch() == b'=' {
                            self.advance();
                            Token { kind: TokenKind::GreaterGreaterGreaterEq, span: self.span(start) }
                        } else {
                            Token { kind: TokenKind::GreaterGreaterGreater, span: self.span(start) }
                        }
                    } else if !self.at_end() && self.ch() == b'=' {
                        self.advance();
                        Token { kind: TokenKind::GreaterGreaterEq, span: self.span(start) }
                    } else {
                        Token { kind: TokenKind::GreaterGreater, span: self.span(start) }
                    }
                } else if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::GreaterEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Greater, span: self.span(start) }
                }
            }
            b'&' => {
                self.advance();
                if !self.at_end() && self.ch() == b'&' {
                    self.advance();
                    Token { kind: TokenKind::AmpAmp, span: self.span(start) }
                } else if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::AmpEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Amp, span: self.span(start) }
                }
            }
            b'|' => {
                self.advance();
                if !self.at_end() && self.ch() == b'|' {
                    self.advance();
                    Token { kind: TokenKind::PipePipe, span: self.span(start) }
                } else if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::PipeEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Pipe, span: self.span(start) }
                }
            }
            b'^' => {
                self.advance();
                if !self.at_end() && self.ch() == b'=' {
                    self.advance();
                    Token { kind: TokenKind::CaretEq, span: self.span(start) }
                } else {
                    Token { kind: TokenKind::Caret, span: self.span(start) }
                }
            }

            _ => {
                // Unknown character — skip it
                self.advance();
                self.next_token()
            }
        }
    }

    fn scan_string(&mut self) -> Token {
        let start = self.pos;
        let quote = self.advance();
        let mut s = String::new();
        while !self.at_end() {
            let c = self.advance();
            if c == quote {
                return Token {
                    kind: TokenKind::Str(s),
                    span: self.span(start),
                };
            }
            if c == b'\\' && !self.at_end() {
                let esc = self.advance();
                match esc {
                    b'n' => s.push('\n'),
                    b'r' => s.push('\r'),
                    b't' => s.push('\t'),
                    b'\\' => s.push('\\'),
                    b'\'' => s.push('\''),
                    b'"' => s.push('"'),
                    b'0' => s.push('\0'),
                    b'x' => {
                        if let Some(ch) = self.scan_hex_escape(2) {
                            s.push(ch);
                        }
                    }
                    b'u' => {
                        if let Some(ch) = self.scan_unicode_escape() {
                            s.push(ch);
                        }
                    }
                    _ => {
                        s.push('\\');
                        s.push(esc as char);
                    }
                }
            } else {
                s.push(c as char);
            }
        }
        // Unterminated string — return what we have
        Token {
            kind: TokenKind::Str(s),
            span: self.span(start),
        }
    }

    fn scan_hex_escape(&mut self, digits: usize) -> Option<char> {
        let mut val = 0u32;
        for _ in 0..digits {
            if self.at_end() {
                return None;
            }
            let d = self.advance();
            let hex = match d {
                b'0'..=b'9' => d - b'0',
                b'a'..=b'f' => d - b'a' + 10,
                b'A'..=b'F' => d - b'A' + 10,
                _ => return None,
            };
            val = val * 16 + hex as u32;
        }
        char::from_u32(val)
    }

    fn scan_unicode_escape(&mut self) -> Option<char> {
        if self.at_end() {
            return None;
        }
        if self.ch() == b'{' {
            // \u{XXXX}
            self.advance();
            let mut val = 0u32;
            while !self.at_end() && self.ch() != b'}' {
                let d = self.advance();
                let hex = match d {
                    b'0'..=b'9' => d - b'0',
                    b'a'..=b'f' => d - b'a' + 10,
                    b'A'..=b'F' => d - b'A' + 10,
                    _ => return None,
                };
                val = val * 16 + hex as u32;
            }
            if !self.at_end() {
                self.advance(); // skip }
            }
            char::from_u32(val)
        } else {
            self.scan_hex_escape(4)
        }
    }

    fn scan_template_start(&mut self) -> Token {
        let start = self.pos;
        self.advance(); // skip `
        let mut s = String::new();
        while !self.at_end() {
            if self.ch() == b'`' {
                self.advance();
                return Token {
                    kind: TokenKind::TemplateFull(s),
                    span: self.span(start),
                };
            }
            if self.ch() == b'$' && self.ch_at(1) == b'{' {
                self.pos += 2;
                self.depth += 1;
                self.template_depth += 1;
                return Token {
                    kind: TokenKind::TemplateStart(s),
                    span: self.span(start),
                };
            }
            if self.ch() == b'\\' && self.pos + 1 < self.bytes.len() {
                self.advance();
                let esc = self.advance();
                match esc {
                    b'n' => s.push('\n'),
                    b'`' => s.push('`'),
                    b'\\' => s.push('\\'),
                    b'$' => s.push('$'),
                    _ => {
                        s.push('\\');
                        s.push(esc as char);
                    }
                }
            } else {
                s.push(self.advance() as char);
            }
        }
        // Unterminated template
        Token {
            kind: TokenKind::TemplateFull(s),
            span: self.span(start),
        }
    }

    fn scan_template_continuation(&mut self) -> Token {
        let start = self.pos;
        let mut s = String::new();
        while !self.at_end() {
            if self.ch() == b'`' {
                self.advance();
                self.template_depth -= 1;
                return Token {
                    kind: TokenKind::TemplateEnd(s),
                    span: self.span(start),
                };
            }
            if self.ch() == b'$' && self.ch_at(1) == b'{' {
                self.pos += 2;
                self.depth += 1;
                return Token {
                    kind: TokenKind::TemplateMiddle(s),
                    span: self.span(start),
                };
            }
            if self.ch() == b'\\' && self.pos + 1 < self.bytes.len() {
                self.advance();
                let esc = self.advance();
                match esc {
                    b'n' => s.push('\n'),
                    b'`' => s.push('`'),
                    b'\\' => s.push('\\'),
                    b'$' => s.push('$'),
                    _ => {
                        s.push('\\');
                        s.push(esc as char);
                    }
                }
            } else {
                s.push(self.advance() as char);
            }
        }
        // Unterminated
        self.template_depth -= 1;
        Token {
            kind: TokenKind::TemplateEnd(s),
            span: self.span(start),
        }
    }

    fn scan_number(&mut self) -> Token {
        let start = self.pos;
        // Check for 0x, 0o, 0b prefixes
        if self.ch() == b'0' && self.pos + 1 < self.bytes.len() {
            match self.bytes[self.pos + 1] {
                b'x' | b'X' => return self.scan_hex_number(start),
                b'o' | b'O' => return self.scan_oct_number(start),
                b'b' | b'B' => return self.scan_bin_number(start),
                _ => {}
            }
        }

        // Decimal integer or float
        while !self.at_end() && self.ch().is_ascii_digit() {
            self.advance();
        }
        // Fractional part
        if !self.at_end() && self.ch() == b'.' && self.ch_at(1).is_ascii_digit() {
            self.advance(); // skip .
            while !self.at_end() && self.ch().is_ascii_digit() {
                self.advance();
            }
        }
        // Exponent
        if !self.at_end() && (self.ch() == b'e' || self.ch() == b'E') {
            self.advance();
            if !self.at_end() && (self.ch() == b'+' || self.ch() == b'-') {
                self.advance();
            }
            while !self.at_end() && self.ch().is_ascii_digit() {
                self.advance();
            }
        }

        let text = &self.src[start..self.pos];
        let val = text.parse::<f64>().unwrap_or(f64::NAN);
        Token {
            kind: TokenKind::Number(val),
            span: self.span(start),
        }
    }

    fn scan_hex_number(&mut self, start: usize) -> Token {
        self.pos += 2; // skip 0x
        while !self.at_end() && self.ch().is_ascii_hexdigit() {
            self.advance();
        }
        let hex = &self.src[start + 2..self.pos];
        let val = u64::from_str_radix(hex, 16).unwrap_or(0) as f64;
        Token {
            kind: TokenKind::Number(val),
            span: self.span(start),
        }
    }

    fn scan_oct_number(&mut self, start: usize) -> Token {
        self.pos += 2; // skip 0o
        while !self.at_end() && matches!(self.ch(), b'0'..=b'7') {
            self.advance();
        }
        let oct = &self.src[start + 2..self.pos];
        let val = u64::from_str_radix(oct, 8).unwrap_or(0) as f64;
        Token {
            kind: TokenKind::Number(val),
            span: self.span(start),
        }
    }

    fn scan_bin_number(&mut self, start: usize) -> Token {
        self.pos += 2; // skip 0b
        while !self.at_end() && matches!(self.ch(), b'0' | b'1') {
            self.advance();
        }
        let bin = &self.src[start + 2..self.pos];
        let val = u64::from_str_radix(bin, 2).unwrap_or(0) as f64;
        Token {
            kind: TokenKind::Number(val),
            span: self.span(start),
        }
    }

    fn scan_story_var(&mut self) -> Token {
        let start = self.pos;
        self.advance(); // skip $
        let name_start = self.pos;
        while !self.at_end() && is_ident_continue(self.ch()) {
            self.advance();
        }
        let name = self.src[name_start..self.pos].to_string();
        Token {
            kind: TokenKind::StoryVar(name),
            span: self.span(start),
        }
    }

    fn scan_temp_var(&mut self) -> Token {
        let start = self.pos;
        self.advance(); // skip _
        let name_start = self.pos;
        while !self.at_end() && is_ident_continue(self.ch()) {
            self.advance();
        }
        let name = self.src[name_start..self.pos].to_string();
        Token {
            kind: TokenKind::TempVar(name),
            span: self.span(start),
        }
    }

    /// Check if `_` starts a temp variable (next char must be alpha/ident-start).
    fn is_var_start(&self) -> bool {
        self.pos + 1 < self.bytes.len() && is_ident_start(self.bytes[self.pos + 1])
    }

    fn scan_ident_or_keyword(&mut self) -> Token {
        let start = self.pos;
        while !self.at_end() && is_ident_continue(self.ch()) {
            self.advance();
        }
        let word = &self.src[start..self.pos];
        let kind = match word {
            "is" => TokenKind::KwIs,
            "isnot" => TokenKind::KwIsnot,
            "to" => TokenKind::KwTo,
            "not" => TokenKind::KwNot,
            "and" => TokenKind::KwAnd,
            "or" => TokenKind::KwOr,
            "eq" => TokenKind::KwEq,
            "neq" => TokenKind::KwNeq,
            "lt" => TokenKind::KwLt,
            "lte" => TokenKind::KwLte,
            "gt" => TokenKind::KwGt,
            "gte" => TokenKind::KwGte,
            "def" => TokenKind::KwDef,
            "ndef" => TokenKind::KwNdef,
            "true" => TokenKind::KwTrue,
            "false" => TokenKind::KwFalse,
            "null" => TokenKind::KwNull,
            "undefined" => TokenKind::KwUndefined,
            "new" => TokenKind::KwNew,
            "delete" => TokenKind::KwDelete,
            "typeof" => TokenKind::KwTypeof,
            "instanceof" => TokenKind::KwInstanceof,
            "in" => TokenKind::KwIn,
            "clone" => TokenKind::KwClone,
            "void" => TokenKind::KwVoid,
            _ => TokenKind::Ident(word.to_string()),
        };
        Token {
            kind,
            span: self.span(start),
        }
    }
}

fn is_ident_start(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_' || c == b'$'
}

fn is_ident_continue(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c == b'$'
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(src: &str) -> Vec<TokenKind> {
        let mut lex = ExprLexer::new(src, 0);
        let mut tokens = Vec::new();
        loop {
            let tok = lex.next_tok();
            if tok.kind == TokenKind::Eof {
                break;
            }
            tokens.push(tok.kind);
        }
        tokens
    }

    #[test]
    fn simple_number() {
        assert_eq!(tokenize("42"), vec![TokenKind::Number(42.0)]);
    }

    #[test]
    fn float_number() {
        assert_eq!(tokenize("3.25"), vec![TokenKind::Number(3.25)]);
    }

    #[test]
    fn hex_number() {
        assert_eq!(tokenize("0xFF"), vec![TokenKind::Number(255.0)]);
    }

    #[test]
    fn string_double() {
        assert_eq!(
            tokenize(r#""hello""#),
            vec![TokenKind::Str("hello".into())]
        );
    }

    #[test]
    fn string_single() {
        assert_eq!(
            tokenize("'world'"),
            vec![TokenKind::Str("world".into())]
        );
    }

    #[test]
    fn string_escapes() {
        assert_eq!(
            tokenize(r#""a\nb""#),
            vec![TokenKind::Str("a\nb".into())]
        );
    }

    #[test]
    fn story_variable() {
        assert_eq!(
            tokenize("$foo"),
            vec![TokenKind::StoryVar("foo".into())]
        );
    }

    #[test]
    fn temp_variable() {
        assert_eq!(
            tokenize("_bar"),
            vec![TokenKind::TempVar("bar".into())]
        );
    }

    #[test]
    fn sugarcube_keywords() {
        assert_eq!(
            tokenize("is isnot to not and or"),
            vec![
                TokenKind::KwIs,
                TokenKind::KwIsnot,
                TokenKind::KwTo,
                TokenKind::KwNot,
                TokenKind::KwAnd,
                TokenKind::KwOr,
            ]
        );
    }

    #[test]
    fn comparison_keywords() {
        assert_eq!(
            tokenize("eq neq lt lte gt gte"),
            vec![
                TokenKind::KwEq,
                TokenKind::KwNeq,
                TokenKind::KwLt,
                TokenKind::KwLte,
                TokenKind::KwGt,
                TokenKind::KwGte,
            ]
        );
    }

    #[test]
    fn js_keywords() {
        assert_eq!(
            tokenize("true false null undefined new delete typeof"),
            vec![
                TokenKind::KwTrue,
                TokenKind::KwFalse,
                TokenKind::KwNull,
                TokenKind::KwUndefined,
                TokenKind::KwNew,
                TokenKind::KwDelete,
                TokenKind::KwTypeof,
            ]
        );
    }

    #[test]
    fn operators() {
        assert_eq!(
            tokenize("+ - * / % **"),
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Percent,
                TokenKind::StarStar,
            ]
        );
    }

    #[test]
    fn comparison_operators() {
        assert_eq!(
            tokenize("== != === !=="),
            vec![
                TokenKind::EqEq,
                TokenKind::NotEq,
                TokenKind::EqEqEq,
                TokenKind::NotEqEq,
            ]
        );
    }

    #[test]
    fn assignment_operators() {
        assert_eq!(
            tokenize("= += -= *= /="),
            vec![
                TokenKind::Eq,
                TokenKind::PlusEq,
                TokenKind::MinusEq,
                TokenKind::StarEq,
                TokenKind::SlashEq,
            ]
        );
    }

    #[test]
    fn increment_decrement() {
        assert_eq!(
            tokenize("++ --"),
            vec![TokenKind::PlusPlus, TokenKind::MinusMinus]
        );
    }

    #[test]
    fn logical_operators() {
        assert_eq!(
            tokenize("&& || ! ??"),
            vec![
                TokenKind::AmpAmp,
                TokenKind::PipePipe,
                TokenKind::Bang,
                TokenKind::QuestionQuestion,
            ]
        );
    }

    #[test]
    fn brackets_and_parens() {
        assert_eq!(
            tokenize("( ) [ ] { }"),
            vec![
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LBracket,
                TokenKind::RBracket,
                TokenKind::LBrace,
                TokenKind::RBrace,
            ]
        );
    }

    #[test]
    fn arrow_function() {
        assert_eq!(
            tokenize("=>"),
            vec![TokenKind::Arrow]
        );
    }

    #[test]
    fn double_gt_terminates_at_depth_zero() {
        // >> at depth 0 should produce EOF (terminate macro)
        let tokens = tokenize("$x >> more");
        assert_eq!(
            tokens,
            vec![TokenKind::StoryVar("x".into())]
        );
    }

    #[test]
    fn double_gt_at_depth_produces_shift() {
        // >> inside parens is right-shift
        let tokens = tokenize("($x >> 1)");
        assert_eq!(
            tokens,
            vec![
                TokenKind::LParen,
                TokenKind::StoryVar("x".into()),
                TokenKind::GreaterGreater,
                TokenKind::Number(1.0),
                TokenKind::RParen,
            ]
        );
    }

    #[test]
    fn template_no_interpolation() {
        assert_eq!(
            tokenize("`hello world`"),
            vec![TokenKind::TemplateFull("hello world".into())]
        );
    }

    #[test]
    fn template_with_interpolation() {
        let tokens = tokenize("`hello ${name}!`");
        assert_eq!(
            tokens,
            vec![
                TokenKind::TemplateStart("hello ".into()),
                TokenKind::Ident("name".into()),
                TokenKind::TemplateEnd("!".into()),
            ]
        );
    }

    #[test]
    fn spread_operator() {
        assert_eq!(tokenize("..."), vec![TokenKind::DotDotDot]);
    }

    #[test]
    fn complex_expression() {
        // $enemyarousal gte ($enemyarousalmax / 5) * 4
        let tokens = tokenize("$enemyarousal gte ($enemyarousalmax / 5) * 4");
        assert_eq!(
            tokens,
            vec![
                TokenKind::StoryVar("enemyarousal".into()),
                TokenKind::KwGte,
                TokenKind::LParen,
                TokenKind::StoryVar("enemyarousalmax".into()),
                TokenKind::Slash,
                TokenKind::Number(5.0),
                TokenKind::RParen,
                TokenKind::Star,
                TokenKind::Number(4.0),
            ]
        );
    }

    #[test]
    fn block_comment_skipped() {
        assert_eq!(
            tokenize("a /* skip me */ b"),
            vec![
                TokenKind::Ident("a".into()),
                TokenKind::Ident("b".into()),
            ]
        );
    }

    #[test]
    fn line_comment_skipped() {
        assert_eq!(
            tokenize("a // skip me\nb"),
            vec![
                TokenKind::Ident("a".into()),
                TokenKind::Ident("b".into()),
            ]
        );
    }

    #[test]
    fn ternary_tokens() {
        assert_eq!(
            tokenize("x ? 1 : 2"),
            vec![
                TokenKind::Ident("x".into()),
                TokenKind::Question,
                TokenKind::Number(1.0),
                TokenKind::Colon,
                TokenKind::Number(2.0),
            ]
        );
    }

    #[test]
    fn member_access_chain() {
        assert_eq!(
            tokenize("$obj.prop.sub"),
            vec![
                TokenKind::StoryVar("obj".into()),
                TokenKind::Dot,
                TokenKind::Ident("prop".into()),
                TokenKind::Dot,
                TokenKind::Ident("sub".into()),
            ]
        );
    }
}
