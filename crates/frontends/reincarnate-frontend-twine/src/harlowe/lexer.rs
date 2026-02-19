//! Lexer for Harlowe expressions inside `(macro: ...)` argument positions.
//!
//! Much simpler than SugarCube's lexer — no JS template literals or `<<>>`
//! nesting. Handles Harlowe's keyword operators (`is`, `is not`, `contains`,
//! `is in`, `to`, `and`, `or`, `not`), variables (`$x`, `_x`), numbers,
//! strings, time literals (`2s`, `500ms`), and basic punctuation.

use super::ast::Span;

/// A single token in a Harlowe expression.
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// Token types for Harlowe expressions.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Number(f64),
    String(String),
    Ident(String),

    // Variables
    /// `$name`
    StoryVar(String),
    /// `_name`
    TempVar(String),

    // Keywords (operators)
    Is,
    IsNot,
    IsIn,
    Contains,
    And,
    Or,
    Not,
    To,
    True,
    False,
    It,
    /// `each` — lambda binder in `(for:)` loops
    Each,
    /// `where` — filter clause in lambda expressions
    Where,
    /// `...` — spread operator (expands iterables in `(for:)` args)
    Ellipsis,

    // Punctuation
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Lt,
    Gt,
    Lte,
    Gte,
    LParen,
    RParen,
    Comma,
    Colon,
    Apostrophe, // `'s` possessive

    // Time literal
    TimeLiteral(f64), // value in seconds

    // Special
    Eof,
    Error(String),
}

/// Lexer for Harlowe expression arguments.
pub struct ExprLexer<'a> {
    input: &'a str,
    bytes: &'a [u8],
    pub pos: usize,
    pub base_offset: usize,
}

impl<'a> ExprLexer<'a> {
    pub fn new(input: &'a str, base_offset: usize) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            pos: 0,
            base_offset,
        }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    fn at_end(&self) -> bool {
        self.pos >= self.bytes.len()
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.get(self.pos).copied()
    }

    fn peek_at(&self, offset: usize) -> Option<u8> {
        self.bytes.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> u8 {
        let b = self.bytes[self.pos];
        self.pos += 1;
        b
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.bytes.len() && self.bytes[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }
    }

    fn span(&self, start: usize) -> Span {
        Span::new(self.base_offset + start, self.base_offset + self.pos)
    }

    /// Peek at the next token without consuming it.
    pub fn peek_token(&mut self) -> Token {
        let saved = self.pos;
        let tok = self.next_token();
        self.pos = saved;
        tok
    }

    /// Consume and return the next token.
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.at_end() {
            return Token {
                kind: TokenKind::Eof,
                span: self.span(self.pos),
            };
        }

        let start = self.pos;

        match self.peek().unwrap() {
            b'$' => self.lex_story_var(start),
            b'_' if self.peek_at(1).is_some_and(|c| c.is_ascii_alphanumeric()) => {
                self.lex_temp_var(start)
            }
            b'"' | b'\'' => self.lex_string(start),
            b'0'..=b'9' => self.lex_number(start),
            b'+' => {
                self.advance();
                Token {
                    kind: TokenKind::Plus,
                    span: self.span(start),
                }
            }
            b'-' => {
                // Could be negative number or minus operator
                if self.peek_at(1).is_some_and(|c| c.is_ascii_digit()) {
                    // Check if previous meaningful token suggests this is unary
                    // For simplicity, always treat as minus operator and let parser handle
                    self.advance();
                    Token {
                        kind: TokenKind::Minus,
                        span: self.span(start),
                    }
                } else {
                    self.advance();
                    Token {
                        kind: TokenKind::Minus,
                        span: self.span(start),
                    }
                }
            }
            b'*' => {
                self.advance();
                Token {
                    kind: TokenKind::Star,
                    span: self.span(start),
                }
            }
            b'/' => {
                self.advance();
                Token {
                    kind: TokenKind::Slash,
                    span: self.span(start),
                }
            }
            b'%' => {
                self.advance();
                Token {
                    kind: TokenKind::Percent,
                    span: self.span(start),
                }
            }
            b'<' => {
                self.advance();
                if self.peek() == Some(b'=') {
                    self.advance();
                    Token {
                        kind: TokenKind::Lte,
                        span: self.span(start),
                    }
                } else {
                    Token {
                        kind: TokenKind::Lt,
                        span: self.span(start),
                    }
                }
            }
            b'>' => {
                self.advance();
                if self.peek() == Some(b'=') {
                    self.advance();
                    Token {
                        kind: TokenKind::Gte,
                        span: self.span(start),
                    }
                } else {
                    Token {
                        kind: TokenKind::Gt,
                        span: self.span(start),
                    }
                }
            }
            b'(' => {
                self.advance();
                Token {
                    kind: TokenKind::LParen,
                    span: self.span(start),
                }
            }
            b')' => {
                self.advance();
                Token {
                    kind: TokenKind::RParen,
                    span: self.span(start),
                }
            }
            b',' => {
                self.advance();
                Token {
                    kind: TokenKind::Comma,
                    span: self.span(start),
                }
            }
            b':' => {
                self.advance();
                Token {
                    kind: TokenKind::Colon,
                    span: self.span(start),
                }
            }
            b'#' => self.lex_color_hash(start),
            b'.' => {
                // Check for `...` (ellipsis / spread).
                // self.pos is at the first dot (not yet consumed), so check
                // pos+1 and pos+2 for the second and third dots.
                if self.pos + 2 < self.bytes.len()
                    && self.bytes[self.pos + 1] == b'.'
                    && self.bytes[self.pos + 2] == b'.'
                {
                    self.pos += 3; // consume all three dots
                    Token {
                        kind: TokenKind::Ellipsis,
                        span: self.span(start),
                    }
                } else {
                    self.advance();
                    Token {
                        kind: TokenKind::Error("unexpected '.'".to_string()),
                        span: self.span(start),
                    }
                }
            }
            c if c.is_ascii_alphabetic() => self.lex_ident_or_keyword(start),
            _ => {
                self.advance();
                Token {
                    kind: TokenKind::Error(format!(
                        "unexpected character '{}'",
                        self.input[start..self.pos].chars().next().unwrap_or('?')
                    )),
                    span: self.span(start),
                }
            }
        }
    }

    fn lex_story_var(&mut self, start: usize) -> Token {
        self.advance(); // skip $
        let name_start = self.pos;
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos].is_ascii_alphanumeric() || self.bytes[self.pos] == b'_')
        {
            self.pos += 1;
        }
        let name = self.input[name_start..self.pos].to_string();
        Token {
            kind: TokenKind::StoryVar(name),
            span: self.span(start),
        }
    }

    fn lex_temp_var(&mut self, start: usize) -> Token {
        self.advance(); // skip _
        let name_start = self.pos;
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos].is_ascii_alphanumeric() || self.bytes[self.pos] == b'_')
        {
            self.pos += 1;
        }
        let name = self.input[name_start..self.pos].to_string();
        Token {
            kind: TokenKind::TempVar(name),
            span: self.span(start),
        }
    }

    fn lex_string(&mut self, start: usize) -> Token {
        let quote = self.advance();
        let mut value = String::new();
        while !self.at_end() {
            let c = self.advance();
            if c == quote {
                return Token {
                    kind: TokenKind::String(value),
                    span: self.span(start),
                };
            }
            if c == b'\\' && !self.at_end() {
                let esc = self.advance();
                match esc {
                    b'n' => value.push('\n'),
                    b't' => value.push('\t'),
                    b'\\' => value.push('\\'),
                    b'\'' => value.push('\''),
                    b'"' => value.push('"'),
                    _ => {
                        value.push('\\');
                        value.push(esc as char);
                    }
                }
            } else {
                value.push(c as char);
            }
        }
        Token {
            kind: TokenKind::Error("unterminated string".to_string()),
            span: self.span(start),
        }
    }

    fn lex_number(&mut self, start: usize) -> Token {
        while self.pos < self.bytes.len() && self.bytes[self.pos].is_ascii_digit() {
            self.pos += 1;
        }
        // Decimal part
        if self.pos < self.bytes.len()
            && self.bytes[self.pos] == b'.'
            && self.pos + 1 < self.bytes.len()
            && self.bytes[self.pos + 1].is_ascii_digit()
        {
            self.pos += 1; // skip .
            while self.pos < self.bytes.len() && self.bytes[self.pos].is_ascii_digit() {
                self.pos += 1;
            }
        }
        let num_str = &self.input[start..self.pos];
        let value: f64 = num_str.parse().unwrap_or(0.0);

        // Check for time suffix: `s` or `ms`
        if self.pos < self.bytes.len() {
            if self.bytes[self.pos] == b'm'
                && self.pos + 1 < self.bytes.len()
                && self.bytes[self.pos + 1] == b's'
            {
                self.pos += 2;
                return Token {
                    kind: TokenKind::TimeLiteral(value / 1000.0),
                    span: self.span(start),
                };
            }
            if self.bytes[self.pos] == b's'
                && (self.pos + 1 >= self.bytes.len()
                    || !self.bytes[self.pos + 1].is_ascii_alphabetic())
            {
                self.pos += 1;
                return Token {
                    kind: TokenKind::TimeLiteral(value),
                    span: self.span(start),
                };
            }
        }

        // Check for ordinal suffix: `st`, `nd`, `rd`, `th`
        if self.pos + 1 < self.bytes.len() {
            let a = self.bytes[self.pos];
            let b = self.bytes[self.pos + 1];
            if (a == b's' && b == b't')
                || (a == b'n' && b == b'd')
                || (a == b'r' && b == b'd')
                || (a == b't' && b == b'h')
            {
                // Only if not followed by more alpha (so `1stand` doesn't match)
                if self.pos + 2 >= self.bytes.len()
                    || !self.bytes[self.pos + 2].is_ascii_alphabetic()
                {
                    // Return as number; the parser will handle ordinal context
                    // Actually, store it specially via Ident so parser knows
                    let ordinal_str = format!("{}{}{}", num_str, a as char, b as char);
                    self.pos += 2;
                    return Token {
                        kind: TokenKind::Ident(ordinal_str),
                        span: self.span(start),
                    };
                }
            }
        }

        Token {
            kind: TokenKind::Number(value),
            span: self.span(start),
        }
    }

    fn lex_color_hash(&mut self, start: usize) -> Token {
        self.advance(); // skip #
        let hex_start = self.pos;
        while self.pos < self.bytes.len() && self.bytes[self.pos].is_ascii_hexdigit() {
            self.pos += 1;
        }
        let hex = &self.input[hex_start..self.pos];
        if hex.len() == 3 || hex.len() == 6 {
            Token {
                kind: TokenKind::String(format!("#{hex}")),
                span: self.span(start),
            }
        } else {
            Token {
                kind: TokenKind::Error(format!("invalid hex color: #{hex}")),
                span: self.span(start),
            }
        }
    }

    fn lex_ident_or_keyword(&mut self, start: usize) -> Token {
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos].is_ascii_alphanumeric()
                || self.bytes[self.pos] == b'_'
                || self.bytes[self.pos] == b'-')
        {
            self.pos += 1;
        }
        let word = &self.input[start..self.pos];
        let kind = match word {
            "is" => {
                // Check for "is not" or "is in" (with whitespace between)
                let saved = self.pos;
                self.skip_whitespace();
                if self.pos + 3 <= self.bytes.len() && &self.input[self.pos..self.pos + 3] == "not" {
                    let after_not = self.pos + 3;
                    if after_not >= self.bytes.len()
                        || !self.bytes[after_not].is_ascii_alphanumeric()
                    {
                        self.pos = after_not;
                        TokenKind::IsNot
                    } else {
                        self.pos = saved;
                        TokenKind::Is
                    }
                } else if self.pos + 2 <= self.bytes.len()
                    && &self.input[self.pos..self.pos + 2] == "in"
                {
                    let after_in = self.pos + 2;
                    if after_in >= self.bytes.len()
                        || !self.bytes[after_in].is_ascii_alphanumeric()
                    {
                        self.pos = after_in;
                        TokenKind::IsIn
                    } else {
                        self.pos = saved;
                        TokenKind::Is
                    }
                } else {
                    self.pos = saved;
                    TokenKind::Is
                }
            }
            "contains" => TokenKind::Contains,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "to" => TokenKind::To,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "it" => TokenKind::It,
            "each" => TokenKind::Each,
            "where" => TokenKind::Where,
            "last" => TokenKind::Ident("last".to_string()),
            "length" => TokenKind::Ident("length".to_string()),
            _ => TokenKind::Ident(word.to_string()),
        };

        // Check for `'s` possessive after identifiers/variables
        // (handled at parser level rather than lexer level)

        Token {
            kind,
            span: self.span(start),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all(input: &str) -> Vec<TokenKind> {
        let mut lexer = ExprLexer::new(input, 0);
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token();
            if tok.kind == TokenKind::Eof {
                break;
            }
            tokens.push(tok.kind);
        }
        tokens
    }

    #[test]
    fn test_simple_set() {
        let tokens = lex_all("$recovery to \"Floor 1 entryway\"");
        assert_eq!(
            tokens,
            vec![
                TokenKind::StoryVar("recovery".to_string()),
                TokenKind::To,
                TokenKind::String("Floor 1 entryway".to_string()),
            ]
        );
    }

    #[test]
    fn test_is_not() {
        let tokens = lex_all("$event3First is not 0");
        assert_eq!(
            tokens,
            vec![
                TokenKind::StoryVar("event3First".to_string()),
                TokenKind::IsNot,
                TokenKind::Number(0.0),
            ]
        );
    }

    #[test]
    fn test_is_in() {
        let tokens = lex_all("\"x\" is in $arr");
        assert_eq!(
            tokens,
            vec![
                TokenKind::String("x".to_string()),
                TokenKind::IsIn,
                TokenKind::StoryVar("arr".to_string()),
            ]
        );
    }

    #[test]
    fn test_compound_expr() {
        let tokens = lex_all("$x + 1");
        assert_eq!(
            tokens,
            vec![
                TokenKind::StoryVar("x".to_string()),
                TokenKind::Plus,
                TokenKind::Number(1.0),
            ]
        );
    }

    #[test]
    fn test_time_literal() {
        let tokens = lex_all("2s");
        assert_eq!(tokens, vec![TokenKind::TimeLiteral(2.0)]);

        let tokens = lex_all("500ms");
        assert_eq!(tokens, vec![TokenKind::TimeLiteral(0.5)]);
    }

    #[test]
    fn test_boolean_and_it() {
        let tokens = lex_all("true and it");
        assert_eq!(
            tokens,
            vec![TokenKind::True, TokenKind::And, TokenKind::It]
        );
    }

    #[test]
    fn test_comparison() {
        let tokens = lex_all("$x <= 5");
        assert_eq!(
            tokens,
            vec![
                TokenKind::StoryVar("x".to_string()),
                TokenKind::Lte,
                TokenKind::Number(5.0),
            ]
        );
    }

    #[test]
    fn test_color_hash() {
        let tokens = lex_all("#FF00FF");
        assert_eq!(tokens, vec![TokenKind::String("#FF00FF".to_string())]);
    }

    #[test]
    fn test_or_with_string() {
        let tokens = lex_all("$recover is 0 or \"\"");
        assert_eq!(
            tokens,
            vec![
                TokenKind::StoryVar("recover".to_string()),
                TokenKind::Is,
                TokenKind::Number(0.0),
                TokenKind::Or,
                TokenKind::String("".to_string()),
            ]
        );
    }

    #[test]
    fn test_ordinal() {
        let tokens = lex_all("1st");
        assert_eq!(tokens, vec![TokenKind::Ident("1st".to_string())]);
    }

    #[test]
    fn test_temp_var() {
        let tokens = lex_all("_item");
        assert_eq!(tokens, vec![TokenKind::TempVar("item".to_string())]);
    }

    #[test]
    fn test_nested_parens() {
        let tokens = lex_all("(saved-games:) contains \"Slot A\"");
        assert_eq!(
            tokens,
            vec![
                TokenKind::LParen,
                TokenKind::Ident("saved-games".to_string()),
                TokenKind::Colon,
                TokenKind::RParen,
                TokenKind::Contains,
                TokenKind::String("Slot A".to_string()),
            ]
        );
    }
}
