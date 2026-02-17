//! Recursive descent parser for Harlowe passage content.
//!
//! Scans passage source for `(macro: args)`, `[hooks]`, `[[links]]`,
//! variable interpolation (`$var`, `_var`), HTML tags, and plain text.
//! Error recovery: accumulates errors without aborting.

use super::ast::*;
use super::expr;
use super::lexer::ExprLexer;
use super::macros;
use crate::html_util;

/// Parse a Harlowe passage into an AST.
pub fn parse(source: &str) -> PassageAst {
    let mut parser = Parser::new(source);
    let body = parser.parse_body(false);
    PassageAst {
        body,
        errors: parser.errors,
    }
}

struct Parser<'a> {
    source: &'a str,
    bytes: &'a [u8],
    pos: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
            errors: Vec::new(),
        }
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

    fn remaining(&self) -> &str {
        &self.source[self.pos..]
    }

    /// Lookahead: does the `(` at current position start a macro (`(name:…)`)?
    /// Returns false for prose like `(obviously)`.
    fn looks_like_macro(&self) -> bool {
        let mut i = self.pos + 1; // skip `(`
        // Scan past name chars
        while i < self.bytes.len()
            && (self.bytes[i].is_ascii_alphanumeric()
                || self.bytes[i] == b'-'
                || self.bytes[i] == b'_')
        {
            i += 1;
        }
        // Empty name → `(` followed by non-name char, could be `($var)` etc.
        if i == self.pos + 1 {
            return true;
        }
        // Skip whitespace
        while i < self.bytes.len() && self.bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        // Must have `:` after name to be a macro
        i < self.bytes.len() && self.bytes[i] == b':'
    }

    fn error(&mut self, span: Span, message: impl Into<String>) {
        self.errors.push(ParseError {
            span,
            message: message.into(),
        });
    }

    /// Parse body content until end of input or a closing `]`.
    /// If `in_hook` is true, stop at `]`.
    fn parse_body(&mut self, in_hook: bool) -> Vec<Node> {
        let mut nodes = Vec::new();

        while !self.at_end() {
            if in_hook && self.peek() == Some(b']') {
                break;
            }

            if let Some(node) = self.parse_node(in_hook) {
                nodes.push(node);
            }
        }

        nodes
    }

    /// Parse a single node from the current position.
    fn parse_node(&mut self, in_hook: bool) -> Option<Node> {
        match self.peek()? {
            b'(' => self.parse_macro(),
            b'[' => {
                if self.peek_at(1) == Some(b'[') {
                    self.parse_link()
                } else {
                    // Bare `[` not after a macro — treat as text
                    let start = self.pos;
                    self.pos += 1;
                    Some(Node {
                        kind: NodeKind::Text("[".to_string()),
                        span: Span::new(start, self.pos),
                    })
                }
            }
            b'$' => self.parse_var_interp(),
            b'_' if self.peek_at(1).is_some_and(|c| c.is_ascii_alphanumeric()) => {
                self.parse_temp_var_interp()
            }
            b'<' => {
                if self.remaining().starts_with("<!--") {
                    self.parse_comment()
                } else if self.peek_at(1).is_some_and(|c| c.is_ascii_alphabetic() || c == b'/') {
                    self.parse_html()
                } else {
                    self.parse_text(in_hook)
                }
            }
            b'\n' => {
                let start = self.pos;
                self.pos += 1;
                Some(Node {
                    kind: NodeKind::LineBreak,
                    span: Span::new(start, self.pos),
                })
            }
            // Markup delimiters
            b'*' if self.peek_at(1) == Some(b'*') => {
                if self.peek_at(2) == Some(b'*') {
                    self.parse_markup_compound("***", "strong", "em", in_hook)
                } else {
                    self.parse_markup("**", "strong", in_hook)
                }
            }
            b'*' if !in_hook || self.peek_at(1).is_some_and(|c| c != b']') => {
                self.parse_markup("*", "em", in_hook)
            }
            b'\'' if self.peek_at(1) == Some(b'\'') => {
                self.parse_markup("''", "strong", in_hook)
            }
            b'/' if self.peek_at(1) == Some(b'/') => {
                self.parse_markup("//", "em", in_hook)
            }
            b'~' if self.peek_at(1) == Some(b'~') => {
                self.parse_markup("~~", "del", in_hook)
            }
            b'^' if self.peek_at(1) == Some(b'^') => {
                self.parse_markup("^^", "sup", in_hook)
            }
            b'`' => self.parse_verbatim(),
            _ => self.parse_text(in_hook),
        }
    }

    /// Parse a macro: `(name: args)` with optional trailing `[hook]`.
    fn parse_macro(&mut self) -> Option<Node> {
        let start = self.pos;
        self.pos += 1; // skip `(`

        // Parse macro name
        let name_start = self.pos;
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos].is_ascii_alphanumeric()
                || self.bytes[self.pos] == b'-'
                || self.bytes[self.pos] == b'_')
        {
            self.pos += 1;
        }
        let name = self.source[name_start..self.pos].to_lowercase();

        if name.is_empty() {
            // Not a macro — could be a parenthesized expression in text, restore
            self.pos = start;
            return self.parse_text(false);
        }

        // Skip whitespace
        self.skip_whitespace();

        // Expect `:` after name (for macros with args) or `)` (no args)
        let args = if self.peek() == Some(b':') {
            self.pos += 1; // skip `:`
            self.skip_whitespace();

            // Parse args up to the matching `)`
            let args_text = self.extract_balanced_args();
            self.parse_macro_args(&name, &args_text, start)
        } else if self.peek() == Some(b')') {
            // No colon — Harlowe macros always require `:`. This is prose like `(obviously)`.
            self.pos += 1; // consume the `)`
            let text = self.source[start..self.pos].to_string();
            return Some(Node {
                kind: NodeKind::Text(text),
                span: Span::new(start, self.pos),
            });
        } else {
            // Not a real macro
            self.pos = start;
            return self.parse_text(false);
        };

        // Expect closing `)`
        if self.peek() == Some(b')') {
            self.pos += 1;
        } else {
            self.error(
                Span::new(start, self.pos),
                format!("unclosed macro '({name}:'"),
            );
        }

        let macro_end = self.pos;

        // Check for attached hook `[...]`
        let hook = self.try_parse_hook(macros::expects_hook(&name));
        let has_hook = hook.is_some();

        // For if-chains: collect else-if / else clauses
        let clauses = if name == "if" || name == "unless" {
            self.parse_if_clauses()
        } else {
            Vec::new()
        };

        let end = if has_hook || !clauses.is_empty() {
            self.pos
        } else {
            macro_end
        };

        Some(Node {
            kind: NodeKind::Macro(MacroNode {
                name,
                args,
                hook,
                clauses,
            }),
            span: Span::new(start, end),
        })
    }

    /// Extract balanced text for macro args up to the matching `)`.
    /// Handles nested `(` `)` and string literals.
    fn extract_balanced_args(&mut self) -> String {
        let start = self.pos;
        let mut depth = 1; // We're inside the outer `(`

        while !self.at_end() && depth > 0 {
            match self.bytes[self.pos] {
                b'(' => {
                    depth += 1;
                    self.pos += 1;
                }
                b')' => {
                    depth -= 1;
                    if depth > 0 {
                        self.pos += 1;
                    }
                    // Don't advance past the final `)` — caller handles it
                }
                b'"' | b'\'' => {
                    let quote = self.bytes[self.pos];
                    self.pos += 1;
                    while !self.at_end() && self.bytes[self.pos] != quote {
                        if self.bytes[self.pos] == b'\\' {
                            self.pos += 1; // skip escape
                        }
                        self.pos += 1;
                    }
                    if !self.at_end() {
                        self.pos += 1; // skip closing quote
                    }
                }
                _ => self.pos += 1,
            }
        }

        self.source[start..self.pos].to_string()
    }

    /// Parse macro arguments from extracted text.
    fn parse_macro_args(&mut self, _name: &str, args_text: &str, base_offset: usize) -> Vec<Expr> {
        if args_text.trim().is_empty() {
            return Vec::new();
        }
        let mut lexer = ExprLexer::new(args_text, base_offset);
        expr::parse_args(&mut lexer)
    }

    /// Try to parse a hook `[...]` immediately after a macro.
    /// When `macro_expects_hook` is true, a leading `[[` inside the hook is
    /// parsed as a link rather than rejecting the entire bracket as a non-hook.
    fn try_parse_hook(&mut self, macro_expects_hook: bool) -> Option<Vec<Node>> {
        if self.peek() != Some(b'[') {
            return None;
        }
        // Don't confuse `[[link]]` with hook — unless a macro that expects a hook precedes us
        if !macro_expects_hook && self.peek_at(1) == Some(b'[') {
            return None;
        }
        self.pos += 1; // skip `[`
        let body = self.parse_body(true);
        if self.peek() == Some(b']') {
            self.pos += 1; // skip `]`
        }
        Some(body)
    }

    /// Parse else-if / else clauses that follow an if-block.
    fn parse_if_clauses(&mut self) -> Vec<IfClause> {
        let mut clauses = Vec::new();

        loop {
            self.skip_whitespace();

            // Check for `(else-if:` or `(else:` or `(elseif:`
            if self.peek() != Some(b'(') {
                break;
            }

            let saved = self.pos;

            // Peek ahead to see if this is an else-if or else macro
            self.pos += 1;
            let name_start = self.pos;
            while self.pos < self.bytes.len()
                && (self.bytes[self.pos].is_ascii_alphanumeric()
                    || self.bytes[self.pos] == b'-'
                    || self.bytes[self.pos] == b'_')
            {
                self.pos += 1;
            }
            let name = self.source[name_start..self.pos].to_lowercase();

            if !macros::is_if_clause(&name) {
                self.pos = saved;
                break;
            }

            self.skip_whitespace();

            // Parse args (for else-if) or skip colon (for else)
            let cond = if self.peek() == Some(b':') {
                self.pos += 1;
                self.skip_whitespace();
                if name == "else" {
                    None
                } else {
                    let args_text = self.extract_balanced_args();
                    if args_text.trim().is_empty() {
                        None
                    } else {
                        let mut lexer = ExprLexer::new(&args_text, saved);
                        Some(expr::parse_expr(&mut lexer))
                    }
                }
            } else {
                None
            };

            // Close paren
            if self.peek() == Some(b')') {
                self.pos += 1;
            }

            // Parse hook — else-if/else always expect a hook
            let body = self.try_parse_hook(true).unwrap_or_default();

            clauses.push(IfClause {
                kind: name,
                cond,
                body,
            });

            // After `(else:)[...]`, stop looking for more clauses
            if clauses.last().is_some_and(|c| c.kind == "else") {
                break;
            }
        }

        clauses
    }

    /// Parse a `[[text->passage]]` or `[[passage]]` link.
    fn parse_link(&mut self) -> Option<Node> {
        let start = self.pos;
        self.pos += 2; // skip `[[`

        // Find the closing `]]`
        let content_start = self.pos;
        let mut depth = 0;
        while !self.at_end() {
            if self.pos + 1 < self.bytes.len()
                && self.bytes[self.pos] == b']'
                && self.bytes[self.pos + 1] == b']'
                && depth == 0
            {
                break;
            }
            if self.bytes[self.pos] == b'[' {
                depth += 1;
            }
            if self.bytes[self.pos] == b']' && depth > 0 {
                depth -= 1;
            }
            self.pos += 1;
        }

        let content = &self.source[content_start..self.pos];

        // Skip closing `]]`
        if self.pos + 1 < self.bytes.len()
            && self.bytes[self.pos] == b']'
            && self.bytes[self.pos + 1] == b']'
        {
            self.pos += 2;
        }

        // Parse link content: `text->passage` or `passage<-text` or just `passage`
        let (text, passage) = if let Some(arrow_pos) = content.find("->") {
            let text = content[..arrow_pos].trim().to_string();
            let passage = content[arrow_pos + 2..].trim().to_string();
            (text, passage)
        } else if let Some(arrow_pos) = content.find("<-") {
            let passage = content[..arrow_pos].trim().to_string();
            let text = content[arrow_pos + 2..].trim().to_string();
            (text, passage)
        } else {
            // [[passage]] — text and target are the same
            let name = content.trim().to_string();
            (name.clone(), name)
        };

        Some(Node {
            kind: NodeKind::Link(LinkNode { text, passage }),
            span: Span::new(start, self.pos),
        })
    }

    /// Parse a story variable interpolation: `$name`.
    fn parse_var_interp(&mut self) -> Option<Node> {
        let start = self.pos;
        self.pos += 1; // skip `$`
        let name_start = self.pos;
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos].is_ascii_alphanumeric() || self.bytes[self.pos] == b'_')
        {
            self.pos += 1;
        }

        if self.pos == name_start {
            // Just a `$` character, not a variable
            return Some(Node {
                kind: NodeKind::Text("$".to_string()),
                span: Span::new(start, self.pos),
            });
        }

        let name = self.source[name_start..self.pos].to_string();

        // Check for $var[hook] pattern (but not $var[[link]])
        if self.peek() == Some(b'[') && self.peek_at(1) != Some(b'[') {
            self.pos += 1; // skip [
            let hook = self.parse_body(true);
            if self.peek() == Some(b']') {
                self.pos += 1;
            }
            return Some(Node {
                kind: NodeKind::ChangerApply {
                    name: format!("${name}"),
                    hook,
                },
                span: Span::new(start, self.pos),
            });
        }

        Some(Node {
            kind: NodeKind::VarInterp(format!("${name}")),
            span: Span::new(start, self.pos),
        })
    }

    /// Parse a temp variable interpolation: `_name`.
    fn parse_temp_var_interp(&mut self) -> Option<Node> {
        let start = self.pos;
        self.pos += 1; // skip `_`
        let name_start = self.pos;
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos].is_ascii_alphanumeric() || self.bytes[self.pos] == b'_')
        {
            self.pos += 1;
        }
        let name = self.source[name_start..self.pos].to_string();

        // Check for _var[hook] pattern (but not _var[[link]])
        if self.peek() == Some(b'[') && self.peek_at(1) != Some(b'[') {
            self.pos += 1; // skip [
            let hook = self.parse_body(true);
            if self.peek() == Some(b']') {
                self.pos += 1;
            }
            return Some(Node {
                kind: NodeKind::ChangerApply {
                    name: format!("_{name}"),
                    hook,
                },
                span: Span::new(start, self.pos),
            });
        }

        Some(Node {
            kind: NodeKind::VarInterp(format!("_{name}")),
            span: Span::new(start, self.pos),
        })
    }

    /// Parse an HTML comment `<!-- ... -->`.
    fn parse_comment(&mut self) -> Option<Node> {
        let start = self.pos;
        self.pos += 4; // skip `<!--`
        let content_start = self.pos;
        while !self.at_end() && !self.remaining().starts_with("-->") {
            self.pos += 1;
        }
        let _content = &self.source[content_start..self.pos];
        if self.remaining().starts_with("-->") {
            self.pos += 3;
        }
        // Comments are silently discarded
        Some(Node {
            kind: NodeKind::Text(String::new()),
            span: Span::new(start, self.pos),
        })
    }

    /// Parse an HTML tag into a structured AST node.
    fn parse_html(&mut self) -> Option<Node> {
        let start = self.pos;

        // Scan from `<` to `>`, handling quoted attributes.
        self.pos += 1; // skip `<`
        while !self.at_end() && self.bytes[self.pos] != b'>' {
            if self.bytes[self.pos] == b'"' || self.bytes[self.pos] == b'\'' {
                let q = self.bytes[self.pos];
                self.pos += 1;
                while !self.at_end() && self.bytes[self.pos] != q {
                    self.pos += 1;
                }
                if !self.at_end() {
                    self.pos += 1;
                }
            } else {
                self.pos += 1;
            }
        }
        if !self.at_end() {
            self.pos += 1; // skip `>`
        }

        let raw = &self.source[start..self.pos];
        let span = Span::new(start, self.pos);

        // Tokenize with html5ever for spec-compliant parsing
        let kind = if let Some(info) = html_util::tokenize_html_tag(raw) {
            if info.is_end {
                NodeKind::HtmlClose(info.name)
            } else if info.is_void {
                NodeKind::HtmlVoid {
                    tag: info.name,
                    attrs: info.attrs,
                }
            } else {
                NodeKind::HtmlOpen {
                    tag: info.name,
                    attrs: info.attrs,
                }
            }
        } else {
            // Fallback: emit as text if html5ever can't parse it
            NodeKind::Text(raw.to_string())
        };

        Some(Node { kind, span })
    }

    /// Parse inline markup like `**bold**`, `//italic//`, etc.
    /// Returns the Markup node on success, or falls back to text on failure.
    fn parse_markup(&mut self, delimiter: &str, tag: &str, in_hook: bool) -> Option<Node> {
        let start = self.pos;
        let delim_len = delimiter.len();
        self.pos += delim_len; // skip opening delimiter

        // Parse body until closing delimiter or end of line
        let mut body = Vec::new();
        while !self.at_end() {
            // Check for closing delimiter
            if self.remaining().starts_with(delimiter) {
                self.pos += delim_len;
                return Some(Node {
                    kind: NodeKind::Markup {
                        tag: tag.to_string(),
                        body,
                    },
                    span: Span::new(start, self.pos),
                });
            }
            // End of line = no closing delimiter found on this line
            if self.peek() == Some(b'\n') {
                break;
            }
            // Stop at hook boundary
            if in_hook && self.peek() == Some(b']') {
                break;
            }
            if let Some(node) = self.parse_node(in_hook) {
                body.push(node);
            }
        }

        // No closing delimiter found — treat opening as literal text
        self.pos = start;
        self.parse_text(in_hook)
    }

    /// Parse compound markup like `***bold+italic***` → nested Markup nodes.
    fn parse_markup_compound(
        &mut self,
        delimiter: &str,
        outer_tag: &str,
        inner_tag: &str,
        in_hook: bool,
    ) -> Option<Node> {
        let start = self.pos;
        let delim_len = delimiter.len();
        self.pos += delim_len; // skip opening delimiter

        // Parse body until closing delimiter or end of line
        let mut body = Vec::new();
        while !self.at_end() {
            if self.remaining().starts_with(delimiter) {
                self.pos += delim_len;
                // Wrap as outer[inner[body]]
                let inner = Node {
                    kind: NodeKind::Markup {
                        tag: inner_tag.to_string(),
                        body,
                    },
                    span: Span::new(start, self.pos),
                };
                return Some(Node {
                    kind: NodeKind::Markup {
                        tag: outer_tag.to_string(),
                        body: vec![inner],
                    },
                    span: Span::new(start, self.pos),
                });
            }
            if self.peek() == Some(b'\n') {
                break;
            }
            if in_hook && self.peek() == Some(b']') {
                break;
            }
            if let Some(node) = self.parse_node(in_hook) {
                body.push(node);
            }
        }

        // No closing delimiter — treat as literal text
        self.pos = start;
        self.parse_text(in_hook)
    }

    /// Parse a backtick-delimited verbatim span: `` `...` ``
    /// Everything between backticks is literal text — `]`, `[`, `(` etc. are not structural.
    fn parse_verbatim(&mut self) -> Option<Node> {
        let start = self.pos;
        self.pos += 1; // skip opening `
        while !self.at_end() && self.bytes[self.pos] != b'`' {
            self.pos += 1;
        }
        if !self.at_end() {
            self.pos += 1; // skip closing `
        }
        let text = self.source[start..self.pos].to_string();
        Some(Node {
            kind: NodeKind::Text(text),
            span: Span::new(start, self.pos),
        })
    }

    /// Parse a run of plain text until a special character.
    fn parse_text(&mut self, in_hook: bool) -> Option<Node> {
        let start = self.pos;

        while !self.at_end() {
            let ch = self.bytes[self.pos];
            match ch {
                b'(' if self.looks_like_macro() => break,
                b'$' | b'<' | b'\n' => break,
                b'[' => {
                    if self.peek_at(1) == Some(b'[') {
                        break; // `[[` link
                    }
                    if in_hook {
                        break; // nested hook boundary
                    }
                    self.pos += 1;
                }
                b']' if in_hook => break,
                b'_' if self.peek_at(1).is_some_and(|c| c.is_ascii_alphanumeric()) => break,
                // Markup delimiters
                b'*' => break,
                b'\'' if self.peek_at(1) == Some(b'\'') => break,
                b'/' if self.peek_at(1) == Some(b'/') => break,
                b'~' if self.peek_at(1) == Some(b'~') => break,
                b'^' if self.peek_at(1) == Some(b'^') => break,
                b'`' => break,
                _ => self.pos += 1,
            }
        }

        if self.pos == start {
            // Couldn't consume anything — advance one byte to avoid infinite loop
            self.pos += 1;
            let text = self.source[start..self.pos].to_string();
            return Some(Node {
                kind: NodeKind::Text(text),
                span: Span::new(start, self.pos),
            });
        }

        let text = self.source[start..self.pos].to_string();
        Some(Node {
            kind: NodeKind::Text(text),
            span: Span::new(start, self.pos),
        })
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos] == b' ' || self.bytes[self.pos] == b'\t')
        {
            self.pos += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plain_text() {
        let ast = parse("Hello world");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        assert!(matches!(&ast.body[0].kind, NodeKind::Text(s) if s == "Hello world"));
    }

    #[test]
    fn test_simple_macro() {
        let ast = parse("(set: $x to 1)");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "set");
            assert_eq!(m.args.len(), 1);
            assert!(matches!(&m.args[0].kind, ExprKind::Assign { .. }));
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn test_macro_with_hook() {
        let ast = parse("(color: green)[Hello]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "color");
            assert!(m.hook.is_some());
            let hook = m.hook.as_ref().unwrap();
            assert_eq!(hook.len(), 1);
            assert!(matches!(&hook[0].kind, NodeKind::Text(s) if s == "Hello"));
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn test_link() {
        let ast = parse("[[Start game->new game check]]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::Link(l) = &ast.body[0].kind {
            assert_eq!(l.text, "Start game");
            assert_eq!(l.passage, "new game check");
        } else {
            panic!("expected link, got {:?}", ast.body[0].kind);
        }
    }

    #[test]
    fn test_link_simple() {
        let ast = parse("[[Credits]]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::Link(l) = &ast.body[0].kind {
            assert_eq!(l.text, "Credits");
            assert_eq!(l.passage, "Credits");
        } else {
            panic!("expected link");
        }
    }

    #[test]
    fn test_var_interp() {
        let ast = parse("Score: $totalTF points");
        assert!(ast.errors.is_empty());
        // Should produce: Text("Score: "), VarInterp("$totalTF"), Text(" points")
        assert!(ast.body.len() >= 3);
        assert!(matches!(&ast.body[1].kind, NodeKind::VarInterp(s) if s == "$totalTF"));
    }

    #[test]
    fn test_if_else_chain() {
        let ast = parse("(if: $x is 1)[yes](else:)[no]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "if");
            assert!(m.hook.is_some());
            assert_eq!(m.clauses.len(), 1);
            assert_eq!(m.clauses[0].kind, "else");
            assert!(m.clauses[0].cond.is_none());
        } else {
            panic!("expected if macro");
        }
    }

    #[test]
    fn test_nested_macros() {
        let ast = parse("(if: $x is 1)[(set: $y to 2)]");
        assert_eq!(ast.errors.len(), 0);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "if");
            let hook = m.hook.as_ref().unwrap();
            assert_eq!(hook.len(), 1);
            assert!(matches!(&hook[0].kind, NodeKind::Macro(inner) if inner.name == "set"));
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn test_multiple_nodes() {
        let ast = parse("Hello (set: $x to 1) world");
        assert_eq!(ast.errors.len(), 0);
        assert!(ast.body.len() >= 3);
    }

    #[test]
    fn test_html_void() {
        let ast = parse("<img src=\"test.png\" width=\"400px\">");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::HtmlVoid { tag, attrs } = &ast.body[0].kind {
            assert_eq!(tag, "img");
            assert_eq!(attrs.len(), 2);
        } else {
            panic!("expected HtmlVoid, got {:?}", ast.body[0].kind);
        }
    }

    #[test]
    fn test_html_open_close() {
        let ast = parse("<table><tr><td>cell</td></tr></table>");
        assert!(ast.errors.is_empty());
        // Should produce: HtmlOpen(table), HtmlOpen(tr), HtmlOpen(td), Text, HtmlClose(td), HtmlClose(tr), HtmlClose(table)
        assert!(ast.body.len() >= 7);
        assert!(matches!(&ast.body[0].kind, NodeKind::HtmlOpen { tag, .. } if tag == "table"));
        assert!(matches!(&ast.body[3].kind, NodeKind::Text(s) if s == "cell"));
        assert!(matches!(&ast.body[4].kind, NodeKind::HtmlClose(tag) if tag == "td"));
    }

    #[test]
    fn test_goto() {
        let ast = parse("(goto: \"Event 3-check\")");
        assert_eq!(ast.errors.len(), 0);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "goto");
            assert_eq!(m.args.len(), 1);
            assert!(matches!(&m.args[0].kind, ExprKind::Str(s) if s == "Event 3-check"));
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn test_color_plus() {
        let ast = parse("(color: magenta+white)[text]");
        assert_eq!(ast.errors.len(), 0);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "color");
            assert_eq!(m.args.len(), 1);
            assert!(
                matches!(&m.args[0].kind, ExprKind::ColorLiteral(s) if s == "magenta+white")
            );
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn test_complex_passage() {
        let src = r#"(set: $recovery to "Floor 1 entryway")
You're at the **entryway**

(if: $hypnoStat < 70)[Normal text.](else:)[(color: magenta+white)[Hypno text.]]"#;
        let ast = parse(src);
        // Should parse without crashing — exact node count depends on line breaks etc.
        assert!(ast.errors.is_empty(), "errors: {:?}", ast.errors);
        assert!(!ast.body.is_empty());
    }

    #[test]
    fn test_link_macro_in_hook() {
        let ast = parse("(link: \"Continue\")[(goto: $recover)]");
        assert_eq!(ast.errors.len(), 0);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "link");
            assert!(m.hook.is_some());
            let hook = m.hook.as_ref().unwrap();
            assert_eq!(hook.len(), 1);
            if let NodeKind::Macro(inner) = &hook[0].kind {
                assert_eq!(inner.name, "goto");
            } else {
                panic!("expected inner macro");
            }
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn test_live_stop() {
        let ast = parse("(live: 2s)[(stop:)]");
        assert_eq!(ast.errors.len(), 0);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "live");
            assert_eq!(m.args.len(), 1);
            assert!(matches!(m.args[0].kind, ExprKind::TimeLiteral(t) if t == 2.0));
            assert!(m.hook.is_some());
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn test_hook_containing_link() {
        // A macro that expects a hook should parse `[[[link->target]]]` as a hook
        // containing a link, not reject the `[` because the next char is `[`.
        let ast = parse("(transition: \"dissolve\")[[[Start game->new game check]]]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "transition");
            let hook = m.hook.as_ref().expect("transition should have a hook");
            assert_eq!(hook.len(), 1);
            assert!(
                matches!(&hook[0].kind, NodeKind::Link(l) if l.text == "Start game" && l.passage == "new game check"),
                "expected link inside hook, got {:?}",
                hook[0].kind
            );
        } else {
            panic!("expected macro, got {:?}", ast.body[0].kind);
        }
    }

    #[test]
    fn test_bare_bracket_link_not_hook() {
        // Without a preceding macro, `[[link]]` should still be parsed as a link, not a hook.
        let ast = parse("[[Credits]]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        assert!(matches!(&ast.body[0].kind, NodeKind::Link(l) if l.passage == "Credits"));
    }

    #[test]
    fn test_verbatim_backtick_in_hook() {
        // Backtick verbatim spans must not break hook parsing.
        // `]` inside backticks is literal, not a hook closer.
        let ast = parse("(if: true)[`[X]`](else:)[`[Y]`]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::Macro(mac) = &ast.body[0].kind {
            assert_eq!(mac.name, "if");
            // Main hook should contain the verbatim text
            let hook = mac.hook.as_ref().unwrap();
            assert_eq!(hook.len(), 1);
            assert!(matches!(&hook[0].kind, NodeKind::Text(t) if t == "`[X]`"));
            // Should have one else clause
            assert_eq!(mac.clauses.len(), 1);
            assert_eq!(mac.clauses[0].kind, "else");
            assert_eq!(mac.clauses[0].body.len(), 1);
            assert!(matches!(&mac.clauses[0].body[0].kind, NodeKind::Text(t) if t == "`[Y]`"));
        } else {
            panic!("expected macro node");
        }
    }

    #[test]
    fn test_verbatim_backtick_with_else_if() {
        let ast = parse("(if: $x is 1)[(color: red)[`[A]`]](else-if: $x is 2)[(color: blue)[`[B]`]](else:)[`[C]`]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::Macro(mac) = &ast.body[0].kind {
            assert_eq!(mac.name, "if");
            assert_eq!(mac.clauses.len(), 2, "should have else-if + else clauses");
            assert_eq!(mac.clauses[0].kind, "else-if");
            assert_eq!(mac.clauses[1].kind, "else");
        } else {
            panic!("expected macro node");
        }
    }
}
