//! Recursive descent parser for SugarCube passage source text.
//!
//! Converts raw passage source into a `PassageAst` of `Node` items:
//! macros (`<<...>>`), links (`[[...]]`), variable interpolation
//! (`$var`, `_tmp`), inline HTML, and plain text.
//!
//! Error recovery: errors are accumulated in `PassageAst.errors` and
//! parsing continues. A single broken passage must not prevent parsing
//! the other 9,999.

use super::ast::*;
use super::expr;
use super::lexer::ExprLexer;
use super::macros::{self, MacroKind};

/// Parse a SugarCube passage source string into an AST.
pub fn parse(source: &str) -> PassageAst {
    let mut parser = Parser::new(source);
    let body = parser.parse_body(&[]);
    PassageAst {
        body,
        errors: parser.errors,
    }
}

struct Parser<'a> {
    src: &'a str,
    bytes: &'a [u8],
    pos: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            bytes: src.as_bytes(),
            pos: 0,
            errors: Vec::new(),
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.bytes.len()
    }

    fn ch(&self) -> u8 {
        self.bytes[self.pos]
    }

    fn remaining(&self) -> &'a str {
        &self.src[self.pos..]
    }

    fn error(&mut self, span: Span, msg: impl Into<String>) {
        self.errors.push(ParseError {
            span,
            message: msg.into(),
        });
    }

    /// Parse passage body until we encounter a closing macro tag or clause tag
    /// that matches one of the `stop_on` names, or end of input.
    ///
    /// `stop_on` contains the macro name(s) we're inside. When we see
    /// `<</name>>` or a clause for that macro, we stop.
    fn parse_body(&mut self, stop_on: &[&str]) -> Vec<Node> {
        let mut nodes = Vec::new();

        while !self.at_end() {
            // Check for closing or clause tags that should stop us
            if !stop_on.is_empty() && self.remaining().starts_with("<</") {
                break;
            }
            if !stop_on.is_empty() && self.remaining().starts_with("<<") {
                if let Some(name) = self.peek_macro_name() {
                    if macros::is_clause(&name) {
                        // Check if this clause belongs to our parent
                        if let Some(parent) = macros::clause_parent(&name) {
                            if stop_on.contains(&parent) {
                                break;
                            }
                        }
                    }
                }
            }

            if let Some(node) = self.parse_node() {
                nodes.push(node);
            }
        }

        nodes
    }

    /// Parse a single node from the current position.
    fn parse_node(&mut self) -> Option<Node> {
        if self.at_end() {
            return None;
        }

        let remaining = self.remaining();

        // <<macro>> or <</macro>>
        if remaining.starts_with("<<") {
            return self.parse_macro_or_close();
        }

        // [[link]]
        if remaining.starts_with("[[") {
            return Some(self.parse_link());
        }

        // /* block comment */
        if remaining.starts_with("/*") {
            return Some(self.parse_block_comment());
        }

        // <!-- HTML comment -->
        if remaining.starts_with("<!--") {
            return Some(self.parse_html_comment());
        }

        // Inline HTML tag
        if remaining.starts_with('<')
            && remaining.len() > 1
            && (remaining.as_bytes()[1].is_ascii_alphabetic()
                || remaining.as_bytes()[1] == b'/')
        {
            return Some(self.parse_html_tag());
        }

        // Variable interpolation: $name or _name (followed by ident char)
        if (self.ch() == b'$' || self.ch() == b'_') && self.pos + 1 < self.bytes.len() {
            let next = self.bytes[self.pos + 1];
            if next.is_ascii_alphabetic() || next == b'_' {
                return Some(self.parse_var_interp());
            }
        }

        // Plain text (consume until next special character)
        Some(self.parse_text())
    }

    /// Peek at the macro name after `<<` without consuming input.
    fn peek_macro_name(&self) -> Option<String> {
        let rest = &self.src[self.pos..];
        if !rest.starts_with("<<") {
            return None;
        }
        let after = &rest[2..];
        // Skip closing slash for <</name>>
        let after = after.strip_prefix('/').unwrap_or(after);
        // Special macros: <<= expr>> and <<- expr>>
        if after.starts_with('=') || after.starts_with('-') {
            return Some(after[..1].to_string());
        }
        let end = after
            .find(|c: char| !c.is_ascii_alphanumeric() && c != '_' && c != '-')
            .unwrap_or(after.len());
        if end == 0 {
            return None;
        }
        Some(after[..end].to_string())
    }

    fn parse_macro_or_close(&mut self) -> Option<Node> {
        let start = self.pos;

        // Check for closing tag <</name>>
        if self.remaining().starts_with("<</") {
            // This is a closing tag — it should have been consumed by the
            // block macro parser. If we see it here, it's an orphaned close.
            let close_name = self.peek_macro_name().unwrap_or_default();
            // Skip past >>
            if let Some(end) = self.src[self.pos..].find(">>") {
                self.pos += end + 2;
            } else {
                self.pos = self.bytes.len();
            }
            self.error(
                Span::new(start, self.pos),
                format!("orphaned closing tag: <</{close_name}>>"),
            );
            return Some(Node {
                kind: NodeKind::Text(self.src[start..self.pos].to_string()),
                span: Span::new(start, self.pos),
            });
        }

        // Opening macro tag: <<name args>>
        self.pos += 2; // skip <<

        // Special: <<= expr>> or <<- expr>>
        let name = if !self.at_end() && (self.ch() == b'=' || self.ch() == b'-') {
            let c = self.ch();
            self.pos += 1;
            String::from(c as char)
        } else {
            self.scan_macro_name()
        };

        if name.is_empty() {
            // Not a valid macro, treat as text
            self.error(
                Span::new(start, self.pos),
                "empty macro name",
            );
            return Some(Node {
                kind: NodeKind::Text(self.src[start..self.pos].to_string()),
                span: Span::new(start, self.pos),
            });
        }

        // Determine macro kind
        let kind = macros::macro_kind(&name);

        match kind {
            Some(MacroKind::SelfClosing) => {
                let args = self.parse_macro_args(&name);
                let end = self.skip_to_close_tag();
                Some(Node {
                    kind: NodeKind::Macro(MacroNode {
                        name,
                        args,
                        clauses: Vec::new(),
                    }),
                    span: Span::new(start, end),
                })
            }
            Some(MacroKind::Block) => self.parse_block_macro(start, &name),
            Some(MacroKind::Raw) => self.parse_raw_macro(start, &name),
            None => {
                // Unknown macro: could be a widget or user-defined macro.
                // Check if it's a clause (should have been caught by parse_body).
                if macros::is_clause(&name) {
                    // Orphaned clause — skip past >>
                    let end = self.skip_to_close_tag();
                    self.error(
                        Span::new(start, end),
                        format!("orphaned clause: <<{name}>>"),
                    );
                    return Some(Node {
                        kind: NodeKind::Text(self.src[start..end].to_string()),
                        span: Span::new(start, end),
                    });
                }
                // Unknown macro: use lookahead to determine block vs self-closing
                self.parse_unknown_macro(start, &name)
            }
        }
    }

    /// Parse arguments for a macro (text between name and `>>`).
    fn parse_macro_args(&mut self, name: &str) -> MacroArgs {
        self.skip_whitespace();

        // Check if we're already at >>
        if self.remaining().starts_with(">>") {
            return MacroArgs::None;
        }

        match name {
            "set" | "unset" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let mut lexer = ExprLexer::new(args_src, self.pos - args_src.len());
                let list = expr::parse_assignment_list(&mut lexer);
                MacroArgs::AssignList(list)
            }
            "run" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let mut lexer = ExprLexer::new(args_src, self.pos - args_src.len());
                let e = expr::parse_expr(&mut lexer);
                MacroArgs::Expr(e)
            }
            "if" | "elseif" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let mut lexer = ExprLexer::new(args_src, self.pos - args_src.len());
                let e = expr::parse_expr(&mut lexer);
                MacroArgs::Expr(e)
            }
            "print" | "=" | "-" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let mut lexer = ExprLexer::new(args_src, self.pos - args_src.len());
                let e = expr::parse_expr(&mut lexer);
                MacroArgs::Expr(e)
            }
            "switch" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let mut lexer = ExprLexer::new(args_src, self.pos - args_src.len());
                let e = expr::parse_expr(&mut lexer);
                MacroArgs::Switch(e)
            }
            "case" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let mut lexer = ExprLexer::new(args_src, self.pos - args_src.len());
                let mut values = vec![expr::parse_single_expr(&mut lexer)];
                while lexer.peek().kind != super::lexer::TokenKind::Eof {
                    values.push(expr::parse_single_expr(&mut lexer));
                }
                MacroArgs::CaseValues(values)
            }
            "for" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                self.parse_for_args(args_src)
            }
            "widget" => {
                let args_src = self.capture_args_src();
                let trimmed = args_src.trim();
                // Widget name may be quoted or bare
                let widget_name = if (trimmed.starts_with('"') && trimmed.ends_with('"'))
                    || (trimmed.starts_with('\'') && trimmed.ends_with('\''))
                {
                    trimmed[1..trimmed.len() - 1].to_string()
                } else {
                    trimmed.to_string()
                };
                MacroArgs::WidgetDef { name: widget_name }
            }
            "link" | "linkappend" | "linkprepend" | "linkreplace" | "button" => {
                self.parse_link_macro_args()
            }
            "capture" => {
                // <<capture $var1, _var2, ...>>
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let mut lexer = ExprLexer::new(args_src, self.pos - args_src.len());
                let list = expr::parse_assignment_list(&mut lexer);
                MacroArgs::AssignList(list)
            }
            _ => {
                // Generic: parse as expression if possible, or raw
                let args_src = self.capture_args_src();
                let trimmed = args_src.trim();
                if trimmed.is_empty() {
                    MacroArgs::None
                } else {
                    let mut lexer = ExprLexer::new(args_src, self.pos - args_src.len());
                    let e = expr::parse_expr(&mut lexer);
                    if matches!(e.kind, ExprKind::Error(_)) {
                        MacroArgs::Raw(args_src.to_string())
                    } else {
                        MacroArgs::Expr(e)
                    }
                }
            }
        }
    }

    /// Capture the raw argument source text between current pos and `>>`.
    /// Advances pos past the args but NOT past `>>`.
    fn capture_args_src(&mut self) -> &'a str {
        let start = self.pos;
        let mut depth = 0i32;

        while !self.at_end() {
            let ch = self.ch();
            match ch {
                b'(' | b'[' | b'{' => {
                    depth += 1;
                    self.pos += 1;
                }
                b')' | b']' | b'}' => {
                    depth -= 1;
                    self.pos += 1;
                }
                b'>' if depth <= 0
                    && self.pos + 1 < self.bytes.len()
                    && self.bytes[self.pos + 1] == b'>' =>
                {
                    break;
                }
                b'"' | b'\'' => {
                    self.skip_string(ch);
                }
                b'`' => {
                    self.skip_template();
                }
                b'/' if self.pos + 1 < self.bytes.len() && self.bytes[self.pos + 1] == b'/' => {
                    // Line comment — skip to end of line
                    while !self.at_end() && self.ch() != b'\n' {
                        self.pos += 1;
                    }
                }
                b'/' if self.pos + 1 < self.bytes.len() && self.bytes[self.pos + 1] == b'*' => {
                    // Block comment — skip to */
                    self.pos += 2;
                    while !self.at_end() {
                        if self.ch() == b'*'
                            && self.pos + 1 < self.bytes.len()
                            && self.bytes[self.pos + 1] == b'/'
                        {
                            self.pos += 2;
                            break;
                        }
                        self.pos += 1;
                    }
                }
                _ => self.pos += 1,
            }
        }
        &self.src[start..self.pos]
    }

    fn skip_string(&mut self, quote: u8) {
        self.pos += 1; // skip opening quote
        while !self.at_end() {
            let c = self.ch();
            if c == quote {
                self.pos += 1;
                return;
            }
            if c == b'\\' {
                self.pos += 1; // skip escape
            }
            self.pos += 1;
        }
    }

    fn skip_template(&mut self) {
        self.pos += 1; // skip `
        while !self.at_end() {
            if self.ch() == b'`' {
                self.pos += 1;
                return;
            }
            if self.ch() == b'\\' {
                self.pos += 1;
            }
            self.pos += 1;
        }
    }

    /// Skip past `>>` (the macro close delimiter), returning the end position.
    fn skip_to_close_tag(&mut self) -> usize {
        if self.remaining().starts_with(">>") {
            self.pos += 2;
        }
        self.pos
    }

    fn skip_whitespace(&mut self) {
        while !self.at_end() && matches!(self.ch(), b' ' | b'\t') {
            self.pos += 1;
        }
    }

    fn scan_macro_name(&mut self) -> String {
        let start = self.pos;
        while !self.at_end() {
            let c = self.ch();
            if c.is_ascii_alphanumeric() || c == b'_' || c == b'-' {
                self.pos += 1;
            } else {
                break;
            }
        }
        self.src[start..self.pos].to_string()
    }

    /// Parse a block macro: <<name args>>...body...<</name>>
    fn parse_block_macro(&mut self, start: usize, name: &str) -> Option<Node> {
        let args = self.parse_macro_args(name);
        self.skip_to_close_tag();

        let mut clauses = Vec::new();

        // Parse main clause body
        let body = self.parse_body(&[name]);

        clauses.push(MacroClause {
            kind: name.to_string(),
            args,
            body,
        });

        // Parse intermediate clauses (elseif, else, case, default)
        loop {
            if self.at_end() {
                self.error(
                    Span::new(start, self.pos),
                    format!("unclosed block macro: <<{name}>>"),
                );
                break;
            }

            if self.remaining().starts_with("<</") {
                // Closing tag — consume it
                let close_start = self.pos;
                if let Some(end) = self.src[self.pos..].find(">>") {
                    self.pos += end + 2;
                } else {
                    self.pos = self.bytes.len();
                }
                let _ = close_start; // consumed
                break;
            }

            // Must be a clause (<<elseif>>, <<else>>, <<case>>, <<default>>)
            if self.remaining().starts_with("<<") {
                let clause_name = self.peek_macro_name().unwrap_or_default();
                if macros::is_clause(&clause_name) {
                    self.pos += 2; // skip <<
                    self.scan_macro_name(); // consume clause name
                    let clause_args = self.parse_macro_args(&clause_name);
                    self.skip_to_close_tag();
                    let clause_body = self.parse_body(&[name]);
                    clauses.push(MacroClause {
                        kind: clause_name,
                        args: clause_args,
                        body: clause_body,
                    });
                    continue;
                }
            }

            // Unexpected content — error recovery
            self.error(
                Span::new(self.pos, self.pos + 1),
                format!("unexpected content inside <<{name}>> macro"),
            );
            break;
        }

        let end = self.pos;
        Some(Node {
            kind: NodeKind::Macro(MacroNode {
                name: name.to_string(),
                args: MacroArgs::None, // args are in the first clause
                clauses,
            }),
            span: Span::new(start, end),
        })
    }

    /// Parse a raw macro: <<script>>...raw content...<</script>>
    fn parse_raw_macro(&mut self, start: usize, name: &str) -> Option<Node> {
        // Skip args (if any) and close >>
        let _ = self.capture_args_src();
        self.skip_to_close_tag();

        // Find closing tag <</name>>
        let close_pattern = format!("<</{name}>>");
        let body_start = self.pos;
        let body_end = if let Some(offset) = self.src[self.pos..].find(&close_pattern) {
            let end = self.pos + offset;
            self.pos = end + close_pattern.len();
            end
        } else {
            self.error(
                Span::new(start, self.pos),
                format!("unclosed raw macro: <<{name}>>"),
            );
            let end = self.bytes.len();
            self.pos = end;
            end
        };

        let raw_body = self.src[body_start..body_end].to_string();
        Some(Node {
            kind: NodeKind::Macro(MacroNode {
                name: name.to_string(),
                args: MacroArgs::Raw(raw_body.clone()),
                clauses: vec![MacroClause {
                    kind: name.to_string(),
                    args: MacroArgs::Raw(raw_body),
                    body: Vec::new(),
                }],
            }),
            span: Span::new(start, self.pos),
        })
    }

    /// Parse an unknown macro (widget/custom): use lookahead to determine
    /// if it's block or self-closing.
    fn parse_unknown_macro(&mut self, start: usize, name: &str) -> Option<Node> {
        let args = self.parse_macro_args(name);
        self.skip_to_close_tag();

        // Lookahead: does `<</name>>` appear at any depth?
        let close_pattern = format!("<</{name}>>");
        if self.src[self.pos..].contains(&close_pattern) {
            // Block macro
            let body = self.parse_body(&[name]);
            let mut clauses = vec![MacroClause {
                kind: name.to_string(),
                args,
                body,
            }];

            // Consume closing tag
            if self.remaining().starts_with("<</") {
                if let Some(end) = self.src[self.pos..].find(">>") {
                    self.pos += end + 2;
                }
            } else if !self.at_end() {
                // Parse additional clauses (unlikely for custom macros)
                self.error(
                    Span::new(start, self.pos),
                    format!("unclosed custom macro: <<{name}>>"),
                );
            }

            let _ = &mut clauses; // keep borrow checker happy
            Some(Node {
                kind: NodeKind::Macro(MacroNode {
                    name: name.to_string(),
                    args: MacroArgs::None,
                    clauses,
                }),
                span: Span::new(start, self.pos),
            })
        } else {
            // Self-closing
            Some(Node {
                kind: NodeKind::Macro(MacroNode {
                    name: name.to_string(),
                    args,
                    clauses: Vec::new(),
                }),
                span: Span::new(start, self.pos),
            })
        }
    }

    /// Parse `<<for>>` arguments into the appropriate variant.
    fn parse_for_args(&mut self, args_src: &'a str) -> MacroArgs {
        let trimmed = args_src.trim();

        // Check for C-style: contains `;`
        if trimmed.contains(';') {
            let parts: Vec<&str> = args_src.splitn(3, ';').collect();
            let base = self.pos - args_src.len();

            let init = {
                let s = parts[0].trim();
                if s.is_empty() {
                    None
                } else {
                    let mut lex = ExprLexer::new(s, base);
                    Some(Box::new(expr::parse_expr(&mut lex)))
                }
            };

            let cond = if parts.len() > 1 {
                let s = parts[1].trim();
                let offset = base + parts[0].len() + 1;
                if s.is_empty() {
                    None
                } else {
                    let mut lex = ExprLexer::new(s, offset);
                    Some(Box::new(expr::parse_expr(&mut lex)))
                }
            } else {
                None
            };

            let update = if parts.len() > 2 {
                let s = parts[2].trim();
                let offset = base + parts[0].len() + 1 + parts[1].len() + 1;
                if s.is_empty() {
                    None
                } else {
                    let mut lex = ExprLexer::new(s, offset);
                    Some(Box::new(expr::parse_expr(&mut lex)))
                }
            } else {
                None
            };

            MacroArgs::ForCStyle { init, cond, update }
        } else if trimmed.contains(" range ") {
            // Range for: _var range start end
            // or: _val, _key range collection
            let range_idx = trimmed.find(" range ").unwrap();
            let before = trimmed[..range_idx].trim();
            let after = trimmed[range_idx + 7..].trim();
            let base = self.pos - args_src.len();

            if before.contains(',') {
                // For-in: _val, _key range collection
                let parts: Vec<&str> = before.splitn(2, ',').collect();
                let value_var = parts[0].trim().to_string();
                let key_var = Some(parts[1].trim().to_string());
                let mut lex = ExprLexer::new(after, base + range_idx + 7);
                let collection = Box::new(expr::parse_expr(&mut lex));
                MacroArgs::ForIn {
                    value_var,
                    key_var,
                    collection,
                }
            } else {
                // Range for: _var range expr
                let var = before.to_string();
                let mut lex = ExprLexer::new(after, base + range_idx + 7);
                let collection = Box::new(expr::parse_expr(&mut lex));
                MacroArgs::ForIn {
                    value_var: var,
                    key_var: None,
                    collection,
                }
            }
        } else {
            // Simple condition: <<for expr>>
            let base = self.pos - args_src.len();
            let mut lex = ExprLexer::new(trimmed, base);
            let e = expr::parse_expr(&mut lex);
            MacroArgs::Expr(e)
        }
    }

    /// Parse link macro args: <<link "text" "passage">> or <<link [[text|passage]]>>
    fn parse_link_macro_args(&mut self) -> MacroArgs {
        self.skip_whitespace();
        let remaining = self.remaining();

        // Check for [[...]] link syntax inside macro args
        if remaining.starts_with("[[") {
            let args_src = self.capture_args_src();
            let trimmed = args_src.trim();
            // Parse as link target
            if let Some(inner) = trimmed.strip_prefix("[[") {
                if let Some(inner) = inner.strip_suffix("]]") {
                    let (text, passage) = if let Some(pipe) = inner.find('|') {
                        (&inner[..pipe], Some(&inner[pipe + 1..]))
                    } else {
                        (inner, None)
                    };
                    return MacroArgs::LinkArgs {
                        text: LinkText::Plain(text.to_string()),
                        passage: passage.map(|p| {
                            Box::new(Expr {
                                kind: ExprKind::Str(p.to_string()),
                                span: Span::empty(self.pos),
                            })
                        }),
                    };
                }
            }
            return MacroArgs::Raw(args_src.to_string());
        }

        // Parse as expression(s): <<link "text">> or <<link "text" "passage">>
        let args_src = self.capture_args_src();
        if args_src.trim().is_empty() {
            return MacroArgs::None;
        }

        let mut lexer = ExprLexer::new(args_src, self.pos - args_src.len());
        let text_expr = expr::parse_single_expr(&mut lexer);

        // Check for optional passage arg
        let passage = if lexer.peek().kind != super::lexer::TokenKind::Eof {
            Some(Box::new(expr::parse_single_expr(&mut lexer)))
        } else {
            None
        };

        let text = match text_expr.kind {
            ExprKind::Str(ref s) => LinkText::Plain(s.clone()),
            _ => LinkText::Expr(Box::new(text_expr)),
        };

        MacroArgs::LinkArgs { text, passage }
    }

    /// Parse a `[[link]]` or `[[text|passage]]` navigation link.
    fn parse_link(&mut self) -> Node {
        let start = self.pos;
        self.pos += 2; // skip [[

        // Find the closing ]]
        let content_start = self.pos;
        let mut depth = 0i32;
        while !self.at_end() {
            if self.remaining().starts_with("]]") && depth == 0 {
                break;
            }
            match self.ch() {
                b'[' => depth += 1,
                b']' => depth -= 1,
                _ => {}
            }
            self.pos += 1;
        }
        let content = &self.src[content_start..self.pos];

        // Skip closing ]]
        if self.remaining().starts_with("]]") {
            self.pos += 2;
        }

        // Parse link content: text|passage or just passage
        // Also handle reversed form: passage|text (SugarCube supports both)
        let (text, target) = if let Some(pipe) = content.rfind('|') {
            let left = &content[..pipe];
            let right = &content[pipe + 1..];
            (left.to_string(), right.to_string())
        } else if let Some(arrow_right) = content.find("->") {
            let left = &content[..arrow_right];
            let right = &content[arrow_right + 2..];
            (left.to_string(), right.to_string())
        } else if let Some(arrow_left) = content.find("<-") {
            let left = &content[..arrow_left];
            let right = &content[arrow_left + 2..];
            (right.to_string(), left.to_string())
        } else {
            (content.to_string(), content.to_string())
        };

        // Check for setter: [$code] after the link
        let mut setters = Vec::new();
        while !self.at_end() && self.remaining().starts_with("[$") {
            self.pos += 1; // skip [
            let setter_start = self.pos;
            let mut sdepth = 1i32;
            while !self.at_end() && sdepth > 0 {
                match self.ch() {
                    b'[' => sdepth += 1,
                    b']' => sdepth -= 1,
                    _ => {}
                }
                if sdepth > 0 {
                    self.pos += 1;
                }
            }
            let setter_src = &self.src[setter_start..self.pos];
            if !self.at_end() {
                self.pos += 1; // skip ]
            }
            let mut lexer = ExprLexer::new(setter_src, setter_start);
            setters.push(expr::parse_expr(&mut lexer));
        }

        // Determine target kind: if it starts with $ or _, it's an expression
        let link_target = if target.starts_with('$') || target.starts_with('_') {
            let mut lexer = ExprLexer::new(&target, content_start);
            let e = expr::parse_expr(&mut lexer);
            LinkTarget::Expr(Box::new(e))
        } else {
            LinkTarget::Name(target)
        };

        Node {
            kind: NodeKind::Link(LinkNode {
                text: LinkText::Plain(text),
                target: link_target,
                setters,
            }),
            span: Span::new(start, self.pos),
        }
    }

    /// Parse variable interpolation: `$name.prop[idx]` or `_temp.prop`.
    fn parse_var_interp(&mut self) -> Node {
        let start = self.pos;
        // Use expression lexer to parse the full variable reference
        let remaining = self.remaining();

        // Find the extent of the variable reference — scan until we hit
        // something that can't be part of a member/index chain.
        let var_end = self.scan_var_extent();
        let var_src = &self.src[start..var_end];
        self.pos = var_end;

        let mut lexer = ExprLexer::new(var_src, start);
        let e = expr::parse_expr(&mut lexer);

        // Adjust pos for any unconsumed input in the lexer
        let _ = remaining;

        Node {
            kind: NodeKind::VarInterp(e),
            span: Span::new(start, self.pos),
        }
    }

    /// Scan the extent of a variable interpolation reference.
    /// Handles: $name, $name.prop, $name[idx], $name.method(args)
    fn scan_var_extent(&self) -> usize {
        let mut pos = self.pos;
        let bytes = self.bytes;

        // Skip $ or _ prefix
        if pos < bytes.len() && (bytes[pos] == b'$' || bytes[pos] == b'_') {
            pos += 1;
        }

        // Scan identifier
        while pos < bytes.len() && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_') {
            pos += 1;
        }

        // Continue with .prop, [idx], (args) chains
        loop {
            if pos >= bytes.len() {
                break;
            }
            if bytes[pos] == b'.' && pos + 1 < bytes.len() && bytes[pos + 1].is_ascii_alphabetic()
            {
                pos += 1;
                while pos < bytes.len()
                    && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_')
                {
                    pos += 1;
                }
            } else if bytes[pos] == b'[' {
                let mut depth = 1;
                pos += 1;
                while pos < bytes.len() && depth > 0 {
                    match bytes[pos] {
                        b'[' => depth += 1,
                        b']' => depth -= 1,
                        _ => {}
                    }
                    pos += 1;
                }
            } else if bytes[pos] == b'(' {
                let mut depth = 1;
                pos += 1;
                while pos < bytes.len() && depth > 0 {
                    match bytes[pos] {
                        b'(' => depth += 1,
                        b')' => depth -= 1,
                        _ => {}
                    }
                    pos += 1;
                }
            } else {
                break;
            }
        }

        pos
    }

    fn parse_block_comment(&mut self) -> Node {
        let start = self.pos;
        self.pos += 2; // skip /*
        let end = if let Some(offset) = self.src[self.pos..].find("*/") {
            let end = self.pos + offset + 2;
            self.pos = end;
            end
        } else {
            self.pos = self.bytes.len();
            self.bytes.len()
        };
        let content = self.src[start + 2..end.saturating_sub(2)].to_string();
        Node {
            kind: NodeKind::Comment(content),
            span: Span::new(start, end),
        }
    }

    fn parse_html_comment(&mut self) -> Node {
        let start = self.pos;
        self.pos += 4; // skip <!--
        let end = if let Some(offset) = self.src[self.pos..].find("-->") {
            let end = self.pos + offset + 3;
            self.pos = end;
            end
        } else {
            self.pos = self.bytes.len();
            self.bytes.len()
        };
        let content = self.src[start + 4..end.saturating_sub(3)].to_string();
        Node {
            kind: NodeKind::Comment(content),
            span: Span::new(start, end),
        }
    }

    fn parse_html_tag(&mut self) -> Node {
        let start = self.pos;
        // Scan forward to find the matching >
        // Handle self-closing tags and quoted attributes
        self.pos += 1; // skip <
        while !self.at_end() {
            match self.ch() {
                b'>' => {
                    self.pos += 1;
                    break;
                }
                b'"' | b'\'' => {
                    let q = self.ch();
                    self.pos += 1;
                    while !self.at_end() && self.ch() != q {
                        self.pos += 1;
                    }
                    if !self.at_end() {
                        self.pos += 1;
                    }
                }
                _ => self.pos += 1,
            }
        }
        let html = self.src[start..self.pos].to_string();
        Node {
            kind: NodeKind::Html(html),
            span: Span::new(start, self.pos),
        }
    }

    /// Parse plain text until we hit a special character sequence.
    fn parse_text(&mut self) -> Node {
        let start = self.pos;
        while !self.at_end() {
            let ch = self.ch();
            match ch {
                // Stop at special sequences
                b'<' if self.remaining().starts_with("<<")
                    || self.remaining().starts_with("<!--")
                    || (self.pos + 1 < self.bytes.len()
                        && (self.bytes[self.pos + 1].is_ascii_alphabetic()
                            || self.bytes[self.pos + 1] == b'/')) =>
                {
                    break;
                }
                b'[' if self.remaining().starts_with("[[") => break,
                b'/' if self.remaining().starts_with("/*") => break,
                b'$' if self.pos + 1 < self.bytes.len()
                    && (self.bytes[self.pos + 1].is_ascii_alphabetic()
                        || self.bytes[self.pos + 1] == b'_') =>
                {
                    break;
                }
                b'_'
                    if self.pos + 1 < self.bytes.len()
                        && self.bytes[self.pos + 1].is_ascii_alphabetic() =>
                {
                    break;
                }
                _ => self.pos += 1,
            }
        }
        let text = self.src[start..self.pos].to_string();
        Node {
            kind: NodeKind::Text(text),
            span: Span::new(start, self.pos),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_str(src: &str) -> PassageAst {
        parse(src)
    }

    fn first_node(src: &str) -> Node {
        let ast = parse_str(src);
        ast.body.into_iter().next().expect("expected at least one node")
    }

    // ── Plain text ──────────────────────────────────────────────────

    #[test]
    fn plain_text() {
        let ast = parse_str("Hello world!");
        assert_eq!(ast.body.len(), 1);
        assert!(matches!(&ast.body[0].kind, NodeKind::Text(s) if s == "Hello world!"));
        assert!(ast.errors.is_empty());
    }

    // ── Variable interpolation ──────────────────────────────────────

    #[test]
    fn story_var_interpolation() {
        let ast = parse_str("Hello $name!");
        assert_eq!(ast.body.len(), 3);
        assert!(matches!(&ast.body[0].kind, NodeKind::Text(s) if s == "Hello "));
        assert!(matches!(&ast.body[1].kind, NodeKind::VarInterp(_)));
        assert!(matches!(&ast.body[2].kind, NodeKind::Text(s) if s == "!"));
    }

    #[test]
    fn temp_var_interpolation() {
        let ast = parse_str("Value: _temp done");
        assert_eq!(ast.body.len(), 3);
        assert!(matches!(&ast.body[0].kind, NodeKind::Text(s) if s == "Value: "));
        assert!(matches!(&ast.body[1].kind, NodeKind::VarInterp(_)));
    }

    #[test]
    fn chained_var_interpolation() {
        let node = first_node("$obj.prop.sub");
        if let NodeKind::VarInterp(e) = &node.kind {
            assert!(matches!(e.kind, ExprKind::Member { .. }));
        } else {
            panic!("expected VarInterp");
        }
    }

    // ── Links ───────────────────────────────────────────────────────

    #[test]
    fn simple_link() {
        let node = first_node("[[Bedroom]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "Bedroom"));
            assert!(matches!(&link.target, LinkTarget::Name(s) if s == "Bedroom"));
        } else {
            panic!("expected Link");
        }
    }

    #[test]
    fn link_with_text() {
        let node = first_node("[[Leave|Orphanage]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "Leave"));
            assert!(matches!(&link.target, LinkTarget::Name(s) if s == "Orphanage"));
        } else {
            panic!("expected Link");
        }
    }

    #[test]
    fn link_with_variable_target() {
        let node = first_node("[[Back|$exitPassage]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "Back"));
            assert!(matches!(&link.target, LinkTarget::Expr(_)));
        } else {
            panic!("expected Link");
        }
    }

    #[test]
    fn link_with_arrow() {
        let node = first_node("[[Go home->Home]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "Go home"));
            assert!(matches!(&link.target, LinkTarget::Name(s) if s == "Home"));
        } else {
            panic!("expected Link");
        }
    }

    // ── Self-closing macros ─────────────────────────────────────────

    #[test]
    fn set_macro() {
        let node = first_node("<<set $x to 5>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "set");
            assert!(matches!(m.args, MacroArgs::AssignList(_)));
            assert!(m.clauses.is_empty());
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn set_multi_assign() {
        let node = first_node("<<set $x to 1, $y to 2>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "set");
            if let MacroArgs::AssignList(list) = &m.args {
                assert_eq!(list.len(), 2);
            } else {
                panic!("expected AssignList");
            }
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn print_macro() {
        let node = first_node("<<print $x + 1>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "print");
            assert!(matches!(m.args, MacroArgs::Expr(_)));
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn print_shorthand() {
        let node = first_node("<<= $x>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "=");
            assert!(matches!(m.args, MacroArgs::Expr(_)));
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn run_macro() {
        let node = first_node("<<run $arr.push(5)>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "run");
            assert!(matches!(m.args, MacroArgs::Expr(_)));
        } else {
            panic!("expected Macro");
        }
    }

    // ── Block macros ────────────────────────────────────────────────

    #[test]
    fn if_block() {
        let node = first_node("<<if $x is 1>>yes<</if>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 1);
            assert_eq!(m.clauses[0].kind, "if");
            assert_eq!(m.clauses[0].body.len(), 1);
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn if_else_block() {
        let node = first_node("<<if $x>>yes<<else>>no<</if>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 2);
            assert_eq!(m.clauses[0].kind, "if");
            assert_eq!(m.clauses[1].kind, "else");
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn if_elseif_else_block() {
        let node = first_node("<<if $x>>a<<elseif $y>>b<<else>>c<</if>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 3);
            assert_eq!(m.clauses[0].kind, "if");
            assert_eq!(m.clauses[1].kind, "elseif");
            assert_eq!(m.clauses[2].kind, "else");
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn switch_block() {
        let src = r#"<<switch $x>><<case 1>>one<<case 2>>two<<default>>other<</switch>>"#;
        let node = first_node(src);
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "switch");
            assert_eq!(m.clauses.len(), 4);
            assert_eq!(m.clauses[0].kind, "switch");
            assert_eq!(m.clauses[1].kind, "case");
            assert_eq!(m.clauses[2].kind, "case");
            assert_eq!(m.clauses[3].kind, "default");
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn for_c_style() {
        let node = first_node("<<for _i to 0; _i lt 10; _i++>>body<</for>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "for");
            assert_eq!(m.clauses.len(), 1);
            if let MacroArgs::ForCStyle { init, cond, update } = &m.clauses[0].args {
                assert!(init.is_some());
                assert!(cond.is_some());
                assert!(update.is_some());
            } else {
                panic!("expected ForCStyle, got {:?}", m.clauses[0].args);
            }
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn for_range() {
        let node = first_node("<<for _item range $array>>body<</for>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "for");
            if let MacroArgs::ForIn { value_var, .. } = &m.clauses[0].args {
                assert_eq!(value_var, "_item");
            } else {
                panic!("expected ForIn");
            }
        } else {
            panic!("expected Macro");
        }
    }

    // ── Special macros ──────────────────────────────────────────────

    #[test]
    fn silently_block() {
        let node = first_node("<<silently>><<set $x to 1>><</silently>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "silently");
            assert_eq!(m.clauses.len(), 1);
            assert!(!m.clauses[0].body.is_empty());
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn widget_definition() {
        let node = first_node(r#"<<widget "myWidget">>body<</widget>>"#);
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "widget");
            assert_eq!(m.clauses.len(), 1);
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn script_raw_block() {
        let node = first_node("<<script>>var x = 1 + 2;<</script>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "script");
            // Raw body should be preserved
            if let MacroArgs::Raw(body) = &m.args {
                assert_eq!(body, "var x = 1 + 2;");
            } else {
                panic!("expected Raw args");
            }
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn link_macro_with_text() {
        let node = first_node(r#"<<link "Click me">>body<</link>>"#);
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "link");
            assert_eq!(m.clauses.len(), 1);
        } else {
            panic!("expected Macro");
        }
    }

    // ── Custom/unknown macros ───────────────────────────────────────

    #[test]
    fn unknown_block_macro() {
        let node = first_node("<<myWidget>>content<</myWidget>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "myWidget");
            assert_eq!(m.clauses.len(), 1);
            assert!(!m.clauses[0].body.is_empty());
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn unknown_self_closing_macro() {
        let node = first_node("<<customEffect>>");
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "customEffect");
            assert!(m.clauses.is_empty());
        } else {
            panic!("expected Macro");
        }
    }

    // ── Comments ────────────────────────────────────────────────────

    #[test]
    fn block_comment() {
        let node = first_node("/* this is a comment */");
        assert!(matches!(&node.kind, NodeKind::Comment(s) if s.contains("comment")));
    }

    #[test]
    fn html_comment() {
        let node = first_node("<!-- effects -->");
        assert!(matches!(&node.kind, NodeKind::Comment(s) if s.contains("effects")));
    }

    // ── HTML ────────────────────────────────────────────────────────

    #[test]
    fn inline_html() {
        let node = first_node("<span class=\"red\">text</span>");
        // First node should be the opening tag
        assert!(matches!(&node.kind, NodeKind::Html(s) if s.starts_with("<span")));
    }

    #[test]
    fn br_tag() {
        let node = first_node("<br>");
        assert!(matches!(&node.kind, NodeKind::Html(s) if s == "<br>"));
    }

    // ── Nested structures ───────────────────────────────────────────

    #[test]
    fn nested_if() {
        let src = "<<if $a>><<if $b>>inner<</if>><</if>>";
        let node = first_node(src);
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 1);
            // Inner should contain a nested if macro
            let inner_nodes = &m.clauses[0].body;
            assert!(inner_nodes.iter().any(|n| matches!(&n.kind, NodeKind::Macro(m) if m.name == "if")));
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn link_inside_if() {
        let src = "<<if $x>>[[Go|Room]]<</if>>";
        let node = first_node(src);
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "if");
            let body = &m.clauses[0].body;
            assert!(body.iter().any(|n| matches!(&n.kind, NodeKind::Link(_))));
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn macro_inside_link_macro() {
        let src = "<<link [[Leave|Start]]>><<set $x to 1>><</link>>";
        let node = first_node(src);
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "link");
            let body = &m.clauses[0].body;
            assert!(body.iter().any(|n| matches!(&n.kind, NodeKind::Macro(inner) if inner.name == "set")));
        } else {
            panic!("expected Macro");
        }
    }

    // ── Error recovery ──────────────────────────────────────────────

    #[test]
    fn unclosed_macro_doesnt_panic() {
        let ast = parse_str("<<if $x>>content");
        assert!(!ast.errors.is_empty());
        assert!(!ast.body.is_empty());
    }

    #[test]
    fn orphaned_close_doesnt_panic() {
        let ast = parse_str("text<</if>>");
        assert!(!ast.errors.is_empty());
    }

    // ── Mixed content ───────────────────────────────────────────────

    #[test]
    fn mixed_content() {
        let src = "Hello $name! <<if $x is 1>><span class=\"blue\">yes</span><<else>>no<</if>> [[Back|Start]]";
        let ast = parse_str(src);
        assert!(ast.errors.is_empty(), "errors: {:?}", ast.errors);
        // Should have: Text, VarInterp, Text, Macro(if), Text, Link
        assert!(ast.body.len() >= 4);
    }

    #[test]
    fn real_world_passage() {
        let src = r#"<<set $outside to 0>><<set $location to "home">>

<<if $options.images is 1>>
	<img class="resize" src="img/misc/banner.png">
<</if>>

[[Bedroom]]
[[Garden]]"#;
        let ast = parse_str(src);
        assert!(ast.errors.is_empty(), "errors: {:?}", ast.errors);
        // Should parse without errors
        let macro_count = ast.body.iter().filter(|n| matches!(n.kind, NodeKind::Macro(_))).count();
        assert!(macro_count >= 3, "expected at least 3 macros, got {}", macro_count);
    }
}
