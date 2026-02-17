//! Recursive descent parser for SugarCube passage source text.
//!
//! Converts raw passage source into a `PassageAst` of `Node` items:
//! macros (`<<...>>`), links (`[[...]]`), variable interpolation
//! (`$var`, `_tmp`), inline HTML, and plain text.
//!
//! Expressions are stored as raw source byte ranges (`Expr { start, end }`)
//! — actual parsing happens at translation time via oxc.
//!
//! Error recovery: errors are accumulated in `PassageAst.errors` and
//! parsing continues. A single broken passage must not prevent parsing
//! the other 9,999.

use super::ast::*;
use super::macros::{self, MacroKind};
use crate::html_util;

/// Parse a SugarCube passage source string into an AST.
pub fn parse(source: &str) -> PassageAst {
    let mut parser = Parser::new(source);
    let body = parser.parse_body(&[]);
    PassageAst {
        body,
        errors: parser.errors,
        source: source.to_string(),
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
        // Safety: ensure we're on a char boundary
        let pos = self.snap_to_char_boundary(self.pos);
        &self.src[pos..]
    }

    /// Snap a byte position forward to the nearest UTF-8 char boundary.
    fn snap_to_char_boundary(&self, pos: usize) -> usize {
        let mut p = pos;
        while p < self.bytes.len() && !self.src.is_char_boundary(p) {
            p += 1;
        }
        p
    }

    /// Advance past the current UTF-8 character (1-4 bytes).
    fn advance_char(&mut self) {
        if self.pos < self.bytes.len() {
            let b = self.bytes[self.pos];
            let width = if b < 0x80 {
                1
            } else if b < 0xE0 {
                2
            } else if b < 0xF0 {
                3
            } else {
                4
            };
            self.pos = (self.pos + width).min(self.bytes.len());
        }
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

        // [img[src]] or [img[src][link]]
        if remaining.starts_with("[img[") {
            return Some(self.parse_image());
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

        // Line break
        if self.ch() == b'\n' {
            let start = self.pos;
            self.pos += 1;
            return Some(Node {
                kind: NodeKind::LineBreak,
                span: Span::new(start, self.pos),
            });
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
            // Not a valid macro (e.g. `<<<`), treat `<<` as literal text
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
                let base = self.pos - args_src.len();
                let exprs = split_assignment_list(args_src, base);
                MacroArgs::AssignList(exprs)
            }
            "run" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let base = self.pos - args_src.len();
                MacroArgs::Expr(make_trimmed_expr(args_src, base))
            }
            "if" | "elseif" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let base = self.pos - args_src.len();
                MacroArgs::Expr(make_trimmed_expr(args_src, base))
            }
            "print" | "=" | "-" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let base = self.pos - args_src.len();
                MacroArgs::Expr(make_trimmed_expr(args_src, base))
            }
            "switch" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let base = self.pos - args_src.len();
                MacroArgs::Switch(make_trimmed_expr(args_src, base))
            }
            "case" => {
                let args_src = self.capture_args_src();
                if args_src.trim().is_empty() {
                    return MacroArgs::None;
                }
                let base = self.pos - args_src.len();
                let values = split_case_values(args_src, base);
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
                let base = self.pos - args_src.len();
                let exprs = split_assignment_list(args_src, base);
                MacroArgs::AssignList(exprs)
            }
            "back" | "return" | "goto" | "include" => {
                // Navigation macros: detect [[...]] link syntax before
                // falling through to expression parsing.
                if self.remaining().starts_with("[[") {
                    let args_src = self.capture_args_src();
                    let trimmed = args_src.trim();
                    if let Some(inner) = trimmed.strip_prefix("[[") {
                        if let Some(inner) = inner.strip_suffix("]]") {
                            // Extract passage target
                            let passage_str = if let Some(pipe) = inner.rfind('|') {
                                inner[pipe + 1..].trim()
                            } else if let Some(arrow) = inner.find("->") {
                                inner[arrow + 2..].trim()
                            } else if let Some(arrow) = inner.find("<-") {
                                inner[..arrow].trim()
                            } else {
                                inner.trim()
                            };
                            let base = self.pos - args_src.len();
                            if passage_str.starts_with('$')
                                || passage_str.starts_with('_')
                            {
                                // Variable reference — store as expression
                                let offset = base + (passage_str.as_ptr() as usize
                                    - args_src.as_ptr() as usize);
                                return MacroArgs::Expr(Expr::new(
                                    offset,
                                    offset + passage_str.len(),
                                ));
                            } else {
                                // Literal passage name — store as Raw
                                return MacroArgs::Raw(passage_str.to_string());
                            }
                        }
                    }
                    MacroArgs::Raw(args_src.to_string())
                } else {
                    let args_src = self.capture_args_src();
                    let trimmed = args_src.trim();
                    if trimmed.is_empty() {
                        MacroArgs::None
                    } else {
                        let base = self.pos - args_src.len();
                        MacroArgs::Expr(make_trimmed_expr(args_src, base))
                    }
                }
            }
            // Macros whose arguments are CSS selectors, durations, or other
            // non-expression tokens — store as Raw.
            "replace" | "append" | "prepend" | "timed" | "repeat" | "type"
            | "addclass" | "removeclass" | "toggleclass"
            | "copy" | "remove" | "done" | "listbox" | "cycle"
            | "textbox" | "textarea" | "numberbox" | "checkbox" | "radiobutton" => {
                let args_src = self.capture_args_src();
                let trimmed = args_src.trim();
                if trimmed.is_empty() {
                    MacroArgs::None
                } else {
                    MacroArgs::Raw(trimmed.to_string())
                }
            }
            _ => {
                // Unknown macro: store as Raw (unknown arg semantics)
                let args_src = self.capture_args_src();
                let trimmed = args_src.trim();
                if trimmed.is_empty() {
                    MacroArgs::None
                } else {
                    MacroArgs::Raw(trimmed.to_string())
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
                b'[' if self.pos + 1 < self.bytes.len()
                    && self.bytes[self.pos + 1] == b'[' =>
                {
                    // [[...]] link — scan to ]] without interpreting quotes
                    // (passage names like "Valentine's Day" contain apostrophes)
                    self.pos += 2;
                    while !self.at_end() {
                        if self.pos + 1 < self.bytes.len()
                            && self.bytes[self.pos] == b']'
                            && self.bytes[self.pos + 1] == b']'
                        {
                            self.pos += 2;
                            break;
                        }
                        self.advance_char();
                    }
                }
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
                        self.advance_char();
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
                        self.advance_char();
                    }
                }
                _ => self.advance_char(),
            }
        }
        let end = self.snap_to_char_boundary(self.pos);
        &self.src[start..end]
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
                self.pos += 1; // skip escape char
            }
            self.advance_char();
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
            self.advance_char();
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
                // Extract closing tag name and verify it matches
                let close_name = self.peek_macro_name().unwrap_or_default();
                if close_name == name {
                    // Correct close — consume it
                    if let Some(end) = self.src[self.pos..].find(">>") {
                        self.pos += end + 2;
                    } else {
                        self.pos = self.bytes.len();
                    }
                } else {
                    // Mismatched close tag — emit error for current macro
                    // but do NOT consume the tag so the parent can match it
                    self.error(
                        Span::new(start, self.pos),
                        format!("unclosed block macro: <<{name}>>"),
                    );
                }
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
                    let offset = base + (s.as_ptr() as usize - args_src.as_ptr() as usize);
                    Some(Box::new(Expr::new(offset, offset + s.len())))
                }
            };

            let cond = if parts.len() > 1 {
                let s = parts[1].trim();
                if s.is_empty() {
                    None
                } else {
                    let offset = base + (s.as_ptr() as usize - args_src.as_ptr() as usize);
                    Some(Box::new(Expr::new(offset, offset + s.len())))
                }
            } else {
                None
            };

            let update = if parts.len() > 2 {
                let s = parts[2].trim();
                if s.is_empty() {
                    None
                } else {
                    let offset = base + (s.as_ptr() as usize - args_src.as_ptr() as usize);
                    Some(Box::new(Expr::new(offset, offset + s.len())))
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
            let trimmed_offset = base + (trimmed.as_ptr() as usize - args_src.as_ptr() as usize);

            if before.contains(',') {
                // For-in: _val, _key range collection
                let parts: Vec<&str> = before.splitn(2, ',').collect();
                let value_var = parts[0].trim().to_string();
                let key_var = Some(parts[1].trim().to_string());
                let after_offset = trimmed_offset + range_idx + 7
                    + (after.as_ptr() as usize
                        - trimmed[range_idx + 7..].as_ptr() as usize);
                let collection = Box::new(Expr::new(after_offset, after_offset + after.len()));
                MacroArgs::ForIn {
                    value_var,
                    key_var,
                    collection,
                }
            } else {
                // Range for: _var range expr
                let var = before.to_string();
                let after_offset = trimmed_offset + range_idx + 7
                    + (after.as_ptr() as usize
                        - trimmed[range_idx + 7..].as_ptr() as usize);
                let collection = Box::new(Expr::new(after_offset, after_offset + after.len()));
                MacroArgs::ForIn {
                    value_var: var,
                    key_var: None,
                    collection,
                }
            }
        } else {
            // Simple condition: <<for expr>>
            let base = self.pos - args_src.len();
            MacroArgs::Expr(make_trimmed_expr(args_src, base))
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
                            if p.starts_with('$') || p.starts_with('_') {
                                let base = self.pos - args_src.len();
                                let offset = base
                                    + (p.as_ptr() as usize - args_src.as_ptr() as usize);
                                LinkTarget::Expr(Box::new(Expr::new(offset, offset + p.len())))
                            } else {
                                LinkTarget::Name(p.to_string())
                            }
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

        let base = self.pos - args_src.len();
        let values = split_link_args(args_src, base);

        if values.is_empty() {
            return MacroArgs::None;
        }

        // First value is the text expression
        let text_expr = values[0].clone();
        let text_str = text_expr.text(self.src);

        // Check for optional passage arg
        let passage = if values.len() > 1 {
            Some(LinkTarget::Expr(Box::new(values[1].clone())))
        } else {
            None
        };

        // Determine if text is a plain string literal
        let text = if (text_str.starts_with('"') && text_str.ends_with('"'))
            || (text_str.starts_with('\'') && text_str.ends_with('\''))
        {
            LinkText::Plain(text_str[1..text_str.len() - 1].to_string())
        } else {
            LinkText::Expr(Box::new(text_expr))
        };

        MacroArgs::LinkArgs { text, passage }
    }

    /// Parse an `[img[src]]` or `[img[src][link]]` inline image.
    fn parse_image(&mut self) -> Node {
        let start = self.pos;
        self.pos += 5; // skip [img[

        // Scan src up to `]`
        let src_start = self.pos;
        while !self.at_end() && self.ch() != b']' {
            self.advance_char();
        }
        let src_end = self.snap_to_char_boundary(self.pos);
        let src = self.src[src_start..src_end].to_string();

        // Consume `]`
        if !self.at_end() && self.ch() == b']' {
            self.pos += 1;
        }

        // Optional link target: `[link]`
        let link = if !self.at_end() && self.ch() == b'[' {
            self.pos += 1; // skip [
            let link_start = self.pos;
            while !self.at_end() && self.ch() != b']' {
                self.advance_char();
            }
            let link_end = self.snap_to_char_boundary(self.pos);
            let link_text = self.src[link_start..link_end].to_string();
            if !self.at_end() && self.ch() == b']' {
                self.pos += 1; // skip ]
            }
            Some(link_text)
        } else {
            None
        };

        // Consume final `]`
        if !self.at_end() && self.ch() == b']' {
            self.pos += 1;
        }

        Node {
            kind: NodeKind::Image { src, link },
            span: Span::new(start, self.pos),
        }
    }

    /// Parse a `[[link]]` or `[[text|passage]]` navigation link.
    ///
    /// Syntax: `[[content][$setter1][$setter2]...]`
    /// where content is `text|passage`, `text->passage`, `passage<-text`, or `passage`.
    /// The outer delimiters are `[[` ... `]]`, but setters `[$...]` sit between the
    /// inner `]` (closing the content) and the outer `]` (closing the link).
    fn parse_link(&mut self) -> Node {
        let start = self.pos;
        self.pos += 2; // skip [[

        // Scan link content up to the first `]` at bracket depth 0.
        // This stops before any `][$setter]` blocks.
        let content_start = self.pos;
        let mut depth = 0i32;
        while !self.at_end() {
            match self.ch() {
                b'[' => {
                    depth += 1;
                    self.pos += 1;
                }
                b']' => {
                    if depth == 0 {
                        break;
                    }
                    depth -= 1;
                    self.pos += 1;
                }
                _ => self.advance_char(),
            }
        }
        let content_end = self.snap_to_char_boundary(self.pos);
        let content = &self.src[content_start..content_end];

        // Consume the `]` that closes the content portion
        if !self.at_end() && self.ch() == b']' {
            self.pos += 1;
        }

        // Parse link content: text|passage or just passage
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

        // Parse setter blocks: [$code] between the content `]` and the closing `]`
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
                    self.advance_char();
                }
            }
            let setter_end = self.snap_to_char_boundary(self.pos);
            if !self.at_end() {
                self.pos += 1; // skip closing ]
            }
            setters.push(Expr::new(setter_start, setter_end));
        }

        // Consume the final `]` (closing bracket of the link)
        if !self.at_end() && self.ch() == b']' {
            self.pos += 1;
        }

        // Determine target kind: if it starts with $ or _, it's an expression
        let link_target = if target.starts_with('$') || target.starts_with('_') {
            let target_offset = content_start
                + (target.as_ptr() as usize - content.as_ptr() as usize);
            LinkTarget::Expr(Box::new(Expr::new(
                target_offset,
                target_offset + target.len(),
            )))
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
        // Find the extent of the variable reference — scan until we hit
        // something that can't be part of a member/index chain.
        let var_end = self.scan_var_extent();
        self.pos = var_end;

        Node {
            kind: NodeKind::VarInterp(Expr::new(start, var_end)),
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
                        self.advance_char();
                    }
                    if !self.at_end() {
                        self.pos += 1;
                    }
                }
                _ => self.advance_char(),
            }
        }
        let end = self.snap_to_char_boundary(self.pos);
        let raw = &self.src[start..end];
        let span = Span::new(start, end);

        // Extract @attr="expr" dynamic attributes from raw string before html5ever
        let (clean_html, dynamic_attrs) = extract_dynamic_attrs(raw);

        // Tokenize with html5ever for spec-compliant parsing
        let kind = if let Some(info) = html_util::tokenize_html_tag(&clean_html) {
            if info.is_end {
                NodeKind::HtmlClose(info.name)
            } else if info.is_void {
                NodeKind::HtmlVoid {
                    tag: info.name,
                    attrs: info.attrs,
                    dynamic_attrs,
                }
            } else {
                NodeKind::HtmlOpen {
                    tag: info.name,
                    attrs: info.attrs,
                    dynamic_attrs,
                }
            }
        } else {
            // Fallback: emit as text
            NodeKind::Text(raw.to_string())
        };

        Node { kind, span }
    }

    /// Parse plain text until we hit a special character sequence.
    fn parse_text(&mut self) -> Node {
        let start = self.pos;
        while !self.at_end() {
            let ch = self.ch();
            // Only ASCII bytes can start special sequences
            if ch >= 0x80 {
                self.advance_char();
                continue;
            }
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
                b'[' if self.remaining().starts_with("[img[")
                    || self.remaining().starts_with("[[") =>
                {
                    break;
                }
                b'\n' => break,
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
        let end = self.snap_to_char_boundary(self.pos);
        let text = self.src[start..end].to_string();
        Node {
            kind: NodeKind::Text(text),
            span: Span::new(start, end),
        }
    }
}

// ── Expression splitting helpers ──────────────────────────────────────────

/// Create a trimmed Expr from a source slice with a base offset.
fn make_trimmed_expr(src: &str, base: usize) -> Expr {
    let trimmed = src.trim();
    let offset = base + (trimmed.as_ptr() as usize - src.as_ptr() as usize);
    Expr::new(offset, offset + trimmed.len())
}

/// Split a comma-separated assignment list into individual Expr spans.
/// Respects bracket/paren/string depth — commas inside nested expressions
/// are not treated as separators.
fn split_assignment_list(src: &str, base: usize) -> Vec<Expr> {
    let mut result = Vec::new();
    let bytes = src.as_bytes();
    let mut start = 0;
    let mut depth = 0i32;
    let mut i = 0;

    while i < bytes.len() {
        match bytes[i] {
            b'(' | b'[' | b'{' => {
                depth += 1;
                i += 1;
            }
            b')' | b']' | b'}' => {
                depth -= 1;
                i += 1;
            }
            b'"' | b'\'' => {
                i = skip_string_in(bytes, i);
            }
            b'`' => {
                i = skip_template_in(bytes, i);
            }
            b',' if depth == 0 => {
                let piece = src[start..i].trim();
                if !piece.is_empty() {
                    let offset =
                        base + (piece.as_ptr() as usize - src.as_ptr() as usize);
                    result.push(Expr::new(offset, offset + piece.len()));
                }
                start = i + 1;
                i += 1;
            }
            _ => i += 1,
        }
    }

    // Last piece
    let piece = src[start..].trim();
    if !piece.is_empty() {
        let offset = base + (piece.as_ptr() as usize - src.as_ptr() as usize);
        result.push(Expr::new(offset, offset + piece.len()));
    }

    result
}

/// Split `<<case val1 val2>>` values on whitespace at depth 0.
/// Each value is parsed as a separate expression.
fn split_case_values(src: &str, base: usize) -> Vec<Expr> {
    let mut result = Vec::new();
    let bytes = src.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        // Skip whitespace
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }

        // Scan one value
        let val_start = i;
        let mut depth = 0i32;
        while i < bytes.len() {
            match bytes[i] {
                b'(' | b'[' | b'{' => {
                    depth += 1;
                    i += 1;
                }
                b')' | b']' | b'}' => {
                    depth -= 1;
                    i += 1;
                }
                b'"' | b'\'' => {
                    i = skip_string_in(bytes, i);
                }
                b'`' => {
                    i = skip_template_in(bytes, i);
                }
                b' ' | b'\t' | b'\n' if depth == 0 => break,
                _ => i += 1,
            }
        }

        if i > val_start {
            // Strip trailing comma (DoL uses `<<case "a", "b">>` with commas)
            let mut end = i;
            while end > val_start && src.as_bytes()[end - 1] == b',' {
                end -= 1;
            }
            if end > val_start {
                result.push(Expr::new(base + val_start, base + end));
            }
        }
    }

    result
}

/// Split link macro arguments into individual expression spans.
/// Handles: <<link "text">> and <<link "text" "passage">>
/// Splits on whitespace at depth 0, treating quoted strings as single tokens.
fn split_link_args(src: &str, base: usize) -> Vec<Expr> {
    // Link args are similar to case values — split on whitespace at depth 0
    split_case_values(src, base)
}

/// Skip a string literal starting at position `i`, returning the position after.
fn skip_string_in(bytes: &[u8], start: usize) -> usize {
    let quote = bytes[start];
    let mut i = start + 1;
    while i < bytes.len() {
        if bytes[i] == quote {
            return i + 1;
        }
        if bytes[i] == b'\\' {
            i += 1;
        }
        i += 1;
    }
    i
}

/// Skip a template literal starting at position `i`, returning the position after.
fn skip_template_in(bytes: &[u8], start: usize) -> usize {
    let mut i = start + 1;
    while i < bytes.len() {
        if bytes[i] == b'`' {
            return i + 1;
        }
        if bytes[i] == b'\\' {
            i += 1;
        }
        i += 1;
    }
    i
}

/// Extract `@attr="expr"` dynamic attributes from an HTML tag string.
/// Returns the cleaned tag string (with @attrs removed) and the dynamic attrs.
fn extract_dynamic_attrs(raw: &str) -> (String, Vec<(String, String)>) {
    if !raw.contains('@') {
        return (raw.to_string(), Vec::new());
    }

    let mut clean = String::new();
    let mut dynamic_attrs = Vec::new();
    let bytes = raw.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        // Look for @ preceded by whitespace (start of dynamic attr)
        if bytes[i] == b'@'
            && i > 0
            && (bytes[i - 1] == b' ' || bytes[i - 1] == b'\t' || bytes[i - 1] == b'\n')
        {
            let attr_start = i;
            i += 1; // skip @
            let name_start = i;
            while i < bytes.len()
                && bytes[i] != b'='
                && bytes[i] != b' '
                && bytes[i] != b'>'
                && bytes[i] != b'/'
            {
                i += 1;
            }
            let attr_name = &raw[name_start..i];

            // Skip whitespace around =
            while i < bytes.len() && bytes[i] == b' ' {
                i += 1;
            }

            if i < bytes.len() && bytes[i] == b'=' {
                i += 1; // skip =
                while i < bytes.len() && bytes[i] == b' ' {
                    i += 1;
                }

                // Extract expression value
                let expr_str;
                if i < bytes.len() && (bytes[i] == b'"' || bytes[i] == b'\'') {
                    let quote = bytes[i];
                    i += 1;
                    let expr_start = i;
                    while i < bytes.len() && bytes[i] != quote {
                        i += 1;
                    }
                    expr_str = &raw[expr_start..i];
                    if i < bytes.len() {
                        i += 1; // skip closing quote
                    }
                } else {
                    let expr_start = i;
                    while i < bytes.len()
                        && bytes[i] != b' '
                        && bytes[i] != b'\t'
                        && bytes[i] != b'>'
                        && bytes[i] != b'/'
                    {
                        i += 1;
                    }
                    expr_str = &raw[expr_start..i];
                }

                if !expr_str.is_empty() {
                    dynamic_attrs.push((attr_name.to_string(), expr_str.to_string()));
                    // Trim trailing whitespace from clean
                    let trimmed = clean.trim_end().len();
                    clean.truncate(trimmed);
                    if !clean.ends_with('<') && !clean.is_empty() {
                        clean.push(' ');
                    }
                    continue;
                }
            }

            // Not a valid @attr pattern — copy literally
            clean.push_str(&raw[attr_start..i]);
            continue;
        }

        clean.push(bytes[i] as char);
        i += 1;
    }

    (clean, dynamic_attrs)
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
        let ast = parse_str("$obj.prop.sub");
        let node = &ast.body[0];
        if let NodeKind::VarInterp(e) = &node.kind {
            assert_eq!(e.text(&ast.source), "$obj.prop.sub");
        } else {
            panic!("expected VarInterp");
        }
    }

    // ── Links ───────────────────────────────────────────────────────

    #[test]
    fn simple_link() {
        let node = first_node("[[Kitchen]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "Kitchen"));
            assert!(matches!(&link.target, LinkTarget::Name(s) if s == "Kitchen"));
        } else {
            panic!("expected Link");
        }
    }

    #[test]
    fn link_with_text() {
        let node = first_node("[[Leave|Lobby]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "Leave"));
            assert!(matches!(&link.target, LinkTarget::Name(s) if s == "Lobby"));
        } else {
            panic!("expected Link");
        }
    }

    #[test]
    fn link_with_variable_target() {
        let node = first_node("[[Back|$prevRoom]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "Back"));
            assert!(matches!(&link.target, LinkTarget::Expr(_)));
        } else {
            panic!("expected Link");
        }
    }

    #[test]
    fn link_with_arrow() {
        let node = first_node("[[Go back->Foyer]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "Go back"));
            assert!(matches!(&link.target, LinkTarget::Name(s) if s == "Foyer"));
        } else {
            panic!("expected Link");
        }
    }

    // ── Images ──────────────────────────────────────────────────────

    #[test]
    fn image_simple() {
        let node = first_node("[img[photo.png]]");
        if let NodeKind::Image { src, link } = &node.kind {
            assert_eq!(src, "photo.png");
            assert!(link.is_none());
        } else {
            panic!("expected Image, got {:?}", node.kind);
        }
    }

    #[test]
    fn image_with_link() {
        let node = first_node("[img[photo.png][https://example.com]]");
        if let NodeKind::Image { src, link } = &node.kind {
            assert_eq!(src, "photo.png");
            assert_eq!(link.as_deref(), Some("https://example.com"));
        } else {
            panic!("expected Image, got {:?}", node.kind);
        }
    }

    #[test]
    fn image_in_text() {
        let ast = parse_str("before[img[pic.jpg]]after");
        assert_eq!(ast.body.len(), 3);
        assert!(matches!(&ast.body[0].kind, NodeKind::Text(s) if s == "before"));
        assert!(matches!(&ast.body[1].kind, NodeKind::Image { src, .. } if src == "pic.jpg"));
        assert!(matches!(&ast.body[2].kind, NodeKind::Text(s) if s == "after"));
    }

    // ── Link setters ──────────────────────────────────────────────────

    #[test]
    fn link_with_setter() {
        let node = first_node("[[text|passage][$x to 5]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "text"));
            assert!(matches!(&link.target, LinkTarget::Name(s) if s == "passage"));
            assert_eq!(link.setters.len(), 1, "expected 1 setter, got {:?}", link.setters);
        } else {
            panic!("expected Link");
        }
    }

    #[test]
    fn link_with_multiple_setters() {
        let node = first_node("[[Go|Room][$x to 1][$y to 2]]");
        if let NodeKind::Link(link) = &node.kind {
            assert!(matches!(&link.text, LinkText::Plain(s) if s == "Go"));
            assert!(matches!(&link.target, LinkTarget::Name(s) if s == "Room"));
            assert_eq!(link.setters.len(), 2, "expected 2 setters, got {:?}", link.setters);
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
        assert!(matches!(
            &node.kind,
            NodeKind::HtmlOpen { tag, attrs, .. } if tag == "span" && attrs.iter().any(|(k, _)| k == "class")
        ));
    }

    #[test]
    fn br_tag() {
        let node = first_node("<br>");
        assert!(matches!(&node.kind, NodeKind::HtmlVoid { tag, .. } if tag == "br"));
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
        let src = "<<if $x>>[[Go|Lobby]]<</if>>";
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
        let src = "<<link [[Leave|Menu]]>><<set $x to 1>><</link>>";
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

    #[test]
    fn close_tag_name_mismatch() {
        // <<for>> closed by <</if>> — the for should report unclosed,
        // and the <</if>> should become an orphaned close error.
        let ast = parse_str("<<for _i to 0; _i lt 3; _i++>>body<</if>>");
        assert!(!ast.errors.is_empty());
        let msgs: Vec<&str> = ast.errors.iter().map(|e| e.message.as_str()).collect();
        assert!(
            msgs.iter().any(|m| m.contains("unclosed")),
            "expected 'unclosed' error, got: {msgs:?}"
        );
    }

    #[test]
    fn link_with_apostrophe_in_macro_args() {
        // Apostrophe in passage name inside [[...]] must not be treated as string delimiter
        let src = r#"<<link [[Bob's House|BobsHouse]]>><<set $x to 1>><</link>>"#;
        let ast = parse_str(src);
        assert!(ast.errors.is_empty(), "errors: {:?}", ast.errors);
        let node = &ast.body[0];
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "link");
        } else {
            panic!("expected Macro");
        }
    }

    #[test]
    fn triple_angle_bracket_as_text() {
        // <<< should be treated as literal text, not cause errors
        let ast = parse_str("before<<<after");
        assert!(ast.errors.is_empty(), "errors: {:?}", ast.errors);
    }

    #[test]
    fn radiobutton_is_self_closing() {
        // radiobutton should not consume following content as body
        let src = r#"<<if $x>><<radiobutton "$choice" 1>> Option 1<</if>>"#;
        let ast = parse_str(src);
        assert!(ast.errors.is_empty(), "errors: {:?}", ast.errors);
        let node = &ast.body[0];
        if let NodeKind::Macro(m) = &node.kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 1);
        } else {
            panic!("expected if macro");
        }
    }

    // ── Mixed content ───────────────────────────────────────────────

    #[test]
    fn mixed_content() {
        let src = "Hello $name! <<if $x is 1>><span class=\"blue\">yes</span><<else>>no<</if>> [[Back|Menu]]";
        let ast = parse_str(src);
        assert!(ast.errors.is_empty(), "errors: {:?}", ast.errors);
        // Should have: Text, VarInterp, Text, Macro(if), Text, Link
        assert!(ast.body.len() >= 4);
    }

    #[test]
    fn complex_passage() {
        let src = r#"<<set $score to 0>><<set $room to "lobby">>

<<if $config.debug is 1>>
	<img class="icon" src="img/debug.png">
<</if>>

[[Kitchen]]
[[Attic]]"#;
        let ast = parse_str(src);
        assert!(ast.errors.is_empty(), "errors: {:?}", ast.errors);
        // Should parse without errors
        let macro_count = ast.body.iter().filter(|n| matches!(n.kind, NodeKind::Macro(_))).count();
        assert!(macro_count >= 3, "expected at least 3 macros, got {}", macro_count);
    }

    // ── Navigation macro [[...]] link syntax ─────────────────────────

    fn extract_macro_args(src: &str) -> MacroArgs {
        let ast = parse_str(src);
        let node = ast.body.into_iter().next().expect("expected node");
        match node.kind {
            NodeKind::Macro(m) => m.args,
            other => panic!("expected macro, got {:?}", other),
        }
    }

    #[test]
    fn back_link_pipe_variable() {
        // <<back [[game|$return]]>> → Expr pointing to "$return"
        let src = "<<back [[game|$return]]>>";
        let ast = parse_str(src);
        let node = ast.body.into_iter().next().unwrap();
        if let NodeKind::Macro(m) = node.kind {
            if let MacroArgs::Expr(e) = &m.args {
                assert_eq!(e.text(&ast.source), "$return");
            } else {
                panic!("expected Expr, got {:?}", m.args);
            }
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn goto_link_bare_passage() {
        // <<goto [[Room]]>> → Raw("Room") since it's a literal passage name
        let args = extract_macro_args("<<goto [[Room]]>>");
        match args {
            MacroArgs::Raw(s) => {
                assert_eq!(s, "Room");
            }
            other => panic!("expected Raw, got {:?}", other),
        }
    }

    #[test]
    fn include_link_arrow_right() {
        // <<include [[text->Target]]>> → Raw("Target")
        let args = extract_macro_args("<<include [[text->Target]]>>");
        match args {
            MacroArgs::Raw(s) => {
                assert_eq!(s, "Target");
            }
            other => panic!("expected Raw, got {:?}", other),
        }
    }

    #[test]
    fn return_link_arrow_left() {
        // <<return [[Target<-text]]>> → Raw("Target")
        let args = extract_macro_args("<<return [[Target<-text]]>>");
        match args {
            MacroArgs::Raw(s) => {
                assert_eq!(s, "Target");
            }
            other => panic!("expected Raw, got {:?}", other),
        }
    }

    #[test]
    fn back_link_temp_variable() {
        // <<back [[game|_dest]]>> → Expr pointing to "_dest"
        let src = "<<back [[game|_dest]]>>";
        let ast = parse_str(src);
        let node = ast.body.into_iter().next().unwrap();
        if let NodeKind::Macro(m) = node.kind {
            if let MacroArgs::Expr(e) = &m.args {
                assert_eq!(e.text(&ast.source), "_dest");
            } else {
                panic!("expected Expr, got {:?}", m.args);
            }
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn back_quoted_string_still_works() {
        // <<back "Lobby">> → Expr pointing to `"Lobby"`
        let src = r#"<<back "Lobby">>"#;
        let ast = parse_str(src);
        let node = ast.body.into_iter().next().unwrap();
        if let NodeKind::Macro(m) = node.kind {
            if let MacroArgs::Expr(e) = &m.args {
                assert_eq!(e.text(&ast.source), "\"Lobby\"");
            } else {
                panic!("expected Expr, got {:?}", m.args);
            }
        } else {
            panic!("expected macro");
        }
    }

    #[test]
    fn goto_no_args() {
        // <<goto>> → None (should not panic)
        let args = extract_macro_args("<<goto>>");
        assert!(matches!(args, MacroArgs::None));
    }
}
