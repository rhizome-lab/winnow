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

/// Scan `args_text` for `_varname` tokens and return them as `ExprKind::TempVar` expressions.
///
/// Used for `(macro:)` args which use `type-name _param-name` pairs (no comma between the
/// type annotation and the temp-var). Normal expression parsing drops the `_param` because
/// `parse_expr` consumes the type ident and stops. This scanner works at the byte level.
fn scan_macro_param_names(args_text: &str, base_offset: usize) -> Vec<Expr> {
    let bytes = args_text.as_bytes();
    let mut i = 0;
    let mut params = Vec::new();
    while i < bytes.len() {
        if bytes[i] == b'_' {
            let name_start = i + 1;
            let mut j = name_start;
            while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_') {
                j += 1;
            }
            if j > name_start {
                let name = args_text[name_start..j].to_string();
                let span = Span::new(base_offset + i, base_offset + j);
                params.push(Expr { kind: ExprKind::TempVar(name), span });
                i = j;
                continue;
            }
        }
        i += 1;
    }
    params
}

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
            b'(' => self.parse_macro(in_hook),
            b'[' => {
                if self.peek_at(1) == Some(b'[') {
                    self.parse_link()
                } else {
                    // Bare `[content]` — a right-sided hook (possibly `[content]<name|`).
                    // Parse as a nested hook so the `]` doesn't close an outer hook.
                    self.parse_right_sided_hook()
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
            b'|' if self.looks_like_named_hook() => self.parse_named_hook(),
            _ => self.parse_text(in_hook),
        }
    }

    /// Parse a macro: `(name: args)` with optional trailing `[hook]`.
    fn parse_macro(&mut self, in_hook: bool) -> Option<Node> {
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
        // Harlowe: macro names are case-, dash-, and underscore-insensitive.
        // Normalize to lowercase dash-form: `Go_To` → `go-to`, `GOTO` → `goto`.
        let name = self.source[name_start..self.pos]
            .to_lowercase()
            .replace('_', "-");

        if name.is_empty() {
            // Check for dynamic macro call: `($storyVar: args)` or `(_tempVar: args)`.
            // The callee is a variable holding a `(macro:)` closure.
            let prefix = self.peek();
            if prefix == Some(b'$') || prefix == Some(b'_') {
                let sigil = self.bytes[self.pos] as char;
                self.pos += 1; // skip `$` or `_`
                let var_start = self.pos;
                while self.pos < self.bytes.len()
                    && (self.bytes[self.pos].is_ascii_alphanumeric()
                        || self.bytes[self.pos] == b'_')
                {
                    self.pos += 1;
                }
                let var_name = &self.source[var_start..self.pos];
                if !var_name.is_empty() && self.peek() == Some(b':') {
                    // Valid dynamic macro call — parse as MacroNode with `$name`/`_name`.
                    let dyn_name = format!("{sigil}{var_name}");
                    self.pos += 1; // skip `:`
                    self.skip_whitespace();
                    let args_text = self.extract_balanced_args();
                    let args = self.parse_macro_args(&dyn_name, &args_text, start);
                    if self.peek() == Some(b')') {
                        self.pos += 1;
                    }
                    let macro_end = self.pos;
                    return Some(Node {
                        kind: NodeKind::Macro(MacroNode {
                            name: dyn_name,
                            args,
                            hook: None,
                            clauses: Vec::new(),
                            span: Span::new(start, macro_end),
                        }),
                        span: Span::new(start, macro_end),
                    });
                }
            }
            // Not a macro — could be a parenthesized expression in text, restore
            self.pos = start;
            return self.parse_text(in_hook);
        }

        // Skip whitespace
        self.skip_whitespace();

        // Expect `:` after name (for macros with args) or `)` (no args)
        // `inline_hook`: for `(macro:)`, the CodeHook is INSIDE the parens as the last arg.
        // Other macros have their hook outside (after `)`).
        let (args, inline_hook) = if self.peek() == Some(b':') {
            self.pos += 1; // skip `:`
            self.skip_whitespace();

            // Parse args up to the matching `)`
            let args_text = self.extract_balanced_args();
            if name == "macro" {
                // `(macro: type _param, ..., [CodeHook])` — hook is last arg inside parens.
                self.parse_macro_definition_args(&args_text, start)
            } else {
                (self.parse_macro_args(&name, &args_text, start), None)
            }
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
            return self.parse_text(in_hook);
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

        // Check for attached hook `[...]` (the inline hook takes precedence for `(macro:)`).
        let hook = inline_hook.or_else(|| self.try_parse_hook(macros::expects_hook(&name)));
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
                span: Span::new(start, end),
            }),
            span: Span::new(start, end),
        })
    }

    /// Extract balanced text for macro args up to the matching `)`.
    /// Handles nested `(` `)`, `[` `]` brackets, and string literals.
    /// Inside `[...]` (hook/code content), apostrophes are NOT treated as string
    /// delimiters — they are plain text (e.g. contractions like `doesn't`).
    fn extract_balanced_args(&mut self) -> String {
        let start = self.pos;
        let mut depth = 1; // We're inside the outer `(`
        let mut bracket_depth = 0usize; // depth of `[...]` (hook content)

        while !self.at_end() && depth > 0 {
            match self.bytes[self.pos] {
                b'[' => {
                    bracket_depth += 1;
                    self.pos += 1;
                }
                b']' if bracket_depth > 0 => {
                    // Closing bracket of a nested `[...]` inside args — track depth.
                    bracket_depth -= 1;
                    self.pos += 1;
                }
                b']' => {
                    // `]` at bracket_depth == 0 means we've hit the closing bracket of an
                    // outer hook (e.g. a macro inside `(verbatim:)[...]` that is missing its `)`)
                    // — stop here so we don't consume past the hook boundary.
                    break;
                }
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
                b'"' if bracket_depth == 0 => {
                    self.pos += 1;
                    while !self.at_end() && self.bytes[self.pos] != b'"' {
                        if self.bytes[self.pos] == b'\\' {
                            self.pos += 1; // skip escape
                        }
                        self.pos += 1;
                    }
                    if !self.at_end() {
                        self.pos += 1; // skip closing `"`
                    }
                }
                b'\'' if bracket_depth == 0 => {
                    // Harlowe `'s` possessive — NOT a string delimiter.
                    // Only enter string-scan mode for `'non-s...'` or `'s` followed by
                    // an identifier character (e.g. `'stop'`). A bare `'s ` / `'s(` / `'s,`
                    // is the possessive operator, not a string.
                    let is_possessive = self.bytes.get(self.pos + 1) == Some(&b's')
                        && !self.bytes
                            .get(self.pos + 2)
                            .is_some_and(|c| c.is_ascii_alphanumeric() || *c == b'_');
                    if is_possessive {
                        self.pos += 1; // skip `'` as plain text; `s` scanned next iteration
                    } else {
                        self.pos += 1; // skip opening `'`
                        while !self.at_end() && self.bytes[self.pos] != b'\'' {
                            if self.bytes[self.pos] == b'\\' {
                                self.pos += 1;
                            }
                            self.pos += 1;
                        }
                        if !self.at_end() {
                            self.pos += 1; // skip closing `'`
                        }
                    }
                }
                _ => self.pos += 1,
            }
        }

        self.source[start..self.pos].to_string()
    }

    /// Parse `(macro:)` args: `type-name _param, ..., [CodeHook]`.
    ///
    /// Returns `(param_exprs, Some(hook_nodes))` where params are the `_varname` temp-vars
    /// and `hook_nodes` is the parsed body (the `[CodeHook]` last argument).
    fn parse_macro_definition_args(
        &mut self,
        args_text: &str,
        _base_offset: usize,
    ) -> (Vec<Expr>, Option<Vec<Node>>) {
        // Find the `[CodeHook]` — the first `[` at bracket depth 0 in the args text.
        // Everything before it (stripped of trailing comma) is the params text.
        let bytes = args_text.as_bytes();
        let mut i = 0;
        let mut bracket_start: Option<usize> = None;
        while i < bytes.len() {
            match bytes[i] {
                b'[' => {
                    bracket_start = Some(i);
                    break;
                }
                b'"' => {
                    i += 1;
                    while i < bytes.len() && bytes[i] != b'"' {
                        if bytes[i] == b'\\' {
                            i += 1;
                        }
                        i += 1;
                    }
                    if i < bytes.len() {
                        i += 1; // skip closing `"`
                    }
                }
                b'(' => {
                    // Skip balanced nested parens (inner macros in args are unusual here
                    // but handle them for safety).
                    i += 1;
                    let mut depth = 1usize;
                    while i < bytes.len() && depth > 0 {
                        match bytes[i] {
                            b'(' => {
                                depth += 1;
                                i += 1;
                            }
                            b')' => {
                                depth -= 1;
                                i += 1;
                            }
                            _ => {
                                i += 1;
                            }
                        }
                    }
                }
                _ => {
                    i += 1;
                }
            }
        }

        if let Some(bracket_pos) = bracket_start {
            // Params text: everything before `[`, trailing comma/whitespace stripped.
            let params_text = args_text[..bracket_pos].trim_end_matches(',').trim();
            // Params: scan for `_varname` patterns.
            let params = scan_macro_param_names(params_text, _base_offset);

            // Hook body text: find matching `]`.
            let rest = &args_text[bracket_pos + 1..]; // skip `[`
            let rbytes = rest.as_bytes();
            let mut depth = 1usize;
            let mut j = 0;
            while j < rbytes.len() && depth > 0 {
                match rbytes[j] {
                    b'[' => {
                        depth += 1;
                        j += 1;
                    }
                    b']' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        j += 1;
                    }
                    _ => {
                        j += 1;
                    }
                }
            }
            let hook_text = &rest[..j];
            let hook_nodes = {
                let mut sub = Parser::new(hook_text);
                sub.parse_body(true)
            };
            (params, Some(hook_nodes))
        } else {
            // No `[CodeHook]` found — macro definition with no body (unusual).
            let params = scan_macro_param_names(args_text, _base_offset);
            (params, None)
        }
    }

    /// Parse macro arguments from extracted text.
    fn parse_macro_args(&mut self, name: &str, args_text: &str, base_offset: usize) -> Vec<Expr> {
        if args_text.trim().is_empty() {
            return Vec::new();
        }
        // `(macro:)` uses `type-name _param` pairs — the type annotation and the temp-var
        // name have no comma between them. Normal expression parsing consumes only the type
        // ident and drops `_param`. Scan at the byte level to collect `_varname` tokens.
        if name == "macro" || name.starts_with('$') || name.starts_with('_') {
            return scan_macro_param_names(args_text, base_offset);
        }
        let mut lexer = ExprLexer::new(args_text, base_offset);
        expr::parse_args(&mut lexer)
    }

    /// Try to parse a hook `[...]` immediately after a macro.
    /// When `macro_expects_hook` is true, a leading `[[` inside the hook is
    /// parsed as a link rather than rejecting the entire bracket as a non-hook.
    fn try_parse_hook(&mut self, macro_expects_hook: bool) -> Option<Vec<Node>> {
        // Skip spaces/tabs between macro close `)` and opening `[`.
        // Harlowe allows `(if: $x) [hook]` as well as `(if: $x)[hook]`.
        self.skip_whitespace();
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
            // Skip whitespace, newlines, and `\` erasure markers — Harlowe
            // allows (else:) on a new line or after a `\` whitespace-erasure.
            self.skip_if_clause_gap();

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
            let name = self.source[name_start..self.pos]
                .to_lowercase()
                .replace('_', "-");

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

    /// Parse a right-sided hook: `[content]` or `[content]<name|`.
    ///
    /// Bare `[` in Harlowe content starts a right-sided hook whose body runs to
    /// the matching `]`.  The optional `<name|` suffix gives the hook a name so
    /// it can be targeted by `?name` selectors and `(click:)` / `(replace:)`.
    ///
    /// We must recurse here so that a nested `]` does NOT close an outer hook.
    fn parse_right_sided_hook(&mut self) -> Option<Node> {
        let start = self.pos;
        self.pos += 1; // consume `[`
        let body = self.parse_body(true);
        if self.peek() == Some(b']') {
            self.pos += 1; // consume `]`
        }

        // Check for right-sided name: `<name|`
        if self.peek() == Some(b'<') {
            let saved = self.pos;
            self.pos += 1; // consume `<`
            let name_start = self.pos;
            while self.pos < self.bytes.len()
                && (self.bytes[self.pos].is_ascii_alphanumeric()
                    || self.bytes[self.pos] == b'-'
                    || self.bytes[self.pos] == b'_')
            {
                self.pos += 1;
            }
            let name = self.source[name_start..self.pos].to_string();
            if self.peek() == Some(b'|') && !name.is_empty() {
                self.pos += 1; // consume `|`
                return Some(Node {
                    kind: NodeKind::NamedHook { name, body },
                    span: Span::new(start, self.pos),
                });
            }
            // Not a valid `<name|` — backtrack
            self.pos = saved;
        }

        // Unnamed hook: just render the body inline (wrap in an anonymous hook)
        Some(Node {
            kind: NodeKind::NamedHook {
                name: String::new(),
                body,
            },
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
                    break; // `[` always starts a hook (or `[[` link); stop text here
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
                b'|' if self.looks_like_named_hook() => break,
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

    /// Skip spaces, tabs, newlines, and Harlowe `\` whitespace-erasure markers.
    /// Used in `parse_if_clauses` so that `(else:)` / `(else-if:)` separated
    /// from the preceding `(if:)` hook by only whitespace or `\` are still
    /// recognised as clauses.
    fn skip_if_clause_gap(&mut self) {
        while self.pos < self.bytes.len()
            && matches!(self.bytes[self.pos], b' ' | b'\t' | b'\n' | b'\r' | b'\\')
        {
            self.pos += 1;
        }
    }

    /// Returns `true` if the current position starts a named hook `|name>[...]`.
    fn looks_like_named_hook(&self) -> bool {
        // Must start with `|`
        if self.pos >= self.bytes.len() || self.bytes[self.pos] != b'|' {
            return false;
        }
        // Must be followed by one or more alphanumeric/underscore/hyphen chars
        let mut i = self.pos + 1;
        let name_start = i;
        while i < self.bytes.len()
            && (self.bytes[i].is_ascii_alphanumeric()
                || self.bytes[i] == b'_'
                || self.bytes[i] == b'-')
        {
            i += 1;
        }
        if i == name_start {
            return false; // no name chars
        }
        // Must be followed by `>` or `)` (alternative Harlowe hook-open terminators)
        if i >= self.bytes.len() || (self.bytes[i] != b'>' && self.bytes[i] != b')') {
            return false;
        }
        // The terminator must be followed (possibly with whitespace) by `[`
        let mut j = i + 1;
        while j < self.bytes.len() && (self.bytes[j] == b' ' || self.bytes[j] == b'\t') {
            j += 1;
        }
        j < self.bytes.len() && self.bytes[j] == b'['
    }

    /// Parse a named hook `|name>[...]`.
    /// Called when `looks_like_named_hook()` is true.
    fn parse_named_hook(&mut self) -> Option<Node> {
        let start = self.pos;
        self.pos += 1; // skip `|`

        // Collect the name
        let name_start = self.pos;
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos].is_ascii_alphanumeric()
                || self.bytes[self.pos] == b'_'
                || self.bytes[self.pos] == b'-')
        {
            self.pos += 1;
        }
        let name = self.source[name_start..self.pos].to_string();
        self.pos += 1; // skip `>` or `)`

        // Skip optional whitespace before `[`
        while self.pos < self.bytes.len()
            && (self.bytes[self.pos] == b' ' || self.bytes[self.pos] == b'\t')
        {
            self.pos += 1;
        }
        self.pos += 1; // skip `[`

        let body = self.parse_body(true);
        if !self.at_end() && self.bytes[self.pos] == b']' {
            self.pos += 1; // consume `]`
        }

        Some(Node {
            kind: NodeKind::NamedHook { name, body },
            span: Span::new(start, self.pos),
        })
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
    fn test_if_else_newline() {
        // (else:) on a new line must still be collected as a clause, not emitted standalone
        let ast = parse("(if: $x is 1)[yes]\n(else:)[no]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1, "else should be a clause, not a sibling node");
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 1);
            assert_eq!(m.clauses[0].kind, "else");
        } else {
            panic!("expected if macro");
        }
    }

    #[test]
    fn test_else_if_newline() {
        // (else-if:) on a new line must still be collected as a clause
        let ast = parse("(if: $x is 1)[yes]\n(else-if: $x is 2)[maybe]\n(else:)[no]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 2);
            assert_eq!(m.clauses[0].kind, "else-if");
            assert_eq!(m.clauses[1].kind, "else");
        } else {
            panic!("expected if macro");
        }
    }

    #[test]
    fn test_elseif_alias_clause() {
        // (elseif:) is an alias for (else-if:) and must be collected as a clause
        let ast = parse("(if: $x < 4)[(A)](elseif: $x < 8)[(B)](elseif: $x < 12)[(C)](else:)[(D)]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1, "elseif/else should be clauses, not siblings");
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 3);
            assert_eq!(m.clauses[0].kind, "elseif");
            assert_eq!(m.clauses[1].kind, "elseif");
            assert_eq!(m.clauses[2].kind, "else");
        } else {
            panic!("expected if macro");
        }
    }

    #[test]
    fn test_inline_macro_in_args_then_else() {
        // (if: (passage:)'s tags contains "x")[(set: $t to (passage:)'s tags)](else:)[(set: $t to "")]
        // The nested (passage:) macro in the expression must not confuse else-clause collection.
        let ast = parse(r#"(if: (passage:)'s tags contains "x")[(set: $t to (passage:)'s tags)](else:)[(set: $t to "")]"#);
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1, "(else:) must be a clause, not a sibling");
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 1);
            assert_eq!(m.clauses[0].kind, "else");
        } else {
            panic!("expected if macro");
        }
    }

    #[test]
    fn test_prose_parens_in_hook_followed_by_else() {
        // Bug: parse_macro called parse_text(false) instead of parse_text(in_hook),
        // so prose like `(Some text)` inside a hook consumed the closing `]` and
        // left `(else:)` as a standalone sibling instead of a clause.
        let ast = parse("(if: $x)[Some text (in parens)](else:)[fallback]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1, "(else:) must be a clause, not a sibling");
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 1);
            assert_eq!(m.clauses[0].kind, "else");
        } else {
            panic!("expected if macro");
        }
    }

    #[test]
    fn test_elseif_after_changer_and_interp() {
        // Reproduces: (text-colour:)[text] $var (if: cond)[...](elseif: cond)[...]
        // The (elseif:) must still be a clause of (if:), not a standalone node
        let src = r#"(text-colour: "Pink")[Label:] $val (if: $val < 4)[(A)](elseif: $val < 8)[(B)](elseif: $val < 12)[(C)](else:)[(D)]"#;
        let ast = parse(src);
        assert_eq!(ast.errors.len(), 0);
        // Should have 3 top-level nodes: changer-apply, var-interp, and if
        // (The exact structure depends on changer lowering, but if-node must have 3 clauses)
        let if_node = ast.body.iter().find(|n| matches!(&n.kind, NodeKind::Macro(m) if m.name == "if"));
        let m = match if_node.map(|n| &n.kind) {
            Some(NodeKind::Macro(m)) => m,
            _ => panic!("expected if macro in AST"),
        };
        assert_eq!(m.clauses.len(), 3, "elseif/else must be clauses of if, not siblings");
        assert_eq!(m.clauses[0].kind, "elseif");
        assert_eq!(m.clauses[1].kind, "elseif");
        assert_eq!(m.clauses[2].kind, "else");
        // No standalone elseif/else nodes
        for node in &ast.body {
            if let NodeKind::Macro(mac) = &node.kind {
                assert_ne!(mac.name, "elseif", "standalone (elseif:) found");
                assert_ne!(mac.name, "else", "standalone (else:) found");
            }
        }
    }

    #[test]
    fn test_elseif_exact_thoughts_main() {
        // Exact excerpt from ThoughtsMain passage in the-experiment
        let src = r#"(text-colour: "Red")[Defiance:] $Defiance (if: $Defiance < 4)[(Next Trait at 4)](elseif: $Defiance < 8)[(Next Trait at 8)](elseif: $Defiance < 12)[(Next Trait at 12)](elseif: $Defiance < 17)[(Next Trait at 17)](else:)[(All Defiance Traits Active)]"#;
        let ast = parse(src);
        assert_eq!(ast.errors.len(), 0);
        // Verify no standalone elseif/else
        for node in &ast.body {
            if let NodeKind::Macro(mac) = &node.kind {
                assert!(
                    mac.name != "elseif" && mac.name != "else-if" && mac.name != "else",
                    "standalone clause macro '{}' found in top-level body", mac.name
                );
            }
        }
        let if_node = ast.body.iter().find(|n| matches!(&n.kind, NodeKind::Macro(m) if m.name == "if"));
        let m = match if_node.map(|n| &n.kind) {
            Some(NodeKind::Macro(m)) => m,
            _ => panic!("expected if macro"),
        };
        assert_eq!(m.clauses.len(), 4, "expected 3 elseif + 1 else");
    }

    #[test]
    fn test_else_after_backslash() {
        // `]\` (whitespace-erasure) between hook and (else:) must still be a clause
        let ast = parse("(if: $x is 1)[yes]\\ \n(else:)[no]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1, "else after backslash should be a clause");
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 1);
            assert_eq!(m.clauses[0].kind, "else");
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
        let ast = parse("(goto: \"Some Passage\")");
        assert_eq!(ast.errors.len(), 0);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "goto");
            assert_eq!(m.args.len(), 1);
            assert!(matches!(&m.args[0].kind, ExprKind::Str(s) if s == "Some Passage"));
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
        let src = r#"(set: $location to "the plaza")
You're at the **plaza**

(if: $fearStat < 70)[Normal text.](else:)[(color: magenta+white)[Spooked text.]]"#;
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
    fn test_right_sided_hook_named() {
        // [content]<name| should produce a NamedHook, not close an outer hook
        let ast = parse("[Click me]<button|");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1);
        if let NodeKind::NamedHook { name, body } = &ast.body[0].kind {
            assert_eq!(name, "button");
            assert_eq!(body.len(), 1);
        } else {
            panic!("expected NamedHook");
        }
    }

    #[test]
    fn test_right_sided_hook_does_not_close_outer() {
        // [label]<name| inside an if-hook must NOT close the if-hook early
        let ast =
            parse("(if: $x is 1)[\\ \n    [Sneak]<sneak|\n](else:)[no]");
        assert_eq!(ast.errors.len(), 0);
        assert_eq!(ast.body.len(), 1, "else should be a clause, not a sibling");
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "if");
            assert_eq!(m.clauses.len(), 1, "should have else clause");
            assert_eq!(m.clauses[0].kind, "else");
        } else {
            panic!("expected if macro");
        }
    }

    #[test]
    fn test_apostrophe_in_hook_literal_arg_does_not_unclos_macro() {
        // Regression: apostrophes in hook literals used as macro args (e.g. `(prompt: [label])`)
        // were incorrectly treated as string delimiters in `extract_balanced_args`, causing
        // the outer `(set:` to appear unclosed.
        // Source pattern: `(set: $x to (prompt: [It doesn't matter.], "", ""))`
        let ast = parse(
            r#"(link: "Go")[(set: $x to (prompt: [It doesn't matter.], "", ""))(goto: "next")]"#,
        );
        assert_eq!(ast.errors.len(), 0, "apostrophe in hook arg should not break macro parsing");
    }

    #[test]
    fn test_unclosed_macro_inside_verbatim_hook_does_not_consume_past_bracket() {
        // Regression: an unclosed macro inside `(verbatim:)[...]` was consuming past the `]`
        // that closes the verbatim hook, making the span extend to end-of-passage.
        // Now `extract_balanced_args` stops at `]` when bracket_depth==0.
        let ast = parse("before (verbatim:)[(set: bad macro] after");
        // The (set: is unclosed — we expect 1 parse error, but the 'after' text
        // must still be visible as a sibling node (not swallowed).
        assert_eq!(ast.errors.len(), 1);
        // 'before ' and 'after' should both be parsed
        let text_nodes: Vec<_> = ast
            .body
            .iter()
            .filter(|n| matches!(n.kind, NodeKind::Text(_)))
            .collect();
        assert!(!text_nodes.is_empty(), "text before verbatim should be present");
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
