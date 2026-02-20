//! Pratt parser for Harlowe expressions.
//!
//! Precedence (low to high):
//!   `to` (assignment in set:)
//!   `or`
//!   `and`
//!   `is` / `is not` / `contains` / `is in`
//!   `<` / `>` / `<=` / `>=`
//!   `+` / `-`
//!   `*` / `/` / `%`
//!   `not` / unary `-`
//!   `'s` / `of`
//!   atoms

use super::ast::*;
use super::lexer::{ExprLexer, TokenKind};

/// Interpret a property-name string as an ordinal expression.
/// Handles all Harlowe ordinal forms: `1st`, `2ndlast`, `last`, `length`,
/// `1stto4th`, `2ndto2ndlast`, `lasttolast`, etc.
/// Returns `None` if the string is not an ordinal form (caller treats as plain ident).
fn interpret_property_string(s: &str, span: Span) -> Option<Expr> {
    let ord = if s == "last" {
        Ordinal::Last
    } else if s == "length" {
        Ordinal::Length
    } else if let Some((from, to)) = try_parse_range_str(s) {
        Ordinal::Range { from, to }
    } else if let Some(n) = try_parse_nth_last_str(s) {
        Ordinal::NthLast(n)
    } else if let Some(n) = try_parse_nth_str(s) {
        Ordinal::Nth(n)
    } else {
        return None;
    };
    Some(Expr {
        kind: ExprKind::Ordinal(ord),
        span,
    })
}

/// Try to parse a range ordinal string like `1stto4th`, `2ndto2ndlast`, `lasttolast`.
/// Finds the `to` separator and parses both endpoints.
fn try_parse_range_str(s: &str) -> Option<(RangeEnd, RangeEnd)> {
    let bytes = s.as_bytes();
    let mut i = 0;
    while i + 2 <= bytes.len() {
        if &bytes[i..i + 2] == b"to" {
            let before = &s[..i];
            let after = &s[i + 2..];
            if let (Some(from), Some(to)) =
                (parse_range_end_str(before), parse_range_end_str(after))
            {
                return Some((from, to));
            }
        }
        i += 1;
    }
    None
}

/// Parse one endpoint of a range: `last`, `2ndlast`, `1st`, etc.
fn parse_range_end_str(s: &str) -> Option<RangeEnd> {
    if s == "last" {
        return Some(RangeEnd::Last);
    }
    if let Some(rest) = s.strip_suffix("last") {
        if let Some(n) = try_parse_nth_str(rest) {
            return Some(RangeEnd::NthLast(n));
        }
    }
    try_parse_nth_str(s).map(RangeEnd::Nth)
}

/// Parse a reverse ordinal string: `2ndlast` → `Some(2)`, `3rdlast` → `Some(3)`.
fn try_parse_nth_last_str(s: &str) -> Option<u32> {
    s.strip_suffix("last").and_then(try_parse_nth_str)
}

/// Parse a forward ordinal string: `1st` → `Some(1)`, `2nd` → `Some(2)`, etc.
fn try_parse_nth_str(s: &str) -> Option<u32> {
    if s.len() < 3 {
        return None;
    }
    let (num_part, suffix) = s.split_at(s.len() - 2);
    if matches!(suffix, "st" | "nd" | "rd" | "th") {
        if let Ok(n) = num_part.parse::<u32>() {
            if n > 0 {
                return Some(n);
            }
        }
    }
    None
}

/// Precedence levels for Harlowe operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Prec {
    None = 0,
    Assign = 1,   // `to`
    Or = 2,       // `or`
    And = 3,      // `and`
    Equality = 4, // `is`, `is not`, `contains`, `is in`
    Compare = 5,  // `<`, `>`, `<=`, `>=`
    Add = 6,      // `+`, `-`
    Mul = 7,      // `*`, `/`, `%`
    Unary = 8,    // `not`, unary `-`
    Access = 9,   // `'s`, `of`
}

/// Parse a single Harlowe expression.
pub fn parse_expr(lexer: &mut ExprLexer) -> Expr {
    parse_prec(lexer, Prec::None)
}

/// Parse a comma-separated argument list (for macro args).
pub fn parse_args(lexer: &mut ExprLexer) -> Vec<Expr> {
    let mut args = Vec::new();
    let first = parse_expr(lexer);
    if matches!(first.kind, ExprKind::Error(_)) && lexer.peek_token().kind == TokenKind::Eof {
        return args;
    }
    args.push(first);

    while lexer.peek_token().kind == TokenKind::Comma {
        lexer.next_token(); // consume comma
        args.push(parse_expr(lexer));
    }
    args
}

fn parse_prec(lexer: &mut ExprLexer, min_prec: Prec) -> Expr {
    let mut left = parse_prefix(lexer);

    loop {
        let tok = lexer.peek_token();

        // Handle possessive `'s` — the Apostrophe token is emitted only for `'s\s+`.
        // Scan the property name as a whole word (Harlowe captures the entire compound
        // string like `2ndlast` or `1stto4th` as one unit via regex).
        if matches!(tok.kind, TokenKind::Apostrophe) && min_prec <= Prec::Access {
            lexer.next_token(); // consume `'s`
            let property = if let Some((word, word_span)) = lexer.scan_word() {
                // Interpret the entire property string (handles ordinal, range, named property)
                interpret_property_string(&word, word_span)
                    .unwrap_or(Expr { kind: ExprKind::Ident(word), span: word_span })
            } else {
                // Computed property: `$arr's (expr)`
                parse_prefix(lexer)
            };
            let span = Span::new(left.span.start, property.span.end);
            left = Expr {
                kind: ExprKind::Possessive {
                    object: Box::new(left),
                    property: Box::new(property),
                },
                span,
            };
            continue;
        }

        // Handle `of` — reverse possessive: `1st of $arr`.
        if matches!(tok.kind, TokenKind::Ident(ref s) if s == "of") && min_prec <= Prec::Access {
            lexer.next_token(); // consume `of`
            let object = parse_prec(lexer, Prec::Access);
            let span = Span::new(left.span.start, object.span.end);
            left = Expr {
                kind: ExprKind::Of {
                    property: Box::new(left),
                    object: Box::new(object),
                },
                span,
            };
            continue;
        }

        let (op, prec) = match &tok.kind {
            TokenKind::To if min_prec <= Prec::Assign => (None, Prec::Assign),
            TokenKind::Or if min_prec <= Prec::Or => (Some(BinaryOp::Or), Prec::Or),
            TokenKind::And if min_prec <= Prec::And => (Some(BinaryOp::And), Prec::And),
            TokenKind::Is if min_prec <= Prec::Equality => (Some(BinaryOp::Is), Prec::Equality),
            TokenKind::IsNot if min_prec <= Prec::Equality => {
                (Some(BinaryOp::IsNot), Prec::Equality)
            }
            TokenKind::Contains if min_prec <= Prec::Equality => {
                (Some(BinaryOp::Contains), Prec::Equality)
            }
            TokenKind::IsIn if min_prec <= Prec::Equality => {
                (Some(BinaryOp::IsIn), Prec::Equality)
            }
            TokenKind::IsNotIn if min_prec <= Prec::Equality => {
                (Some(BinaryOp::IsNotIn), Prec::Equality)
            }
            TokenKind::Lt if min_prec <= Prec::Compare => (Some(BinaryOp::Lt), Prec::Compare),
            TokenKind::Gt if min_prec <= Prec::Compare => (Some(BinaryOp::Gt), Prec::Compare),
            TokenKind::Lte if min_prec <= Prec::Compare => (Some(BinaryOp::Lte), Prec::Compare),
            TokenKind::Gte if min_prec <= Prec::Compare => (Some(BinaryOp::Gte), Prec::Compare),
            TokenKind::Plus if min_prec <= Prec::Add => (Some(BinaryOp::Add), Prec::Add),
            TokenKind::Minus if min_prec <= Prec::Add => (Some(BinaryOp::Sub), Prec::Add),
            TokenKind::Star if min_prec <= Prec::Mul => (Some(BinaryOp::Mul), Prec::Mul),
            TokenKind::Slash if min_prec <= Prec::Mul => (Some(BinaryOp::Div), Prec::Mul),
            TokenKind::Percent if min_prec <= Prec::Mul => (Some(BinaryOp::Mod), Prec::Mul),
            _ => break,
        };

        lexer.next_token(); // consume the operator

        // Use next-higher precedence for left-associativity
        let next_prec = match prec {
            Prec::Assign => Prec::Or,
            Prec::Or => Prec::And,
            Prec::And => Prec::Equality,
            Prec::Equality => Prec::Compare,
            Prec::Compare => Prec::Add,
            Prec::Add => Prec::Mul,
            Prec::Mul => Prec::Unary,
            _ => Prec::Access,
        };

        match op {
            Some(bin_op) => {
                let right = parse_prec(lexer, next_prec);

                // Harlowe `is`/`is not` distribution: `$x is A or B` means
                // `($x is A) or ($x is B)`. When or/and follows a comparison
                // and the RHS is a bare value (not itself a comparison),
                // distribute the comparison subject.
                let right = if matches!(bin_op, BinaryOp::Or | BinaryOp::And) {
                    maybe_distribute_comparison(&left, right)
                } else {
                    right
                };

                let span = Span::new(left.span.start, right.span.end);
                left = Expr {
                    kind: ExprKind::Binary {
                        op: bin_op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    span,
                };
            }
            None => {
                // `to` — assignment used in `(set:)`
                let right = parse_prec(lexer, Prec::Or);
                let span = Span::new(left.span.start, right.span.end);
                left = Expr {
                    kind: ExprKind::Assign {
                        target: Box::new(left),
                        value: Box::new(right),
                    },
                    span,
                };
            }
        }
    }

    left
}

/// Harlowe distributes comparisons across `or`/`and`:
/// `$x is 0 or ""` → `($x is 0) or ($x is "")`.
/// When `left` is `(subject IS/ISNOT value)` and `right` is a bare value
/// (not itself a comparison), wrap `right` in the same comparison.
fn maybe_distribute_comparison(left: &Expr, right: Expr) -> Expr {
    if let ExprKind::Binary {
        op: cmp_op @ (BinaryOp::Is | BinaryOp::IsNot),
        left: ref subject,
        ..
    } = left.kind
    {
        // Only distribute if the RHS isn't already a comparison
        if !matches!(
            right.kind,
            ExprKind::Binary {
                op: BinaryOp::Is
                    | BinaryOp::IsNot
                    | BinaryOp::Lt
                    | BinaryOp::Lte
                    | BinaryOp::Gt
                    | BinaryOp::Gte
                    | BinaryOp::Contains
                    | BinaryOp::IsIn
                    | BinaryOp::IsNotIn,
                ..
            }
        ) {
            let span = Span::new(subject.span.start, right.span.end);
            return Expr {
                kind: ExprKind::Binary {
                    op: cmp_op,
                    left: subject.clone(),
                    right: Box::new(right),
                },
                span,
            };
        }
    }
    right
}

fn parse_prefix(lexer: &mut ExprLexer) -> Expr {
    let tok = lexer.peek_token();
    match tok.kind {
        TokenKind::String(ref s) => {
            let s = s.clone();
            lexer.next_token();
            Expr {
                kind: ExprKind::Str(s),
                span: tok.span,
            }
        }
        TokenKind::True => {
            lexer.next_token();
            Expr {
                kind: ExprKind::Bool(true),
                span: tok.span,
            }
        }
        TokenKind::False => {
            lexer.next_token();
            Expr {
                kind: ExprKind::Bool(false),
                span: tok.span,
            }
        }
        TokenKind::It => {
            lexer.next_token();
            Expr {
                kind: ExprKind::It,
                span: tok.span,
            }
        }
        TokenKind::StoryVar(ref name) => {
            let name = name.clone();
            lexer.next_token();
            Expr {
                kind: ExprKind::StoryVar(name),
                span: tok.span,
            }
        }
        TokenKind::TempVar(ref name) => {
            let name = name.clone();
            lexer.next_token();
            Expr {
                kind: ExprKind::TempVar(name),
                span: tok.span,
            }
        }
        TokenKind::TimeLiteral(secs) => {
            lexer.next_token();
            Expr {
                kind: ExprKind::TimeLiteral(secs),
                span: tok.span,
            }
        }
        TokenKind::Not => {
            lexer.next_token();
            let operand = parse_prec(lexer, Prec::Unary);
            let span = Span::new(tok.span.start, operand.span.end);
            Expr {
                kind: ExprKind::Unary {
                    op: UnaryOp::Not,
                    operand: Box::new(operand),
                },
                span,
            }
        }
        TokenKind::Minus => {
            lexer.next_token();
            let operand = parse_prec(lexer, Prec::Unary);
            let span = Span::new(tok.span.start, operand.span.end);
            Expr {
                kind: ExprKind::Unary {
                    op: UnaryOp::Neg,
                    operand: Box::new(operand),
                },
                span,
            }
        }
        TokenKind::LParen => {
            let start = tok.span.start;
            lexer.next_token(); // consume `(`
            // Could be a sub-expression or an inline macro call like `(random: 1, 5)`
            let next = lexer.peek_token();
            if let TokenKind::Ident(ref name) = next.kind {
                let name = name.clone();
                // Check if this is a macro/function call: `(name:` pattern
                let saved = lexer.pos();
                lexer.next_token(); // consume ident
                let after = lexer.peek_token();

                // Handle colon (macro call) or no-arg macro (just rparen)
                if matches!(after.kind, TokenKind::RParen) && is_value_macro(&name) {
                    // No-arg macro call like `(saved-games:)` — wait, that has colon
                    // Actually `(it)` is just grouping. Let's check for colon.
                    lexer.pos = saved;
                    // Fall through to grouped expr
                } else if after.span.start < after.span.end || matches!(after.kind, TokenKind::Eof)
                {
                    // Check raw input for colon
                    // The lexer doesn't produce a Colon token because `:` ends macro args.
                    // We need to check if there's a `:` right after the ident in the raw input.
                    // Actually, in the expression parser, macro calls are `(name: args)` —
                    // but the outer parser already parses macros. Inside expressions,
                    // we mainly see things like `(saved-games:)` or `(random: 1, 5)`.
                    // The colon isn't in our lexer. Let's handle this differently.
                    lexer.pos = saved;
                }
            }

            // Try parsing as a macro call by checking raw input for `name:` pattern
            let inner_result = try_parse_inline_macro(lexer, start);
            if let Some(expr) = inner_result {
                return expr;
            }

            // Otherwise, grouped expression
            let inner = parse_expr(lexer);
            let end_tok = lexer.peek_token();
            if matches!(end_tok.kind, TokenKind::RParen) {
                lexer.next_token();
            }
            let span = Span::new(start, lexer.pos());
            Expr {
                kind: ExprKind::Paren(Box::new(inner)),
                span,
            }
        }
        TokenKind::Number(n) => {
            // Numbers can be the start of compound ordinals like `2ndlast` or `1stto4th`.
            // The lexer splits `2ndlast` as Number(2) + Ident("ndlast") because the ordinal guard
            // rejects suffixes followed by more alpha. Detect and reassemble here.
            lexer.next_token();
            let n_int = n as u32;
            if n_int > 0 && n == f64::from(n_int) {
                let next = lexer.peek_token();
                if let TokenKind::Ident(ref suffix) = next.kind {
                    // Ordinal suffix idents start with st/nd/rd/th (the part after the digits)
                    if suffix.starts_with("st")
                        || suffix.starts_with("nd")
                        || suffix.starts_with("rd")
                        || suffix.starts_with("th")
                    {
                        let combined = format!("{n_int}{suffix}");
                        let end_span = next.span.end;
                        lexer.next_token(); // consume the suffix ident
                        if let Some(expr) =
                            interpret_property_string(&combined, Span::new(tok.span.start, end_span))
                        {
                            return expr;
                        }
                    }
                }
            }
            Expr {
                kind: ExprKind::Number(n),
                span: tok.span,
            }
        }
        TokenKind::Ident(ref s) => {
            let s = s.clone();
            lexer.next_token();
            // `bind $var` / `2bind $var` — Harlowe bound variable reference.
            // For decompilation, treat as the variable's current value by
            // parsing (and returning) the following variable expression.
            if s == "bind" || s == "2bind" {
                let next = lexer.peek_token();
                if matches!(next.kind, TokenKind::StoryVar(_) | TokenKind::TempVar(_)) {
                    return parse_prefix(lexer);
                }
            }
            // `when expr` — Harlowe 3.3+ storylet/event condition prefix.
            // `when` is a no-op keyword that introduces the condition; strip it
            // and return the condition expression directly.
            if s == "when" {
                return parse_prec(lexer, Prec::Or);
            }
            // `via expr` — Harlowe transform lambda used in `(altered:)`.
            // Wraps the following expression; `it`/`its` inside `expr` refers
            // to the current element being transformed.
            if s == "via" {
                let span_start = tok.span.start;
                let body = parse_prec(lexer, Prec::Or);
                let span = Span::new(span_start, body.span.end);
                return Expr {
                    kind: ExprKind::ViaLambda(Box::new(body)),
                    span,
                };
            }
            // Try to interpret as ordinal (handles `last`, `length`, `lasttolast`, etc.)
            if let Some(expr) = interpret_property_string(&s, tok.span) {
                return expr;
            }
            // Check for color names
            if is_color_name(&s) {
                // Check for color composition with `+`: `magenta+white`
                return parse_color_expr(lexer, s, tok.span);
            }
            Expr {
                kind: ExprKind::Ident(s),
                span: tok.span,
            }
        }
        TokenKind::QuestionMark => {
            // Hook selector: `?name` or `?1` — targets named hooks
            let start = tok.span.start;
            lexer.next_token(); // consume `?`
            let name_tok = lexer.peek_token();
            // Accept both identifier names (?cond) and numeric names (?1, ?2)
            let name = match &name_tok.kind {
                TokenKind::Ident(ref s) => {
                    let n = s.clone();
                    lexer.next_token();
                    n
                }
                TokenKind::Number(n) => {
                    // Format integer-like numbers without decimal point
                    let n = *n;
                    lexer.next_token();
                    if n.fract() == 0.0 {
                        format!("{}", n as i64)
                    } else {
                        format!("{n}")
                    }
                }
                _ => {
                    return Expr {
                        kind: ExprKind::Error(format!(
                            "expected hook name after `?`, got {:?}",
                            name_tok.kind
                        )),
                        span: name_tok.span,
                    };
                }
            };
            let span = Span::new(start, lexer.pos());
            Expr {
                kind: ExprKind::HookSelector(name),
                span,
            }
        }
        TokenKind::Each => {
            // Lambda: `each _var` or `each _var where condition`
            let start = tok.span.start;
            lexer.next_token(); // consume `each`
            // Expect a temp variable (_var)
            let var_tok = lexer.peek_token();
            let var_name = if let TokenKind::TempVar(ref name) = var_tok.kind {
                let n = name.clone();
                lexer.next_token(); // consume _var
                n
            } else {
                return Expr {
                    kind: ExprKind::Error(format!(
                        "expected temp variable after `each`, got {:?}",
                        var_tok.kind
                    )),
                    span: var_tok.span,
                };
            };
            // Optional `making _acc via body` — fold lambda for `(folded:)`.
            if let TokenKind::Ident(ref kw) = lexer.peek_token().kind.clone() {
                if kw == "making" {
                    lexer.next_token(); // consume `making`
                    let acc_tok = lexer.peek_token();
                    if let TokenKind::TempVar(ref acc) = acc_tok.kind.clone() {
                        let acc_var = acc.clone();
                        lexer.next_token(); // consume _acc
                        // Expect `via body`
                        if let TokenKind::Ident(ref via_kw) = lexer.peek_token().kind.clone() {
                            if via_kw == "via" {
                                lexer.next_token(); // consume `via`
                                let body = parse_prec(lexer, Prec::Or);
                                let span = Span::new(start, body.span.end);
                                return Expr {
                                    kind: ExprKind::FoldLambda {
                                        item_var: var_name,
                                        acc_var,
                                        body: Box::new(body),
                                    },
                                    span,
                                };
                            }
                        }
                    }
                }
            }
            // Optional `where condition`
            let filter = if matches!(lexer.peek_token().kind, TokenKind::Where) {
                lexer.next_token(); // consume `where`
                Some(Box::new(parse_prec(lexer, Prec::Or)))
            } else {
                None
            };
            let span = Span::new(start, lexer.pos());
            Expr {
                kind: ExprKind::Lambda {
                    var: var_name,
                    filter,
                },
                span,
            }
        }
        TokenKind::Ellipsis => {
            // Spread: `...expr`
            let start = tok.span.start;
            lexer.next_token(); // consume `...`
            let inner = parse_prec(lexer, Prec::Unary);
            let span = Span::new(start, inner.span.end);
            Expr {
                kind: ExprKind::Spread(Box::new(inner)),
                span,
            }
        }
        // Standalone `where condition` — shorthand for `each _it where condition`.
        // Used in `(open-storylets: where ...)` and similar collection predicates.
        // The implicit variable is named "it" so that `its X` in the condition
        // resolves to `it`'s possessive (same as in `via` / ViaLambda context).
        TokenKind::Where => {
            let start = tok.span.start;
            lexer.next_token(); // consume `where`
            let filter = parse_prec(lexer, Prec::Or);
            let span = Span::new(start, filter.span.end);
            Expr {
                kind: ExprKind::Lambda {
                    var: "it".to_string(),
                    filter: Some(Box::new(filter)),
                },
                span,
            }
        }
        TokenKind::Eof => Expr {
            kind: ExprKind::Error("unexpected end of expression".to_string()),
            span: tok.span,
        },
        _ => {
            lexer.next_token();
            Expr {
                kind: ExprKind::Error(format!("unexpected token: {:?}", tok.kind)),
                span: tok.span,
            }
        }
    }
}

/// Try to parse an inline macro call `(name: args)` inside an expression.
/// The opening `(` has already been consumed. Returns None if not a macro call.
fn try_parse_inline_macro(lexer: &mut ExprLexer, start: usize) -> Option<Expr> {
    let saved = lexer.pos();
    let tok = lexer.peek_token();

    // Dynamic macro call: `($storyVar: args)` or `(_tempVar: args)`.
    // The callee is a variable holding a custom macro created with `(macro:)`.
    if let TokenKind::StoryVar(ref name) | TokenKind::TempVar(ref name) = tok.kind {
        let is_story = matches!(tok.kind, TokenKind::StoryVar(_));
        let callee_name = name.clone();
        let callee_span = tok.span;
        lexer.next_token(); // consume $var or _var

        if matches!(lexer.peek_token().kind, TokenKind::Colon) {
            lexer.next_token(); // consume `:`
            let after_colon = lexer.peek_token();
            let args = if matches!(after_colon.kind, TokenKind::RParen) {
                Vec::new()
            } else {
                parse_args(lexer)
            };
            if matches!(lexer.peek_token().kind, TokenKind::RParen) {
                lexer.next_token(); // consume `)`
            }
            let callee_kind = if is_story {
                ExprKind::StoryVar(callee_name)
            } else {
                ExprKind::TempVar(callee_name)
            };
            let span = Span::new(start, lexer.pos());
            return Some(Expr {
                kind: ExprKind::DynCall {
                    callee: Box::new(Expr { kind: callee_kind, span: callee_span }),
                    args,
                },
                span,
            });
        }

        lexer.pos = saved;
        return None;
    }

    if let TokenKind::Ident(ref name) = tok.kind {
        let name = name.clone();
        lexer.next_token(); // consume ident

        let next = lexer.peek_token();

        if matches!(next.kind, TokenKind::Colon) {
            lexer.next_token(); // consume `:`

            // `(macro: type _param, ...)[body]` — Harlowe's custom macro definition.
            // The syntax is `type-name _param-name` pairs (no comma between type and name)
            // with commas separating different parameters. Normal expression parsing fails
            // because `parse_expr` consumes the type ident and stops before `_param`.
            // Use a specialized byte scanner that reliably collects `_varname` tokens and
            // then captures the `[body]` hook.
            if name == "macro" {
                let (param_names, hook_source) = lexer.extract_macro_definition();
                let params: Vec<Expr> = param_names
                    .into_iter()
                    .map(|n| Expr {
                        kind: ExprKind::TempVar(n),
                        span: Span::empty(lexer.pos()),
                    })
                    .collect();
                let span = Span::new(start, lexer.pos());
                let kind = if let Some(hook_source) = hook_source {
                    ExprKind::MacroDef { params, hook_source }
                } else {
                    // No hook body — unusual, but produce a placeholder Call.
                    ExprKind::Call { name, args: params }
                };
                return Some(Expr { kind, span });
            }

            // Check for no-arg call: `(saved-games:)`
            let after_colon = lexer.peek_token();
            let args = if matches!(after_colon.kind, TokenKind::RParen) {
                Vec::new()
            } else {
                // Parse comma-separated args until `)`
                parse_args(lexer)
            };

            // Expect closing `)`
            if matches!(lexer.peek_token().kind, TokenKind::RParen) {
                lexer.next_token(); // consume `)`
            }

            let span = Span::new(start, lexer.pos());
            return Some(Expr {
                kind: ExprKind::Call { name, args },
                span,
            });
        }

        // Not a macro call — restore position
        lexer.pos = saved;
        return None;
    }

    lexer.pos = saved;
    None
}


/// Check if a name is a Harlowe color keyword.
fn is_color_name(name: &str) -> bool {
    matches!(
        name,
        "red"
            | "orange"
            | "yellow"
            | "lime"
            | "green"
            | "aqua"
            | "cyan"
            | "blue"
            | "navy"
            | "purple"
            | "magenta"
            | "white"
            | "black"
            | "grey"
            | "gray"
            | "transparent"
    )
}

/// Check if a name is a value-returning macro (no-arg form).
fn is_value_macro(name: &str) -> bool {
    matches!(
        name,
        "it" | "saved-games"
            | "passage"
            | "visits"
            | "turns"
            | "time"
            | "history"
            | "monthday"
            | "current-date"
            | "current-time"
    )
}

/// Parse a color expression, handling `+` composition (e.g., `magenta+white`).
fn parse_color_expr(lexer: &mut ExprLexer, first: String, span: Span) -> Expr {
    let mut color = first;
    // In Harlowe, color+color is written without spaces around +: `magenta+white`
    // But the lexer tokenizes `+` separately. Check if the next token is `+`
    // followed by another color name.
    loop {
        let saved = lexer.pos();
        let tok = lexer.peek_token();
        if matches!(tok.kind, TokenKind::Plus) {
            lexer.next_token(); // consume +
            let next = lexer.peek_token();
            if let TokenKind::Ident(ref name) = next.kind {
                if is_color_name(name) {
                    color.push('+');
                    color.push_str(name);
                    lexer.next_token(); // consume color name
                    continue;
                }
            }
            // Not a color composition, restore
            lexer.pos = saved;
        }
        break;
    }
    Expr {
        kind: ExprKind::ColorLiteral(color),
        span: Span::new(span.start, lexer.pos()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::harlowe::lexer::ExprLexer;

    fn parse(input: &str) -> Expr {
        let mut lexer = ExprLexer::new(input, 0);
        parse_expr(&mut lexer)
    }

    #[test]
    fn test_simple_assign() {
        let expr = parse("$location to \"the plaza\"");
        assert!(matches!(expr.kind, ExprKind::Assign { .. }));
        if let ExprKind::Assign { target, value } = &expr.kind {
            assert!(matches!(target.kind, ExprKind::StoryVar(ref n) if n == "location"));
            assert!(matches!(value.kind, ExprKind::Str(ref s) if s == "the plaza"));
        }
    }

    #[test]
    fn test_comparison() {
        let expr = parse("$event3First is 0");
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Is, .. }));
    }

    #[test]
    fn test_arithmetic() {
        let expr = parse("$x + 1");
        assert!(matches!(expr.kind, ExprKind::Binary { op: BinaryOp::Add, .. }));
    }

    #[test]
    fn test_precedence() {
        // $x + 1 > 5 should parse as ($x + 1) > 5
        let expr = parse("$x + 1 > 5");
        if let ExprKind::Binary { op, left, .. } = &expr.kind {
            assert_eq!(*op, BinaryOp::Gt);
            assert!(matches!(left.kind, ExprKind::Binary { op: BinaryOp::Add, .. }));
        } else {
            panic!("expected binary");
        }
    }

    #[test]
    fn test_not() {
        let expr = parse("not $done");
        assert!(matches!(expr.kind, ExprKind::Unary { op: UnaryOp::Not, .. }));
    }

    #[test]
    fn test_it_plus() {
        let expr = parse("it + 1");
        if let ExprKind::Binary { left, right, op } = &expr.kind {
            assert_eq!(*op, BinaryOp::Add);
            assert!(matches!(left.kind, ExprKind::It));
            assert!(matches!(right.kind, ExprKind::Number(n) if n == 1.0));
        } else {
            panic!("expected binary");
        }
    }

    #[test]
    fn test_or_chain_distributes_is() {
        // `$recover is 0 or ""` → `($recover is 0) or ($recover is "")`
        let expr = parse("$recover is 0 or \"\"");
        if let ExprKind::Binary { op, left, right } = &expr.kind {
            assert_eq!(*op, BinaryOp::Or);
            // Left: $recover is 0
            assert!(matches!(left.kind, ExprKind::Binary { op: BinaryOp::Is, .. }));
            // Right: $recover is "" (distributed)
            if let ExprKind::Binary {
                op: right_op,
                left: right_subject,
                right: right_value,
            } = &right.kind
            {
                assert_eq!(*right_op, BinaryOp::Is);
                assert!(matches!(right_subject.kind, ExprKind::StoryVar(ref n) if n == "recover"));
                assert!(matches!(right_value.kind, ExprKind::Str(ref s) if s.is_empty()));
            } else {
                panic!("expected distributed comparison on right");
            }
        } else {
            panic!("expected binary");
        }
    }

    #[test]
    fn test_or_no_distribution_when_rhs_is_comparison() {
        // `$x is 0 or $y is 1` → no distribution (RHS already has `is`)
        let expr = parse("$x is 0 or $y is 1");
        if let ExprKind::Binary { op, right, .. } = &expr.kind {
            assert_eq!(*op, BinaryOp::Or);
            // Right should be `$y is 1`, not `$x is ($y is 1)`
            if let ExprKind::Binary {
                op: right_op,
                left: right_subject,
                ..
            } = &right.kind
            {
                assert_eq!(*right_op, BinaryOp::Is);
                assert!(matches!(right_subject.kind, ExprKind::StoryVar(ref n) if n == "y"));
            } else {
                panic!("expected comparison on right");
            }
        } else {
            panic!("expected binary");
        }
    }

    #[test]
    fn test_color_literal() {
        let expr = parse("green");
        assert!(matches!(expr.kind, ExprKind::ColorLiteral(ref s) if s == "green"));
    }

    #[test]
    fn test_color_composition() {
        let expr = parse("magenta+white");
        assert!(matches!(expr.kind, ExprKind::ColorLiteral(ref s) if s == "magenta+white"));
    }

    #[test]
    fn test_time_literal() {
        let expr = parse("2s");
        assert!(matches!(expr.kind, ExprKind::TimeLiteral(t) if t == 2.0));
    }

    #[test]
    fn test_negative_number() {
        let expr = parse("-5");
        assert!(matches!(expr.kind, ExprKind::Unary { op: UnaryOp::Neg, .. }));
    }

    #[test]
    fn test_nth_last_ordinal() {
        let expr = parse("$arr's 2ndlast");
        if let ExprKind::Possessive { property, .. } = &expr.kind {
            assert!(matches!(property.kind, ExprKind::Ordinal(Ordinal::NthLast(2))));
        } else {
            panic!("expected Possessive, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_range_ordinal() {
        // `1stto4th of $arr`
        let expr = parse("1stto4th of $arr");
        if let ExprKind::Of { property, .. } = &expr.kind {
            if let ExprKind::Ordinal(Ordinal::Range { from, to }) = &property.kind {
                assert_eq!(*from, RangeEnd::Nth(1));
                assert_eq!(*to, RangeEnd::Nth(4));
            } else {
                panic!("expected Range ordinal");
            }
        } else {
            panic!("expected Of, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_range_to_last_ordinal() {
        let expr = parse("2ndto2ndlast of $arr");
        if let ExprKind::Of { property, .. } = &expr.kind {
            if let ExprKind::Ordinal(Ordinal::Range { from, to }) = &property.kind {
                assert_eq!(*from, RangeEnd::Nth(2));
                assert_eq!(*to, RangeEnd::NthLast(2));
            } else {
                panic!("expected Range ordinal");
            }
        } else {
            panic!("expected Of, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_range_last_to_last() {
        let expr = parse("lasttolast of $arr");
        if let ExprKind::Of { property, .. } = &expr.kind {
            if let ExprKind::Ordinal(Ordinal::Range { from, to }) = &property.kind {
                assert_eq!(*from, RangeEnd::Last);
                assert_eq!(*to, RangeEnd::Last);
            } else {
                panic!("expected Range ordinal");
            }
        } else {
            panic!("expected Of, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_possessive() {
        // `$arr's 1st` — possessive access
        let expr = parse("$arr's 1st");
        if let ExprKind::Possessive { object, property } = &expr.kind {
            assert!(matches!(object.kind, ExprKind::StoryVar(ref n) if n == "arr"));
            assert!(matches!(property.kind, ExprKind::Ordinal(Ordinal::Nth(1))));
        } else {
            panic!("expected Possessive, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_single_quoted_string() {
        // `'hello'` — single-quoted string is valid in Harlowe
        let expr = parse("'hello'");
        assert!(matches!(expr.kind, ExprKind::Str(ref s) if s == "hello"));
    }

    #[test]
    fn test_possessive_not_triggered_by_string_s() {
        // `'stop'` — `'s` not followed by whitespace, so it's a string
        let expr = parse("'stop'");
        assert!(matches!(expr.kind, ExprKind::Str(ref s) if s == "stop"));
    }

    #[test]
    fn test_of_reverse_possessive() {
        // `1st of $arr` — reverse possessive
        let expr = parse("1st of $arr");
        if let ExprKind::Of { property, object } = &expr.kind {
            assert!(matches!(property.kind, ExprKind::Ordinal(Ordinal::Nth(1))));
            assert!(matches!(object.kind, ExprKind::StoryVar(ref n) if n == "arr"));
        } else {
            panic!("expected Of, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_via_lambda() {
        // `via _x + 1` — ViaLambda wrapping the body expression
        let expr = parse("via _x + 1");
        assert!(
            matches!(expr.kind, ExprKind::ViaLambda(_)),
            "expected ViaLambda, got {:?}",
            expr.kind
        );
    }

    #[test]
    fn test_fold_lambda() {
        // `each _item making _acc via _item + _acc` — FoldLambda
        let expr = parse("each _item making _acc via _item + _acc");
        if let ExprKind::FoldLambda { item_var, acc_var, body } = &expr.kind {
            assert_eq!(item_var, "item");
            assert_eq!(acc_var, "acc");
            assert!(matches!(body.kind, ExprKind::Binary { op: BinaryOp::Add, .. }));
        } else {
            panic!("expected FoldLambda, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_each_without_making_is_lambda() {
        // `each _x` alone — plain Lambda (not FoldLambda)
        let expr = parse("each _x");
        assert!(
            matches!(expr.kind, ExprKind::Lambda { .. }),
            "expected Lambda, got {:?}",
            expr.kind
        );
    }

    #[test]
    fn test_possessive_computed_macro_property() {
        // `$arr's (random: 1, 5)` — possessive with macro call as property
        let expr = parse("$arr's (random: 1, 5)");
        if let ExprKind::Possessive { object, property } = &expr.kind {
            assert!(matches!(object.kind, ExprKind::StoryVar(ref n) if n == "arr"));
            if let ExprKind::Call { name, args } = &property.kind {
                assert_eq!(name, "random");
                assert_eq!(args.len(), 2);
            } else {
                panic!("expected Call property, got {:?}", property.kind);
            }
        } else {
            panic!("expected Possessive, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_inline_macro_call_in_expr() {
        // `(random: 1, 5)` as a standalone expression (value position)
        let expr = parse("(random: 1, 5)");
        if let ExprKind::Call { name, args } = &expr.kind {
            assert_eq!(name, "random");
            assert_eq!(args.len(), 2);
        } else {
            panic!("expected Call, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_macro_def_captures_hook_body() {
        // `(macro: dm-type _x)[body text]` — should produce MacroDef with hook_source.
        // The `extract_macro_definition` scanner collects only `_varname` params (not
        // type annotations), so params contains just TempVar("x").
        let expr = parse("(macro: dm-type _x)[body text]");
        if let ExprKind::MacroDef { params, hook_source } = &expr.kind {
            assert_eq!(params.len(), 1, "expected 1 param (the TempVar), got {params:?}");
            assert!(matches!(&params[0].kind, ExprKind::TempVar(n) if n == "x"));
            assert_eq!(hook_source, "body text");
        } else {
            panic!("expected MacroDef, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_macro_def_multi_param() {
        // `(macro: dm-type _a, str-type _b)[body]` — two params
        let expr = parse("(macro: dm-type _a, str-type _b)[body]");
        if let ExprKind::MacroDef { params, hook_source } = &expr.kind {
            assert_eq!(params.len(), 2, "expected 2 params, got {params:?}");
            assert!(matches!(&params[0].kind, ExprKind::TempVar(n) if n == "a"));
            assert!(matches!(&params[1].kind, ExprKind::TempVar(n) if n == "b"));
            assert_eq!(hook_source, "body");
        } else {
            panic!("expected MacroDef, got {:?}", expr.kind);
        }
    }

    #[test]
    fn test_macro_def_in_assign() {
        // `$fn to (macro: dm-type _x)[body]` — RHS should be MacroDef
        let expr = parse("$fn to (macro: dm-type _x)[body]");
        if let ExprKind::Assign { value, .. } = &expr.kind {
            assert!(
                matches!(value.kind, ExprKind::MacroDef { .. }),
                "expected MacroDef on RHS, got {:?}",
                value.kind
            );
        } else {
            panic!("expected Assign, got {:?}", expr.kind);
        }
    }
}
