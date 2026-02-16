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
            TokenKind::Lt if min_prec <= Prec::Compare => (Some(BinaryOp::Lt), Prec::Compare),
            TokenKind::Gt if min_prec <= Prec::Compare => (Some(BinaryOp::Gt), Prec::Compare),
            TokenKind::Lte if min_prec <= Prec::Compare => (Some(BinaryOp::Lte), Prec::Compare),
            TokenKind::Gte if min_prec <= Prec::Compare => (Some(BinaryOp::Gte), Prec::Compare),
            TokenKind::Plus if min_prec <= Prec::Add => (Some(BinaryOp::Add), Prec::Add),
            TokenKind::Minus if min_prec <= Prec::Add => (Some(BinaryOp::Sub), Prec::Add),
            TokenKind::Star if min_prec <= Prec::Mul => (Some(BinaryOp::Mul), Prec::Mul),
            TokenKind::Slash if min_prec <= Prec::Mul => (Some(BinaryOp::Div), Prec::Mul),
            TokenKind::Percent if min_prec <= Prec::Mul => (Some(BinaryOp::Mod), Prec::Mul),
            // Possessive: check for `'s` after expression
            TokenKind::Ident(s) if s == "s" && min_prec <= Prec::Access => {
                // This is the `'s` possessive — but we need to verify the
                // previous character was `'`. We handle this specially.
                break;
            }
            _ => break,
        };

        lexer.next_token(); // consume the operator

        match op {
            Some(bin_op) => {
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
                // `to` — this is an assignment (used in `(set:)`)
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

    // Check for `'s` possessive access
    left = try_parse_possessive(lexer, left);

    left
}

/// Try to parse `'s property` after an expression.
fn try_parse_possessive(lexer: &mut ExprLexer, mut expr: Expr) -> Expr {
    // In Harlowe, `$arr's 1st` or `$obj's name`.
    // The lexer doesn't produce Apostrophe tokens for `'s` because `'` is also
    // a string quote. We handle this by checking the raw input.
    // For now, detect the pattern at the parser level by looking for `'` + `s` + space.
    loop {
        let saved_pos = lexer.pos();
        let tok = lexer.peek_token();

        // Check for possessive: the raw input at current position should be `'s`
        // This is tricky because the lexer treats `'` as string start.
        // We need a heuristic: if we see an error token that tried to lex `'s `,
        // or if we peek at raw input.
        if matches!(tok.kind, TokenKind::Error(_)) {
            // Could be `'s` — check raw input
            break;
        }

        // Check for `of` keyword for reverse possessive: `1st of $arr`
        if matches!(tok.kind, TokenKind::Ident(ref s) if s == "of") {
            lexer.next_token(); // consume `of`
            let object = parse_prec(lexer, Prec::Access);
            let span = Span::new(expr.span.start, object.span.end);
            expr = Expr {
                kind: ExprKind::Of {
                    property: Box::new(expr),
                    object: Box::new(object),
                },
                span,
            };
            continue;
        }

        _ = saved_pos;
        break;
    }

    expr
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
                    | BinaryOp::IsIn,
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
        TokenKind::Number(n) => {
            lexer.next_token();
            Expr {
                kind: ExprKind::Number(n),
                span: tok.span,
            }
        }
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
        TokenKind::Ident(ref s) => {
            let s = s.clone();
            lexer.next_token();
            // Check for ordinal
            if let Some(ord) = parse_ordinal(&s) {
                return Expr {
                    kind: ExprKind::Ordinal(ord),
                    span: tok.span,
                };
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

    if let TokenKind::Ident(ref name) = tok.kind {
        let name = name.clone();
        lexer.next_token(); // consume ident

        let next = lexer.peek_token();

        if matches!(next.kind, TokenKind::Colon) {
            lexer.next_token(); // consume `:`

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

/// Parse an ordinal string like `1st`, `2nd`, `3rd`, `4th`, `last`.
fn parse_ordinal(s: &str) -> Option<Ordinal> {
    if s == "last" {
        return Some(Ordinal::Last);
    }
    if s == "length" {
        return Some(Ordinal::Length);
    }
    // Try to parse `Nth` + suffix
    if s.len() >= 3 {
        let (num_part, suffix) = s.split_at(s.len() - 2);
        if matches!(suffix, "st" | "nd" | "rd" | "th") {
            if let Ok(n) = num_part.parse::<u32>() {
                if n > 0 {
                    return Some(Ordinal::Nth(n));
                }
            }
        }
    }
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
        let expr = parse("$recovery to \"Floor 1 entryway\"");
        assert!(matches!(expr.kind, ExprKind::Assign { .. }));
        if let ExprKind::Assign { target, value } = &expr.kind {
            assert!(matches!(target.kind, ExprKind::StoryVar(ref n) if n == "recovery"));
            assert!(matches!(value.kind, ExprKind::Str(ref s) if s == "Floor 1 entryway"));
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
}
