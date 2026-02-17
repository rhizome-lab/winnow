//! Pratt parser for TwineScript expressions.
//!
//! TwineScript is a JavaScript superset with keyword operators (`is`, `isnot`,
//! `to`, `not`, `and`, `or`, etc.) and SugarCube extensions (`def`, `ndef`,
//! `clone`). This module parses tokenized input from `ExprLexer` into `Expr`
//! AST nodes.
//!
//! Precedence levels (low to high):
//!  1. Comma (for `<<set>>` multi-assignment)
//!  2. Assignment (`=`, `to`, `+=`, etc.)
//!  3. Ternary (`? :`)
//!  4. Nullish coalescing (`??`)
//!  5. Logical OR (`||`, `or`)
//!  6. Logical AND (`&&`, `and`)
//!  7. Bitwise OR (`|`)
//!  8. Bitwise XOR (`^`)
//!  9. Bitwise AND (`&`)
//! 10. Equality (`==`, `!=`, `===`, `!==`, `is`, `isnot`, `eq`, `neq`)
//! 11. Relational (`<`, `<=`, `>`, `>=`, `lt`, `lte`, `gt`, `gte`, `in`, `instanceof`)
//! 12. Shift (`<<`, `>>`, `>>>`)
//! 13. Additive (`+`, `-`)
//! 14. Multiplicative (`*`, `/`, `%`)
//! 15. Exponentiation (`**`) — right-associative
//! 16. Unary prefix (`!`, `-`, `+`, `~`, `++`, `--`, `not`, `typeof`, `delete`, `clone`, `def`, `ndef`, `void`)
//! 17. Postfix (`++`, `--`)
//! 18. Call / Member access (`.`, `[]`, `()`)

use super::ast::*;
use super::lexer::{ExprLexer, Token, TokenKind};

/// Binding powers for Pratt parser (left and right).
/// Higher values bind tighter.
#[derive(Debug, Clone, Copy)]
struct Bp(u8, u8);

/// Parse a full expression from the lexer (consumes until EOF).
pub fn parse_expr(lexer: &mut ExprLexer<'_>) -> Expr {
    parse_bp(lexer, 0)
}

/// Parse a comma-separated list of assignment expressions (for `<<set>>`).
/// Each element is an assignment expression, not a full comma expression.
pub fn parse_assignment_list(lexer: &mut ExprLexer<'_>) -> Vec<Expr> {
    let mut exprs = vec![parse_bp(lexer, BP_ASSIGN.0)];
    while lexer.peek().kind == TokenKind::Comma {
        lexer.next_tok();
        exprs.push(parse_bp(lexer, BP_ASSIGN.0));
    }
    exprs
}

/// Parse a single expression (no commas at top level).
pub fn parse_single_expr(lexer: &mut ExprLexer<'_>) -> Expr {
    parse_bp(lexer, BP_ASSIGN.0)
}

// ── Binding power constants ─────────────────────────────────────────────

const BP_COMMA: Bp = Bp(1, 2);
const BP_ASSIGN: Bp = Bp(4, 3); // right-associative
const BP_TERNARY: Bp = Bp(5, 6);
const BP_NULLISH: Bp = Bp(7, 8);
const BP_LOR: Bp = Bp(9, 10);
const BP_LAND: Bp = Bp(11, 12);
const BP_BOR: Bp = Bp(13, 14);
const BP_BXOR: Bp = Bp(15, 16);
const BP_BAND: Bp = Bp(17, 18);
const BP_EQ: Bp = Bp(19, 20);
const BP_REL: Bp = Bp(21, 22);
const BP_SHIFT: Bp = Bp(23, 24);
const BP_ADD: Bp = Bp(25, 26);
const BP_MUL: Bp = Bp(27, 28);
const BP_EXP: Bp = Bp(30, 29); // right-associative
const BP_CALL: Bp = Bp(33, 34);

/// Core Pratt parser: parse expression at minimum binding power `min_bp`.
fn parse_bp(lexer: &mut ExprLexer<'_>, min_bp: u8) -> Expr {
    let mut lhs = parse_prefix(lexer);

    loop {
        let tok = lexer.peek();
        if tok.kind == TokenKind::Eof {
            break;
        }

        // Postfix operators
        match &tok.kind {
            TokenKind::PlusPlus | TokenKind::MinusMinus => {
                // Only treat as postfix if on the same "expression unit"
                // (no intervening whitespace check needed for our use case)
                let op = if tok.kind == TokenKind::PlusPlus {
                    UnaryOp::Inc
                } else {
                    UnaryOp::Dec
                };
                let tok = lexer.next_tok();
                let span = Span::new(lhs.span.start, tok.span.end);
                lhs = Expr {
                    kind: ExprKind::Postfix {
                        op,
                        operand: Box::new(lhs),
                    },
                    span,
                };
                continue;
            }
            _ => {}
        }

        // Call / member access (highest precedence)
        match &tok.kind {
            TokenKind::Dot => {
                if BP_CALL.0 < min_bp {
                    break;
                }
                lexer.next_tok();
                let prop_tok = lexer.next_tok();
                let prop = match prop_tok.kind {
                    TokenKind::Ident(s) => s,
                    // SugarCube keywords can appear as property names
                    _ => token_as_ident(&prop_tok),
                };
                let span = Span::new(lhs.span.start, prop_tok.span.end);
                lhs = Expr {
                    kind: ExprKind::Member {
                        object: Box::new(lhs),
                        property: prop,
                    },
                    span,
                };
                continue;
            }
            TokenKind::LBracket => {
                if BP_CALL.0 < min_bp {
                    break;
                }
                lexer.next_tok();
                let index = parse_bp(lexer, 0);
                let end_tok = lexer.next_tok();
                let span = Span::new(lhs.span.start, end_tok.span.end);
                lhs = Expr {
                    kind: ExprKind::Index {
                        object: Box::new(lhs),
                        index: Box::new(index),
                    },
                    span,
                };
                continue;
            }
            TokenKind::LParen => {
                if BP_CALL.0 < min_bp {
                    break;
                }
                lexer.next_tok();
                let args = parse_call_args(lexer);
                let end_tok = lexer.next_tok(); // consume )
                let span = Span::new(lhs.span.start, end_tok.span.end);
                lhs = Expr {
                    kind: ExprKind::Call {
                        callee: Box::new(lhs),
                        args,
                    },
                    span,
                };
                continue;
            }
            _ => {}
        }

        // Infix operators
        if let Some((bp, op)) = infix_bp(&tok.kind) {
            if bp.0 < min_bp {
                break;
            }

            match op {
                InfixOp::Binary(bin_op) => {
                    lexer.next_tok();
                    let rhs = parse_bp(lexer, bp.1);
                    let span = Span::new(lhs.span.start, rhs.span.end);
                    lhs = Expr {
                        kind: ExprKind::Binary {
                            op: bin_op,
                            left: Box::new(lhs),
                            right: Box::new(rhs),
                        },
                        span,
                    };
                }
                InfixOp::Assign(compound) => {
                    lexer.next_tok();
                    let rhs = parse_bp(lexer, bp.1);
                    let span = Span::new(lhs.span.start, rhs.span.end);
                    lhs = Expr {
                        kind: ExprKind::Assign {
                            op: compound,
                            target: Box::new(lhs),
                            value: Box::new(rhs),
                        },
                        span,
                    };
                }
                InfixOp::Ternary => {
                    lexer.next_tok();
                    let then_expr = parse_bp(lexer, 0);
                    // Expect ':'
                    if lexer.peek().kind == TokenKind::Colon {
                        lexer.next_tok();
                    }
                    let else_expr = parse_bp(lexer, bp.1);
                    let span = Span::new(lhs.span.start, else_expr.span.end);
                    lhs = Expr {
                        kind: ExprKind::Ternary {
                            cond: Box::new(lhs),
                            then_expr: Box::new(then_expr),
                            else_expr: Box::new(else_expr),
                        },
                        span,
                    };
                }
                InfixOp::Comma => {
                    lexer.next_tok();
                    let rhs = parse_bp(lexer, bp.1);
                    // Flatten comma chains
                    let mut parts = match lhs.kind {
                        ExprKind::Comma(ref v) => v.clone(),
                        _ => vec![lhs.clone()],
                    };
                    parts.push(rhs.clone());
                    let span = Span::new(lhs.span.start, rhs.span.end);
                    lhs = Expr {
                        kind: ExprKind::Comma(parts),
                        span,
                    };
                }
            }
            continue;
        }

        break;
    }

    lhs
}

enum InfixOp {
    Binary(BinaryOp),
    Assign(Option<CompoundOp>),
    Ternary,
    Comma,
}

fn infix_bp(kind: &TokenKind) -> Option<(Bp, InfixOp)> {
    Some(match kind {
        // Comma
        TokenKind::Comma => (BP_COMMA, InfixOp::Comma),

        // Assignment
        TokenKind::Eq | TokenKind::KwTo => (BP_ASSIGN, InfixOp::Assign(None)),
        TokenKind::PlusEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::Add))),
        TokenKind::MinusEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::Sub))),
        TokenKind::StarEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::Mul))),
        TokenKind::SlashEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::Div))),
        TokenKind::PercentEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::Mod))),
        TokenKind::StarStarEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::Exp))),
        TokenKind::AmpEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::BitAnd))),
        TokenKind::PipeEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::BitOr))),
        TokenKind::CaretEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::BitXor))),
        TokenKind::LessLessEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::Shl))),
        TokenKind::GreaterGreaterEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::Shr))),
        TokenKind::GreaterGreaterGreaterEq => (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::UShr))),
        TokenKind::QuestionQuestionEq => {
            (BP_ASSIGN, InfixOp::Assign(Some(CompoundOp::NullishCoalesce)))
        }

        // Ternary
        TokenKind::Question => (BP_TERNARY, InfixOp::Ternary),

        // Nullish coalescing
        TokenKind::QuestionQuestion => (BP_NULLISH, InfixOp::Binary(BinaryOp::NullishCoalesce)),

        // Logical
        TokenKind::PipePipe | TokenKind::KwOr => (BP_LOR, InfixOp::Binary(BinaryOp::Or)),
        TokenKind::AmpAmp | TokenKind::KwAnd => (BP_LAND, InfixOp::Binary(BinaryOp::And)),

        // Bitwise
        TokenKind::Pipe => (BP_BOR, InfixOp::Binary(BinaryOp::BitOr)),
        TokenKind::Caret => (BP_BXOR, InfixOp::Binary(BinaryOp::BitXor)),
        TokenKind::Amp => (BP_BAND, InfixOp::Binary(BinaryOp::BitAnd)),

        // Equality
        TokenKind::EqEq | TokenKind::KwEq => (BP_EQ, InfixOp::Binary(BinaryOp::Eq)),
        TokenKind::NotEq | TokenKind::KwNeq => (BP_EQ, InfixOp::Binary(BinaryOp::Neq)),
        TokenKind::EqEqEq | TokenKind::KwIs => (BP_EQ, InfixOp::Binary(BinaryOp::StrictEq)),
        TokenKind::NotEqEq | TokenKind::KwIsnot => {
            (BP_EQ, InfixOp::Binary(BinaryOp::StrictNeq))
        }

        // Relational
        TokenKind::Less | TokenKind::KwLt => (BP_REL, InfixOp::Binary(BinaryOp::Lt)),
        TokenKind::LessEq | TokenKind::KwLte => (BP_REL, InfixOp::Binary(BinaryOp::Lte)),
        TokenKind::Greater | TokenKind::KwGt => (BP_REL, InfixOp::Binary(BinaryOp::Gt)),
        TokenKind::GreaterEq | TokenKind::KwGte => (BP_REL, InfixOp::Binary(BinaryOp::Gte)),
        TokenKind::KwIn => (BP_REL, InfixOp::Binary(BinaryOp::In)),
        TokenKind::KwInstanceof => (BP_REL, InfixOp::Binary(BinaryOp::InstanceOf)),

        // Shift
        TokenKind::LessLess => (BP_SHIFT, InfixOp::Binary(BinaryOp::Shl)),
        TokenKind::GreaterGreater => (BP_SHIFT, InfixOp::Binary(BinaryOp::Shr)),
        TokenKind::GreaterGreaterGreater => (BP_SHIFT, InfixOp::Binary(BinaryOp::UShr)),

        // Additive
        TokenKind::Plus => (BP_ADD, InfixOp::Binary(BinaryOp::Add)),
        TokenKind::Minus => (BP_ADD, InfixOp::Binary(BinaryOp::Sub)),

        // Multiplicative
        TokenKind::Star => (BP_MUL, InfixOp::Binary(BinaryOp::Mul)),
        TokenKind::Slash => (BP_MUL, InfixOp::Binary(BinaryOp::Div)),
        TokenKind::Percent => (BP_MUL, InfixOp::Binary(BinaryOp::Mod)),

        // Exponentiation (right-associative)
        TokenKind::StarStar => (BP_EXP, InfixOp::Binary(BinaryOp::Exp)),

        _ => return None,
    })
}

/// Parse a prefix expression (unary operators, literals, variables, etc.).
fn parse_prefix(lexer: &mut ExprLexer<'_>) -> Expr {
    let tok = lexer.next_tok();
    let start = tok.span.start;

    match tok.kind {
        // Literals
        TokenKind::Number(v) => Expr {
            kind: ExprKind::Literal(Literal::Number(NumberLit::new(v))),
            span: tok.span,
        },
        TokenKind::Str(s) => Expr {
            kind: ExprKind::Str(s),
            span: tok.span,
        },
        TokenKind::KwTrue => Expr {
            kind: ExprKind::Literal(Literal::Bool(true)),
            span: tok.span,
        },
        TokenKind::KwFalse => Expr {
            kind: ExprKind::Literal(Literal::Bool(false)),
            span: tok.span,
        },
        TokenKind::KwNull => Expr {
            kind: ExprKind::Literal(Literal::Null),
            span: tok.span,
        },
        TokenKind::KwUndefined => Expr {
            kind: ExprKind::Literal(Literal::Undefined),
            span: tok.span,
        },

        // Variables
        TokenKind::StoryVar(name) => Expr {
            kind: ExprKind::StoryVar(name),
            span: tok.span,
        },
        TokenKind::TempVar(name) => Expr {
            kind: ExprKind::TempVar(name),
            span: tok.span,
        },
        TokenKind::Ident(name) => {
            // Un-parenthesized single-param arrow: `x => expr`
            if lexer.peek().kind == TokenKind::Arrow {
                lexer.next_tok(); // consume =>
                let body = parse_bp(lexer, BP_ASSIGN.0);
                let span = Span::new(start, body.span.end);
                Expr {
                    kind: ExprKind::Arrow {
                        params: vec![name],
                        body: Box::new(body),
                    },
                    span,
                }
            } else {
                Expr {
                    kind: ExprKind::Ident(name),
                    span: tok.span,
                }
            }
        }

        // Parenthesized expression or arrow function
        TokenKind::LParen => {
            // Try to detect arrow function: (params) => ...
            // For simplicity, parse as grouped expression first,
            // then check for arrow
            let inner = parse_bp(lexer, 0);
            let rparen = lexer.next_tok(); // consume )
            if lexer.peek().kind == TokenKind::Arrow {
                lexer.next_tok(); // consume =>
                let params = extract_arrow_params(&inner);
                let body = parse_bp(lexer, BP_ASSIGN.0);
                let span = Span::new(start, body.span.end);
                Expr {
                    kind: ExprKind::Arrow {
                        params,
                        body: Box::new(body),
                    },
                    span,
                }
            } else {
                let span = Span::new(start, rparen.span.end);
                Expr {
                    kind: ExprKind::Paren(Box::new(inner)),
                    span,
                }
            }
        }

        // Array literal
        TokenKind::LBracket => {
            let mut elements = Vec::new();
            if lexer.peek().kind != TokenKind::RBracket {
                elements.push(parse_bp(lexer, BP_ASSIGN.0));
                while lexer.peek().kind == TokenKind::Comma {
                    lexer.next_tok();
                    if lexer.peek().kind == TokenKind::RBracket {
                        break; // trailing comma
                    }
                    elements.push(parse_bp(lexer, BP_ASSIGN.0));
                }
            }
            let end = lexer.next_tok(); // consume ]
            Expr {
                kind: ExprKind::Array(elements),
                span: Span::new(start, end.span.end),
            }
        }

        // Object literal
        TokenKind::LBrace => {
            let mut pairs = Vec::new();
            if lexer.peek().kind != TokenKind::RBrace {
                pairs.push(parse_object_entry(lexer));
                while lexer.peek().kind == TokenKind::Comma {
                    lexer.next_tok();
                    if lexer.peek().kind == TokenKind::RBrace {
                        break; // trailing comma
                    }
                    pairs.push(parse_object_entry(lexer));
                }
            }
            let end = lexer.next_tok(); // consume }
            Expr {
                kind: ExprKind::Object(pairs),
                span: Span::new(start, end.span.end),
            }
        }

        // Template literal
        TokenKind::TemplateFull(s) => Expr {
            kind: ExprKind::Template {
                parts: vec![TemplatePart::Str(s)],
            },
            span: tok.span,
        },
        TokenKind::TemplateStart(s) => {
            let mut parts = Vec::new();
            if !s.is_empty() {
                parts.push(TemplatePart::Str(s));
            }
            loop {
                // Parse interpolated expression
                let expr = parse_bp(lexer, 0);
                parts.push(TemplatePart::Expr(expr));
                // Next should be TemplateMiddle or TemplateEnd
                let cont = lexer.next_tok();
                match cont.kind {
                    TokenKind::TemplateMiddle(s) => {
                        if !s.is_empty() {
                            parts.push(TemplatePart::Str(s));
                        }
                    }
                    TokenKind::TemplateEnd(s) => {
                        if !s.is_empty() {
                            parts.push(TemplatePart::Str(s));
                        }
                        break;
                    }
                    _ => break, // malformed
                }
            }
            let end_pos = lexer.position();
            Expr {
                kind: ExprKind::Template { parts },
                span: Span::new(start, end_pos),
            }
        }

        // Unary prefix operators
        TokenKind::Bang | TokenKind::KwNot => {
            let operand = parse_bp(lexer, 31); // unary prefix bp
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Unary {
                    op: UnaryOp::Not,
                    operand: Box::new(operand),
                },
                span,
            }
        }
        TokenKind::Minus => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Unary {
                    op: UnaryOp::Neg,
                    operand: Box::new(operand),
                },
                span,
            }
        }
        TokenKind::Plus => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Unary {
                    op: UnaryOp::Pos,
                    operand: Box::new(operand),
                },
                span,
            }
        }
        TokenKind::Tilde => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Unary {
                    op: UnaryOp::BitNot,
                    operand: Box::new(operand),
                },
                span,
            }
        }
        TokenKind::PlusPlus => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Unary {
                    op: UnaryOp::Inc,
                    operand: Box::new(operand),
                },
                span,
            }
        }
        TokenKind::MinusMinus => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Unary {
                    op: UnaryOp::Dec,
                    operand: Box::new(operand),
                },
                span,
            }
        }
        TokenKind::KwTypeof => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::TypeOf(Box::new(operand)),
                span,
            }
        }
        TokenKind::KwDelete => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Delete(Box::new(operand)),
                span,
            }
        }
        TokenKind::KwVoid => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            // void expr → just parse and discard to undefined
            Expr {
                kind: ExprKind::Unary {
                    op: UnaryOp::Pos, // void treated as prefix for AST
                    operand: Box::new(operand),
                },
                span,
            }
        }
        TokenKind::KwClone => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Clone(Box::new(operand)),
                span,
            }
        }
        TokenKind::KwDef => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Def(Box::new(operand)),
                span,
            }
        }
        TokenKind::KwNdef => {
            let operand = parse_bp(lexer, 31);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Ndef(Box::new(operand)),
                span,
            }
        }
        TokenKind::KwNew => {
            // new Constructor(args)
            let callee = parse_bp(lexer, BP_CALL.0);
            // Extract args if present (they may have been parsed as a Call)
            match callee.kind {
                ExprKind::Call {
                    callee: inner,
                    args,
                } => Expr {
                    kind: ExprKind::New {
                        callee: inner,
                        args,
                    },
                    span: Span::new(start, callee.span.end),
                },
                _ => Expr {
                    kind: ExprKind::New {
                        callee: Box::new(callee.clone()),
                        args: Vec::new(),
                    },
                    span: Span::new(start, callee.span.end),
                },
            }
        }

        // Spread operator
        TokenKind::DotDotDot => {
            let operand = parse_bp(lexer, BP_ASSIGN.0);
            let span = Span::new(start, operand.span.end);
            Expr {
                kind: ExprKind::Spread(Box::new(operand)),
                span,
            }
        }

        // EOF or unexpected token
        TokenKind::Eof => Expr {
            kind: ExprKind::Error(format!(
                "unexpected end of expression in `{}`",
                lexer.source()
            )),
            span: tok.span,
        },
        _ => {
            let src = lexer.source();
            let context: &str = if src.len() <= 80 {
                src
            } else {
                &src[..80]
            };
            Expr {
                kind: ExprKind::Error(format!(
                    "unexpected `{:?}` in `{}`",
                    tok.kind, context
                )),
                span: tok.span,
            }
        }
    }
}

fn parse_call_args(lexer: &mut ExprLexer<'_>) -> Vec<Expr> {
    let mut args = Vec::new();
    if lexer.peek().kind == TokenKind::RParen {
        return args;
    }
    args.push(parse_bp(lexer, BP_ASSIGN.0));
    while lexer.peek().kind == TokenKind::Comma {
        lexer.next_tok();
        args.push(parse_bp(lexer, BP_ASSIGN.0));
    }
    args
}

fn parse_object_entry(lexer: &mut ExprLexer<'_>) -> (Expr, Expr) {
    // Spread in object literal: { ...expr }
    if lexer.peek().kind == TokenKind::DotDotDot {
        let tok = lexer.next_tok();
        let inner = parse_bp(lexer, BP_ASSIGN.0);
        let span = Span::new(tok.span.start, inner.span.end);
        let spread_key = Expr {
            kind: ExprKind::Str("...".into()),
            span: Span::new(tok.span.start, tok.span.end),
        };
        let spread_val = Expr {
            kind: inner.kind,
            span,
        };
        return (spread_key, spread_val);
    }

    let key = match &lexer.peek().kind {
        TokenKind::Str(_) | TokenKind::Number(_) => parse_prefix(lexer),
        TokenKind::LBracket => {
            // Computed property: [expr]: value
            lexer.next_tok();
            let key = parse_bp(lexer, 0);
            lexer.next_tok(); // consume ]
            key
        }
        _ => {
            // Identifier key (or keyword used as key)
            let tok = lexer.next_tok();
            let name = match tok.kind {
                TokenKind::Ident(s) => s,
                _ => token_as_ident(&tok),
            };
            Expr {
                kind: ExprKind::Ident(name),
                span: tok.span,
            }
        }
    };
    // Expect ':'
    if lexer.peek().kind == TokenKind::Colon {
        lexer.next_tok();
    }
    let value = parse_bp(lexer, BP_ASSIGN.0);
    (key, value)
}

/// Extract parameter names from a parsed expression for arrow functions.
fn extract_arrow_params(expr: &Expr) -> Vec<String> {
    match &expr.kind {
        ExprKind::Ident(s) => vec![s.clone()],
        ExprKind::Comma(parts) => parts.iter().flat_map(extract_arrow_params).collect(),
        _ => Vec::new(),
    }
}

/// Convert a token to an identifier string (for keywords used as property names).
fn token_as_ident(tok: &Token) -> String {
    match &tok.kind {
        TokenKind::Ident(s) => s.clone(),
        TokenKind::KwIs => "is".into(),
        TokenKind::KwIsnot => "isnot".into(),
        TokenKind::KwTo => "to".into(),
        TokenKind::KwNot => "not".into(),
        TokenKind::KwAnd => "and".into(),
        TokenKind::KwOr => "or".into(),
        TokenKind::KwEq => "eq".into(),
        TokenKind::KwNeq => "neq".into(),
        TokenKind::KwLt => "lt".into(),
        TokenKind::KwLte => "lte".into(),
        TokenKind::KwGt => "gt".into(),
        TokenKind::KwGte => "gte".into(),
        TokenKind::KwDef => "def".into(),
        TokenKind::KwNdef => "ndef".into(),
        TokenKind::KwTrue => "true".into(),
        TokenKind::KwFalse => "false".into(),
        TokenKind::KwNull => "null".into(),
        TokenKind::KwUndefined => "undefined".into(),
        TokenKind::KwNew => "new".into(),
        TokenKind::KwDelete => "delete".into(),
        TokenKind::KwTypeof => "typeof".into(),
        TokenKind::KwInstanceof => "instanceof".into(),
        TokenKind::KwIn => "in".into(),
        TokenKind::KwClone => "clone".into(),
        TokenKind::KwVoid => "void".into(),
        _ => format!("{:?}", tok.kind),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(src: &str) -> Expr {
        let mut lexer = ExprLexer::new(src, 0);
        parse_expr(&mut lexer)
    }

    fn parse_assign_list(src: &str) -> Vec<Expr> {
        let mut lexer = ExprLexer::new(src, 0);
        parse_assignment_list(&mut lexer)
    }

    // ── Literals ────────────────────────────────────────────────────

    #[test]
    fn number_literal() {
        let e = parse("42");
        assert!(matches!(e.kind, ExprKind::Literal(Literal::Number(n)) if n.value() == 42.0));
    }

    #[test]
    fn string_literal() {
        let e = parse(r#""hello""#);
        assert!(matches!(e.kind, ExprKind::Str(ref s) if s == "hello"));
    }

    #[test]
    fn bool_literal() {
        let e = parse("true");
        assert!(matches!(e.kind, ExprKind::Literal(Literal::Bool(true))));
    }

    #[test]
    fn null_literal() {
        let e = parse("null");
        assert!(matches!(e.kind, ExprKind::Literal(Literal::Null)));
    }

    #[test]
    fn undefined_literal() {
        let e = parse("undefined");
        assert!(matches!(e.kind, ExprKind::Literal(Literal::Undefined)));
    }

    // ── Variables ───────────────────────────────────────────────────

    #[test]
    fn story_variable() {
        let e = parse("$foo");
        assert!(matches!(e.kind, ExprKind::StoryVar(ref s) if s == "foo"));
    }

    #[test]
    fn temp_variable() {
        let e = parse("_bar");
        assert!(matches!(e.kind, ExprKind::TempVar(ref s) if s == "bar"));
    }

    #[test]
    fn identifier() {
        let e = parse("Math");
        assert!(matches!(e.kind, ExprKind::Ident(ref s) if s == "Math"));
    }

    // ── Binary operators ────────────────────────────────────────────

    #[test]
    fn addition() {
        let e = parse("1 + 2");
        assert!(matches!(
            e.kind,
            ExprKind::Binary {
                op: BinaryOp::Add,
                ..
            }
        ));
    }

    #[test]
    fn precedence_mul_add() {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        let e = parse("1 + 2 * 3");
        if let ExprKind::Binary { op, right, .. } = &e.kind {
            assert_eq!(*op, BinaryOp::Add);
            assert!(matches!(
                right.kind,
                ExprKind::Binary {
                    op: BinaryOp::Mul,
                    ..
                }
            ));
        } else {
            panic!("expected binary");
        }
    }

    #[test]
    fn sugarcube_is() {
        let e = parse("$x is 5");
        assert!(matches!(
            e.kind,
            ExprKind::Binary {
                op: BinaryOp::StrictEq,
                ..
            }
        ));
    }

    #[test]
    fn sugarcube_isnot() {
        let e = parse("$x isnot undefined");
        assert!(matches!(
            e.kind,
            ExprKind::Binary {
                op: BinaryOp::StrictNeq,
                ..
            }
        ));
    }

    #[test]
    fn sugarcube_gte() {
        let e = parse("$x gte 10");
        assert!(matches!(
            e.kind,
            ExprKind::Binary {
                op: BinaryOp::Gte,
                ..
            }
        ));
    }

    #[test]
    fn logical_and_or() {
        let e = parse("$a and $b or $c");
        // Should parse as ($a and $b) or $c
        if let ExprKind::Binary { op, left, .. } = &e.kind {
            assert_eq!(*op, BinaryOp::Or);
            assert!(matches!(
                left.kind,
                ExprKind::Binary {
                    op: BinaryOp::And,
                    ..
                }
            ));
        } else {
            panic!("expected binary");
        }
    }

    // ── Assignment ──────────────────────────────────────────────────

    #[test]
    fn assign_to() {
        let e = parse("$x to 5");
        assert!(matches!(
            e.kind,
            ExprKind::Assign {
                op: None,
                ..
            }
        ));
    }

    #[test]
    fn assign_eq() {
        let e = parse("$x = 5");
        assert!(matches!(
            e.kind,
            ExprKind::Assign {
                op: None,
                ..
            }
        ));
    }

    #[test]
    fn compound_assign() {
        let e = parse("$x += 1");
        assert!(matches!(
            e.kind,
            ExprKind::Assign {
                op: Some(CompoundOp::Add),
                ..
            }
        ));
    }

    #[test]
    fn assignment_list() {
        let list = parse_assign_list("$x to 1, $y to 2, $z to 3");
        assert_eq!(list.len(), 3);
        for item in &list {
            assert!(matches!(item.kind, ExprKind::Assign { .. }));
        }
    }

    // ── Unary ───────────────────────────────────────────────────────

    #[test]
    fn unary_not() {
        let e = parse("!$x");
        assert!(matches!(
            e.kind,
            ExprKind::Unary {
                op: UnaryOp::Not,
                ..
            }
        ));
    }

    #[test]
    fn keyword_not() {
        let e = parse("not $x");
        assert!(matches!(
            e.kind,
            ExprKind::Unary {
                op: UnaryOp::Not,
                ..
            }
        ));
    }

    #[test]
    fn unary_neg() {
        let e = parse("-5");
        assert!(matches!(
            e.kind,
            ExprKind::Unary {
                op: UnaryOp::Neg,
                ..
            }
        ));
    }

    #[test]
    fn prefix_increment() {
        let e = parse("++$x");
        assert!(matches!(
            e.kind,
            ExprKind::Unary {
                op: UnaryOp::Inc,
                ..
            }
        ));
    }

    #[test]
    fn postfix_decrement() {
        let e = parse("$x--");
        assert!(matches!(
            e.kind,
            ExprKind::Postfix {
                op: UnaryOp::Dec,
                ..
            }
        ));
    }

    // ── Special SugarCube ───────────────────────────────────────────

    #[test]
    fn def_operator() {
        let e = parse("def $x");
        assert!(matches!(e.kind, ExprKind::Def(_)));
    }

    #[test]
    fn ndef_operator() {
        let e = parse("ndef $x");
        assert!(matches!(e.kind, ExprKind::Ndef(_)));
    }

    #[test]
    fn clone_operator() {
        let e = parse("clone $x");
        assert!(matches!(e.kind, ExprKind::Clone(_)));
    }

    #[test]
    fn typeof_operator() {
        let e = parse("typeof $x");
        assert!(matches!(e.kind, ExprKind::TypeOf(_)));
    }

    #[test]
    fn delete_operator() {
        let e = parse("delete $x.prop");
        assert!(matches!(e.kind, ExprKind::Delete(_)));
    }

    // ── Ternary ─────────────────────────────────────────────────────

    #[test]
    fn ternary() {
        let e = parse("$x ? 1 : 2");
        assert!(matches!(e.kind, ExprKind::Ternary { .. }));
    }

    // ── Member access / call ────────────────────────────────────────

    #[test]
    fn member_access() {
        let e = parse("$obj.prop");
        assert!(matches!(
            e.kind,
            ExprKind::Member {
                ref property,
                ..
            } if property == "prop"
        ));
    }

    #[test]
    fn chained_member() {
        let e = parse("$obj.a.b.c");
        if let ExprKind::Member { object, property } = &e.kind {
            assert_eq!(property, "c");
            assert!(matches!(
                object.kind,
                ExprKind::Member {
                    ref property,
                    ..
                } if property == "b"
            ));
        } else {
            panic!("expected member");
        }
    }

    #[test]
    fn index_access() {
        let e = parse("$arr[0]");
        assert!(matches!(e.kind, ExprKind::Index { .. }));
    }

    #[test]
    fn function_call() {
        let e = parse("Math.max(1, 2)");
        assert!(matches!(e.kind, ExprKind::Call { .. }));
    }

    #[test]
    fn method_call_chain() {
        let e = parse("$arr.includes(5)");
        if let ExprKind::Call { callee, args } = &e.kind {
            assert!(matches!(callee.kind, ExprKind::Member { .. }));
            assert_eq!(args.len(), 1);
        } else {
            panic!("expected call");
        }
    }

    // ── Array / Object literals ─────────────────────────────────────

    #[test]
    fn array_literal() {
        let e = parse("[1, 2, 3]");
        if let ExprKind::Array(elements) = &e.kind {
            assert_eq!(elements.len(), 3);
        } else {
            panic!("expected array");
        }
    }

    #[test]
    fn object_literal() {
        let e = parse("{a: 1, b: 2}");
        if let ExprKind::Object(pairs) = &e.kind {
            assert_eq!(pairs.len(), 2);
        } else {
            panic!("expected object");
        }
    }

    // ── Template literals ───────────────────────────────────────────

    #[test]
    fn template_simple() {
        let e = parse("`hello`");
        assert!(matches!(e.kind, ExprKind::Template { .. }));
    }

    #[test]
    fn template_interpolated() {
        let e = parse("`hello ${$name}!`");
        if let ExprKind::Template { parts } = &e.kind {
            assert_eq!(parts.len(), 3); // "hello ", expr, "!"
        } else {
            panic!("expected template");
        }
    }

    // ── Arrow functions ─────────────────────────────────────────────

    #[test]
    fn arrow_function() {
        let e = parse("(e) => e.name");
        assert!(matches!(e.kind, ExprKind::Arrow { .. }));
        if let ExprKind::Arrow { params, .. } = &e.kind {
            assert_eq!(params, &["e"]);
        }
    }

    #[test]
    fn arrow_function_bare_param() {
        let e = parse("x => x + 1");
        assert!(matches!(e.kind, ExprKind::Arrow { .. }));
        if let ExprKind::Arrow { params, body } = &e.kind {
            assert_eq!(params, &["x"]);
            assert!(matches!(body.kind, ExprKind::Binary { op: BinaryOp::Add, .. }));
        }
    }

    #[test]
    fn arrow_function_bare_in_call() {
        // Common pattern: arr.forEach(x => x.prop)
        let e = parse("arr.forEach(x => x.prop)");
        assert!(matches!(e.kind, ExprKind::Call { .. }));
        if let ExprKind::Call { args, .. } = &e.kind {
            assert_eq!(args.len(), 1);
            assert!(matches!(args[0].kind, ExprKind::Arrow { .. }));
            if let ExprKind::Arrow { params, .. } = &args[0].kind {
                assert_eq!(params, &["x"]);
            }
        }
    }

    #[test]
    fn arrow_multi_param() {
        let e = parse("(x, i) => x + i");
        assert!(matches!(e.kind, ExprKind::Arrow { .. }));
        if let ExprKind::Arrow { params, .. } = &e.kind {
            assert_eq!(params, &["x", "i"]);
        }
    }

    // ── New ─────────────────────────────────────────────────────────

    #[test]
    fn new_expression() {
        let e = parse("new Map()");
        assert!(matches!(e.kind, ExprKind::New { .. }));
    }

    // ── Complex real-world expressions ──────────────────────────────

    #[test]
    fn complex_comparison() {
        // Real DoL pattern: $enemyarousal gte ($enemyarousalmax / 5) * 4
        let e = parse("$enemyarousal gte ($enemyarousalmax / 5) * 4");
        assert!(matches!(
            e.kind,
            ExprKind::Binary {
                op: BinaryOp::Gte,
                ..
            }
        ));
    }

    #[test]
    fn array_method_with_arrow() {
        // Real DoL pattern
        let e = parse("['dol'].some(e => e === $name)");
        assert!(matches!(e.kind, ExprKind::Call { .. }));
    }

    #[test]
    fn chained_index_and_member() {
        let e = parse("$fetus[_i].stats.gender");
        if let ExprKind::Member { property, .. } = &e.kind {
            assert_eq!(property, "gender");
        } else {
            panic!("expected member");
        }
    }

    #[test]
    fn nullish_coalescing() {
        let e = parse("$x ?? 0");
        assert!(matches!(
            e.kind,
            ExprKind::Binary {
                op: BinaryOp::NullishCoalesce,
                ..
            }
        ));
    }

    #[test]
    fn instanceof_operator() {
        let e = parse("$x instanceof Array");
        assert!(matches!(
            e.kind,
            ExprKind::Binary {
                op: BinaryOp::InstanceOf,
                ..
            }
        ));
    }

    #[test]
    fn exponentiation() {
        // Right-associative: 2 ** 3 ** 2 = 2 ** (3 ** 2)
        let e = parse("2 ** 3 ** 2");
        if let ExprKind::Binary { op, right, .. } = &e.kind {
            assert_eq!(*op, BinaryOp::Exp);
            assert!(matches!(
                right.kind,
                ExprKind::Binary {
                    op: BinaryOp::Exp,
                    ..
                }
            ));
        } else {
            panic!("expected binary");
        }
    }
}
