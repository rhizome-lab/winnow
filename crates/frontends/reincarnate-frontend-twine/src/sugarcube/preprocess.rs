//! SugarCube → JavaScript expression preprocessor.
//!
//! TwineScript extends JavaScript with keyword operators (`is`, `isnot`, `to`,
//! `and`, `or`, `not`, `def`, `ndef`, `clone`, `eq`, `neq`, `lt`, `lte`, `gt`,
//! `gte`). This module rewrites those keywords into their JS equivalents so the
//! expression can be parsed by a real JS parser (oxc).
//!
//! `$name` and `_name` are valid JS identifiers — no preprocessing needed.
//! They are detected by prefix in the AST walker.

/// Result of preprocessing a SugarCube expression.
pub struct Preprocessed {
    /// The JavaScript source after keyword replacement.
    pub js: String,
    /// Byte positions in `js` where `def` was replaced with `typeof`.
    pub def_positions: Vec<usize>,
    /// Byte positions in `js` where `ndef` was replaced with `typeof`.
    pub ndef_positions: Vec<usize>,
    /// Byte positions in `js` where `clone` was replaced with `typeof`.
    pub clone_positions: Vec<usize>,
}

/// Copy one UTF-8 character from `src` at byte position `i` into `out`.
/// Returns the byte position after the character.
fn push_char(out: &mut String, src: &str, i: usize) -> usize {
    let bytes = src.as_bytes();
    if bytes[i] < 0x80 {
        out.push(bytes[i] as char);
        i + 1
    } else {
        let mut end = i + 1;
        while end < bytes.len() && bytes[end] & 0xC0 == 0x80 {
            end += 1;
        }
        out.push_str(&src[i..end]);
        end
    }
}

/// Preprocess a SugarCube expression, replacing TwineScript keywords with JS equivalents.
///
/// Skips replacements inside string literals, template literals, and comments.
/// Respects `.` context — `obj.is` should not become `obj.===`.
pub fn preprocess(src: &str) -> Preprocessed {
    let bytes = src.as_bytes();
    let len = bytes.len();
    let mut out = String::with_capacity(len + 32);
    let mut def_positions = Vec::new();
    let mut ndef_positions = Vec::new();
    let mut clone_positions = Vec::new();
    let mut i = 0;

    while i < len {
        let ch = bytes[i];

        // Skip string literals
        if ch == b'"' || ch == b'\'' {
            let quote = ch;
            out.push(ch as char);
            i += 1;
            while i < len {
                let c = bytes[i];
                if c == quote {
                    out.push(c as char);
                    i += 1;
                    break;
                }
                if c == b'\\' && i < len {
                    out.push(c as char);
                    i += 1;
                    if i < len {
                        i = push_char(&mut out, src, i);
                    }
                } else {
                    i = push_char(&mut out, src, i);
                }
            }
            continue;
        }

        // Template literals: skip literal text but preprocess ${...} expressions
        if ch == b'`' {
            out.push(ch as char);
            i += 1;
            while i < len {
                let c = bytes[i];
                if c == b'`' {
                    out.push(c as char);
                    i += 1;
                    break;
                }
                if c == b'\\' {
                    out.push(c as char);
                    i += 1;
                    if i < len {
                        i = push_char(&mut out, src, i);
                    }
                } else if c == b'$' && i + 1 < len && bytes[i + 1] == b'{' {
                    out.push('$');
                    out.push('{');
                    i += 2;
                    // Extract expression between ${ and matching }, preprocess it
                    let expr_start = i;
                    let mut depth = 1u32;
                    while i < len && depth > 0 {
                        match bytes[i] {
                            b'{' => { depth += 1; i += 1; }
                            b'}' => { depth -= 1; if depth > 0 { i += 1; } }
                            b'"' | b'\'' => { i = skip_string_in_preprocess(bytes, i); }
                            b'`' => { i = skip_template_in_preprocess(bytes, i); }
                            _ => { i += 1; }
                        }
                    }
                    let expr_text = &src[expr_start..i];
                    let pp = preprocess(expr_text);
                    out.push_str(&pp.js);
                    // Track def/ndef/clone positions with offset
                    let offset = out.len() - pp.js.len();
                    for p in pp.def_positions { def_positions.push(offset + p); }
                    for p in pp.ndef_positions { ndef_positions.push(offset + p); }
                    for p in pp.clone_positions { clone_positions.push(offset + p); }
                    if i < len && bytes[i] == b'}' {
                        out.push('}');
                        i += 1;
                    }
                } else {
                    // Template literal text — copy verbatim
                    i = push_char(&mut out, src, i);
                }
            }
            continue;
        }

        // Skip line comments
        if ch == b'/' && i + 1 < len && bytes[i + 1] == b'/' {
            while i < len && bytes[i] != b'\n' {
                i = push_char(&mut out, src, i);
            }
            continue;
        }

        // Skip block comments
        if ch == b'/' && i + 1 < len && bytes[i + 1] == b'*' {
            out.push('/');
            out.push('*');
            i += 2;
            while i < len {
                if bytes[i] == b'*' && i + 1 < len && bytes[i + 1] == b'/' {
                    out.push('*');
                    out.push('/');
                    i += 2;
                    break;
                }
                i = push_char(&mut out, src, i);
            }
            continue;
        }

        // Check for keyword at word boundary
        if ch.is_ascii_alphabetic() {
            // Don't replace when part of a larger identifier (`_is`, `$toString`)
            // — check the character immediately before in the source.
            // Also don't replace after `.` (property access) — check with
            // whitespace skipping since `obj . is` is still property access.
            let part_of_identifier = i > 0 && {
                let prev = bytes[i - 1];
                prev == b'_' || prev == b'$' || prev.is_ascii_alphanumeric()
            };
            let after_dot = {
                let mut j = out.len();
                while j > 0 && matches!(out.as_bytes()[j - 1], b' ' | b'\t') {
                    j -= 1;
                }
                j > 0 && out.as_bytes()[j - 1] == b'.'
            };
            let skip_keyword = part_of_identifier || after_dot;

            if !skip_keyword {
                if let Some((replacement, keyword_len, kind)) = try_match_keyword(bytes, i, len) {
                    // Don't replace keywords used as object property names (followed by `:`)
                    let before_colon = {
                        let mut j = i + keyword_len;
                        while j < len && matches!(bytes[j], b' ' | b'\t') {
                            j += 1;
                        }
                        j < len && bytes[j] == b':'
                    };
                    if !before_colon {
                        let pos = out.len();
                        out.push_str(replacement);
                        match kind {
                            KeywordKind::Def => def_positions.push(pos),
                            KeywordKind::Ndef => ndef_positions.push(pos),
                            KeywordKind::Clone => clone_positions.push(pos),
                            KeywordKind::Normal => {}
                        }
                        i += keyword_len;
                        continue;
                    }
                }
            }

            // Not a keyword — copy the identifier
            while i < len && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] == b'$') {
                out.push(bytes[i] as char); // identifier chars are ASCII
                i += 1;
            }
            continue;
        }

        // Default: copy character (handles multi-byte UTF-8)
        i = push_char(&mut out, src, i);
    }

    Preprocessed {
        js: out,
        def_positions,
        ndef_positions,
        clone_positions,
    }
}

#[derive(Clone, Copy)]
enum KeywordKind {
    Normal,
    Def,
    Ndef,
    Clone,
}

/// Try to match a SugarCube keyword at position `i`. Returns (replacement, keyword_len, kind)
/// if matched, or None if no keyword matches.
///
/// Keywords are only matched at word boundaries — the character after the keyword
/// must not be alphanumeric or `_`.
/// Skip a string literal in the preprocess context.
fn skip_string_in_preprocess(bytes: &[u8], start: usize) -> usize {
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

/// Skip a template literal in the preprocess context (nested templates inside ${}).
fn skip_template_in_preprocess(bytes: &[u8], start: usize) -> usize {
    let mut i = start + 1; // skip opening `
    let mut depth = 0u32;
    while i < bytes.len() {
        match bytes[i] {
            b'`' if depth == 0 => return i + 1,
            b'\\' => { i += 2; continue; }
            b'$' if i + 1 < bytes.len() && bytes[i + 1] == b'{' => { i += 2; depth += 1; continue; }
            b'}' if depth > 0 => { depth -= 1; }
            _ => {}
        }
        i += 1;
    }
    i
}

fn try_match_keyword(bytes: &[u8], i: usize, len: usize) -> Option<(&'static str, usize, KeywordKind)> {
    // Match longer keywords first to avoid prefix conflicts (e.g. `isnot` before `is`)
    let candidates: &[(&[u8], &str, KeywordKind)] = &[
        (b"isnot", "!==", KeywordKind::Normal),
        (b"is", "===", KeywordKind::Normal),
        (b"ndef", "typeof", KeywordKind::Ndef),
        (b"neq", "!=", KeywordKind::Normal),
        (b"not", "!", KeywordKind::Normal),
        (b"and", "&&", KeywordKind::Normal),
        (b"or", "||", KeywordKind::Normal),
        (b"lte", "<=", KeywordKind::Normal),
        (b"lt", "<", KeywordKind::Normal),
        (b"gte", ">=", KeywordKind::Normal),
        (b"gt", ">", KeywordKind::Normal),
        (b"eq", "==", KeywordKind::Normal),
        (b"to", "=", KeywordKind::Normal),
        (b"def", "typeof", KeywordKind::Def),
        (b"clone", "typeof", KeywordKind::Clone),
    ];

    for &(kw, replacement, kind) in candidates {
        let kw_len = kw.len();
        if i + kw_len <= len && &bytes[i..i + kw_len] == kw {
            // Check word boundary: next char must not be ident-continue
            let at_boundary = i + kw_len >= len
                || !(bytes[i + kw_len].is_ascii_alphanumeric() || bytes[i + kw_len] == b'_' || bytes[i + kw_len] == b'$');
            if at_boundary {
                return Some((replacement, kw_len, kind));
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pp(src: &str) -> String {
        preprocess(src).js
    }

    #[test]
    fn basic_keywords() {
        assert_eq!(pp("$x is 1"), "$x === 1");
        assert_eq!(pp("$x isnot 1"), "$x !== 1");
        assert_eq!(pp("$x to 1"), "$x = 1");
        assert_eq!(pp("$a and $b"), "$a && $b");
        assert_eq!(pp("$a or $b"), "$a || $b");
        assert_eq!(pp("not $x"), "! $x");
    }

    #[test]
    fn comparison_keywords() {
        assert_eq!(pp("$x eq $y"), "$x == $y");
        assert_eq!(pp("$x neq $y"), "$x != $y");
        assert_eq!(pp("$x lt $y"), "$x < $y");
        assert_eq!(pp("$x lte $y"), "$x <= $y");
        assert_eq!(pp("$x gt $y"), "$x > $y");
        assert_eq!(pp("$x gte $y"), "$x >= $y");
    }

    #[test]
    fn def_ndef_clone_replaced_with_typeof() {
        let result = preprocess("def $x");
        assert_eq!(result.js, "typeof $x");
        assert_eq!(result.def_positions, vec![0]);

        let result = preprocess("ndef $x");
        assert_eq!(result.js, "typeof $x");
        assert_eq!(result.ndef_positions, vec![0]);

        let result = preprocess("clone $x");
        assert_eq!(result.js, "typeof $x");
        assert_eq!(result.clone_positions, vec![0]);
    }

    #[test]
    fn def_without_space() {
        // `def` followed directly by `(` — typeof needs the operand
        let result = preprocess("def($x)");
        assert_eq!(result.js, "typeof($x)");
        assert_eq!(result.def_positions, vec![0]);
    }

    #[test]
    fn property_access_not_replaced() {
        assert_eq!(pp("$obj.is"), "$obj.is");
        assert_eq!(pp("$obj.to"), "$obj.to");
        assert_eq!(pp("$obj.and"), "$obj.and");
        assert_eq!(pp("$obj.or"), "$obj.or");
        assert_eq!(pp("$obj.clone"), "$obj.clone");
    }

    #[test]
    fn strings_not_replaced() {
        assert_eq!(pp(r#""is not""#), r#""is not""#);
        assert_eq!(pp("'and or'"), "'and or'");
    }

    #[test]
    fn template_text_not_replaced() {
        // Keywords in literal text portions of template are NOT replaced
        assert_eq!(pp("`is ${$x} to`"), "`is ${$x} to`");
    }

    #[test]
    fn template_expression_preprocessed() {
        // Keywords inside ${...} ARE preprocessed
        assert_eq!(
            pp("`${$x is 1 ? 'a' : 'b'}`"),
            "`${$x === 1 ? 'a' : 'b'}`"
        );
        assert_eq!(
            pp("`count: ${_n gt 0}`"),
            "`count: ${_n > 0}`"
        );
    }

    #[test]
    fn complex_expression() {
        assert_eq!(
            pp("$enemyarousal gte ($enemyarousalmax / 5) * 4"),
            "$enemyarousal >= ($enemyarousalmax / 5) * 4"
        );
    }

    #[test]
    fn keyword_prefix_not_matched() {
        // "island" should not be matched as "is" + "land"
        assert_eq!(pp("island"), "island");
        // "torch" should not be matched as "to" + "rch"
        assert_eq!(pp("torch"), "torch");
        // "define" should not be matched as "def" + "ine"
        assert_eq!(pp("define"), "define");
    }

    #[test]
    fn isnot_before_is() {
        // "isnot" must match before "is" to avoid "is" + "not" → "===" + "not"
        assert_eq!(pp("$x isnot $y"), "$x !== $y");
    }

    #[test]
    fn mixed_js_and_sugar() {
        assert_eq!(
            pp("$x is 1 && $y isnot 2"),
            "$x === 1 && $y !== 2"
        );
    }

    #[test]
    fn assignment_with_to() {
        assert_eq!(pp("$x to $y + 1"), "$x = $y + 1");
    }

    #[test]
    fn line_comment_not_replaced() {
        assert_eq!(pp("$x // is comment"), "$x // is comment");
    }

    #[test]
    fn block_comment_not_replaced() {
        assert_eq!(pp("$x /* is comment */ + 1"), "$x /* is comment */ + 1");
    }

    #[test]
    fn object_property_not_replaced() {
        assert_eq!(pp("{ from: 0, to: 200 }"), "{ from: 0, to: 200 }");
        assert_eq!(pp("{ is: true }"), "{ is: true }");
        assert_eq!(pp("{ or: 1 }"), "{ or: 1 }");
    }

    #[test]
    fn identifier_suffix_not_replaced() {
        // `_is` is an identifier, not `_` + keyword `is`
        assert_eq!(pp("_is"), "_is");
        assert_eq!(pp("_to"), "_to");
        assert_eq!(pp("_or"), "_or");
        assert_eq!(pp("$isReady"), "$isReady");
        assert_eq!(pp("count"), "count"); // not `co` + `not` somehow
    }

    #[test]
    fn multiple_def_positions_tracked() {
        let result = preprocess("def $x and def $y");
        assert_eq!(result.js, "typeof $x && typeof $y");
        assert_eq!(result.def_positions.len(), 2);
    }

    #[test]
    fn not_as_prefix_operator() {
        // `not` becomes `!` — the space after is preserved from source
        assert_eq!(pp("not $x and not $y"), "! $x && ! $y");
    }

    #[test]
    fn utf8_preserved() {
        // Non-ASCII chars must not be corrupted (no mojibake)
        assert_eq!(pp("$obj.risqué isnot true"), "$obj.risqué !== true");
        assert_eq!(pp("\"café\""), "\"café\"");
        assert_eq!(pp("`naïve ${$x is 1}`"), "`naïve ${$x === 1}`");
    }
}
