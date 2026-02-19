//! Extraction of custom macro definitions from `Macro.add()` calls in
//! SugarCube JavaScript passages.
//!
//! SugarCube allows games to define custom macros via:
//! ```js
//! Macro.add("name", { handler: function() { ... } })
//! Macro.add("name", { skipArgs: true, handler: function() { ... } })
//! Macro.add("name", { tags: null, handler: function() { ... } })
//! Macro.add(["name1", "name2"], { ... })
//! ```
//!
//! The `tags` property determines block vs self-closing:
//! - Absent → self-closing (no body, no close tag)
//! - `null` or `[...]` → block macro (has body, terminated by `<</name>>`)
//!
//! The `skipArgs` property determines arg parsing semantics:
//! - Absent or `false` → standard `parseArgs()` tokenizer (default)
//! - `true` → entire arg string is a single TwineScript expression

use std::collections::HashMap;

/// Schema extracted from a `Macro.add()` call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CustomMacroDef {
    /// Whether the macro has a block body (`<</name>>` close tag).
    /// True when `tags` property is present in the definition object.
    pub is_block: bool,
    /// Whether the entire arg string is treated as a single TwineScript
    /// expression (`skipArgs: true`). When false, args use `parseArgs()`.
    pub skip_args: bool,
    /// Sub-tag names that can appear inside the block body.
    /// Populated from the `tags: ["subTag"]` array; empty for `tags: null`.
    pub sub_tags: Vec<String>,
}

/// Map from macro name to its schema.
pub type CustomMacroRegistry = HashMap<String, CustomMacroDef>;

/// Scan a JavaScript source string for `Macro.add()` calls and build a registry.
///
/// Uses a simple text-scanning approach (not a full JS parser). Handles:
/// - Single name: `Macro.add("name", { ... })`
/// - Name array: `Macro.add(["a", "b"], { ... })`
/// - Dynamic names (variable first arg) are skipped silently.
pub fn extract_custom_macros(js_source: &str) -> CustomMacroRegistry {
    let mut registry = CustomMacroRegistry::new();
    let bytes = js_source.as_bytes();
    let mut pos = 0;

    while pos < bytes.len() {
        // Find next `Macro.add(`
        let Some(offset) = js_source[pos..].find("Macro.add(") else {
            break;
        };
        pos += offset + "Macro.add(".len();

        // Skip whitespace
        while pos < bytes.len() && bytes[pos].is_ascii_whitespace() {
            pos += 1;
        }
        if pos >= bytes.len() {
            break;
        }

        // Parse first argument: string literal or array of string literals
        let names = match bytes[pos] {
            b'"' | b'\'' => {
                let (name, end) = extract_string_literal(js_source, pos);
                pos = end;
                name.map(|n| vec![n]).unwrap_or_default()
            }
            b'[' => {
                let (names, end) = extract_string_array(js_source, pos);
                pos = end;
                names
            }
            _ => {
                // Dynamic name — skip this call
                continue;
            }
        };

        if names.is_empty() {
            continue;
        }

        // Skip whitespace and comma
        while pos < bytes.len() && bytes[pos].is_ascii_whitespace() {
            pos += 1;
        }
        if pos >= bytes.len() || bytes[pos] != b',' {
            continue;
        }
        pos += 1; // skip ','

        // Skip whitespace
        while pos < bytes.len() && bytes[pos].is_ascii_whitespace() {
            pos += 1;
        }
        if pos >= bytes.len() || bytes[pos] != b'{' {
            continue;
        }

        // Extract the config object literal
        let (config_src, end) = extract_object_literal(js_source, pos);
        pos = end;

        // Parse config properties
        let skip_args = has_skip_args(config_src);
        let (has_tags, sub_tags) = extract_tags(config_src);

        let def = CustomMacroDef {
            is_block: has_tags,
            skip_args,
            sub_tags,
        };

        for name in names {
            registry.insert(name, def.clone());
        }
    }

    registry
}

/// Extract a JS string literal starting at `pos` (quote char at `pos`).
/// Returns `(Some(content), end_pos)` where `end_pos` is past the closing quote.
fn extract_string_literal(src: &str, pos: usize) -> (Option<String>, usize) {
    let bytes = src.as_bytes();
    let quote = bytes[pos];
    let mut i = pos + 1;
    let mut result = String::new();
    while i < bytes.len() {
        if bytes[i] == b'\\' {
            // Skip escaped char (simple escape handling)
            i += 2;
        } else if bytes[i] == quote {
            return (Some(result), i + 1);
        } else {
            result.push(src[i..].chars().next().unwrap_or('\0'));
            i += src[i..].chars().next().map_or(1, |c| c.len_utf8());
        }
    }
    (None, i) // unterminated string
}

/// Extract an array of string literals: `["a", "b", ...]`.
/// Returns `(names, end_pos)` where `end_pos` is past `]`.
fn extract_string_array(src: &str, pos: usize) -> (Vec<String>, usize) {
    let bytes = src.as_bytes();
    debug_assert_eq!(bytes[pos], b'[');
    let mut i = pos + 1;
    let mut names = Vec::new();

    while i < bytes.len() {
        // Skip whitespace
        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }
        match bytes[i] {
            b']' => {
                i += 1;
                break;
            }
            b'"' | b'\'' => {
                let (name, end) = extract_string_literal(src, i);
                i = end;
                if let Some(n) = name {
                    names.push(n);
                }
            }
            b',' => {
                i += 1;
            }
            _ => {
                // Non-string element — skip past array
                i = skip_past_close(src, pos, b'[', b']');
                return (names, i);
            }
        }
    }
    (names, i)
}

/// Extract the object literal starting at `pos` (the `{` char).
/// Returns `(object_src, end_pos)` where `end_pos` is past `}`.
/// `object_src` is the source from `{` to `}` inclusive.
fn extract_object_literal(src: &str, pos: usize) -> (&str, usize) {
    let end = skip_past_close(src, pos, b'{', b'}');
    (&src[pos..end], end)
}

/// Skip from `open` at `pos` to the matching `close`, tracking depth and string literals.
/// Returns position past the `close`.
fn skip_past_close(src: &str, pos: usize, open: u8, close: u8) -> usize {
    let bytes = src.as_bytes();
    debug_assert_eq!(bytes[pos], open);
    let mut depth = 0usize;
    let mut i = pos;
    while i < bytes.len() {
        match bytes[i] {
            b'"' | b'\'' => {
                let quote = bytes[i];
                i += 1;
                while i < bytes.len() {
                    if bytes[i] == b'\\' {
                        i += 2;
                    } else if bytes[i] == quote {
                        i += 1;
                        break;
                    } else {
                        i += 1;
                    }
                }
            }
            b'`' => {
                // Template literal — skip with naive depth tracking
                i += 1;
                while i < bytes.len() {
                    if bytes[i] == b'\\' {
                        i += 2;
                    } else if bytes[i] == b'`' {
                        i += 1;
                        break;
                    } else {
                        i += 1;
                    }
                }
            }
            b if b == open => {
                depth += 1;
                i += 1;
            }
            b if b == close => {
                depth -= 1;
                if depth == 0 {
                    return i + 1;
                }
                i += 1;
            }
            _ => {
                i += 1;
            }
        }
    }
    i
}

/// Return true if `skipArgs: true` appears in the config object source.
/// Handles whitespace variants: `skipArgs : true`, `skipArgs:true`.
fn has_skip_args(config: &str) -> bool {
    let mut rest = config;
    while let Some(idx) = rest.find("skipArgs") {
        let after = rest[idx + "skipArgs".len()..].trim_start();
        if let Some(after_colon) = after.strip_prefix(':') {
            let val = after_colon.trim_start();
            if val.starts_with("true") {
                return true;
            }
        }
        rest = &rest[idx + 1..];
    }
    false
}

/// Return `(has_tags, sub_tags)` from the config object source.
/// `has_tags` is true when a `tags:` property is present (even if `null`).
/// `sub_tags` contains the string elements if it's an array.
fn extract_tags(config: &str) -> (bool, Vec<String>) {
    // Find `tags:` property key
    let mut rest = config;
    while let Some(idx) = rest.find("tags") {
        // Check it's followed by optional whitespace and `:`
        let after = rest[idx + "tags".len()..].trim_start();
        if let Some(after_colon) = after.strip_prefix(':') {
            let val_src = after_colon.trim_start();
            if val_src.starts_with("null") || val_src.starts_with("[]") {
                return (true, Vec::new());
            }
            if val_src.starts_with('[') {
                // Extract array of string literals
                let (names, _) = extract_string_array(val_src, 0);
                return (true, names);
            }
            // `tags:` followed by something else — still counts as present
            return (true, Vec::new());
        }
        rest = &rest[idx + 1..];
    }
    (false, Vec::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn self_closing_no_tags() {
        let js = r#"Macro.add("rng", { handler: function() { this.output.text("hi"); } });"#;
        let reg = extract_custom_macros(js);
        assert_eq!(reg.len(), 1);
        let def = &reg["rng"];
        assert!(!def.is_block);
        assert!(!def.skip_args);
        assert!(def.sub_tags.is_empty());
    }

    #[test]
    fn block_macro_tags_null() {
        let js = r#"Macro.add("foldout", { tags: null, handler: function() {} });"#;
        let reg = extract_custom_macros(js);
        let def = &reg["foldout"];
        assert!(def.is_block);
        assert!(!def.skip_args);
    }

    #[test]
    fn skip_args_true() {
        let js = r#"Macro.add("error", { skipArgs: true, handler: function() {} });"#;
        let reg = extract_custom_macros(js);
        let def = &reg["error"];
        assert!(!def.is_block);
        assert!(def.skip_args);
    }

    #[test]
    fn array_name() {
        let js = r#"Macro.add(["button", "link"], { tags: [], handler: function() {} });"#;
        let reg = extract_custom_macros(js);
        assert!(reg.contains_key("button"));
        assert!(reg.contains_key("link"));
        assert!(reg["button"].is_block);
        assert!(reg["link"].is_block);
    }

    #[test]
    fn tags_with_subtags() {
        let js = r#"Macro.add("condition", { tags: ["compute"], handler: function() {} });"#;
        let reg = extract_custom_macros(js);
        let def = &reg["condition"];
        assert!(def.is_block);
        assert_eq!(def.sub_tags, vec!["compute".to_string()]);
    }

    #[test]
    fn dynamic_name_skipped() {
        let js = r#"Macro.add(widgetName, { handler: function() {} });"#;
        let reg = extract_custom_macros(js);
        assert!(reg.is_empty());
    }

    #[test]
    fn multiple_macros() {
        let js = r#"
            Macro.add("rng", { handler: function() {} });
            Macro.add("foldout", { tags: null, handler: function() {} });
            Macro.add("error", { skipArgs: true, handler: function() {} });
        "#;
        let reg = extract_custom_macros(js);
        assert_eq!(reg.len(), 3);
        assert!(!reg["rng"].is_block);
        assert!(reg["foldout"].is_block);
        assert!(reg["error"].skip_args);
    }

    #[test]
    fn tags_empty_array() {
        let js = r#"Macro.add("dynamicblock", { tags: [], handler: function() {} });"#;
        let reg = extract_custom_macros(js);
        let def = &reg["dynamicblock"];
        assert!(def.is_block);
        assert!(def.sub_tags.is_empty());
    }
}
