//! Macro registry: maps macro names to their syntactic kind.
//!
//! SugarCube macros come in three flavors:
//! - **Block**: `<<name>>...body...<</name>>` (may have clauses like `<<else>>`)
//! - **SelfClosing**: `<<name args>>` (no body, no closing tag)
//! - **Raw**: Like block but the body is raw text, not parsed as SugarCube
//!   (e.g. `<<script>>` body is JS, not passage markup)

/// The syntactic kind of a macro.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroKind {
    /// Block macro with body and closing tag. May have intermediate clauses.
    Block,
    /// Self-closing macro (no body, no `<</name>>`).
    SelfClosing,
    /// Block macro whose body is raw text (not parsed as SugarCube).
    Raw,
}

/// Clauses that can appear inside a block macro.
/// Returns the parent macro name for a clause name.
pub fn clause_parent(clause_name: &str) -> Option<&'static str> {
    match clause_name {
        "elseif" | "else" => Some("if"),
        "case" | "default" => Some("switch"),
        _ => None,
    }
}

/// Return true if the given name is a clause (intermediate tag) rather
/// than a standalone macro.
pub fn is_clause(name: &str) -> bool {
    clause_parent(name).is_some()
}

/// Look up the macro kind for a built-in SugarCube macro name.
/// Returns `None` for unknown/custom macros (widgets, user-defined).
pub fn macro_kind(name: &str) -> Option<MacroKind> {
    Some(match name {
        // ── Control flow (block) ──────────────────────────────────
        "if" => MacroKind::Block,
        "switch" => MacroKind::Block,
        "for" => MacroKind::Block,

        // ── Display / DOM manipulation (block) ────────────────────
        "link" | "linkappend" | "linkprepend" | "linkreplace" => MacroKind::Block,
        "button" => MacroKind::Block,
        "replace" | "append" | "prepend" => MacroKind::Block,
        "repeat" => MacroKind::Block,
        "timed" => MacroKind::Block,
        "type" => MacroKind::Block,
        "nobr" => MacroKind::Block,
        "silently" => MacroKind::Block,
        "capture" => MacroKind::Block,
        "createplaylist" | "createaudiogroup" => MacroKind::Block,
        "widget" => MacroKind::Block,
        "done" => MacroKind::Block,
        "listbox" | "cycle" => MacroKind::Block,

        // ── Self-closing (no body) ────────────────────────────────
        "set" | "unset" => MacroKind::SelfClosing,
        "run" => MacroKind::SelfClosing,
        "print" | "=" | "-" => MacroKind::SelfClosing,
        "include" => MacroKind::SelfClosing,
        "goto" => MacroKind::SelfClosing,
        "back" | "return" => MacroKind::SelfClosing,
        "break" | "continue" => MacroKind::SelfClosing,
        "stop" => MacroKind::SelfClosing,
        "audio" | "masteraudio" | "cacheaudio" | "waitforaudio" | "removeaudiogroup"
        | "removeplaylist" => MacroKind::SelfClosing,
        "addclass" | "removeclass" | "toggleclass" => MacroKind::SelfClosing,
        "copy" => MacroKind::SelfClosing,
        "remove" => MacroKind::SelfClosing,
        "playlist" => MacroKind::SelfClosing,
        "radiobutton" | "checkbox" | "textbox" | "numberbox" | "textarea"
            => MacroKind::SelfClosing,
        "option" | "optionsfrom" => MacroKind::SelfClosing,

        // ── Raw body (block with unparsed content) ────────────────
        "script" => MacroKind::Raw,

        // Clauses are not standalone macros
        "else" | "elseif" | "case" | "default" => return None,

        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_known_macros_classified() {
        let block = &[
            "if", "switch", "for", "link", "linkappend", "linkprepend",
            "linkreplace", "button", "replace", "append", "prepend",
            "repeat", "timed", "type", "nobr", "silently", "capture",
            "createplaylist", "createaudiogroup", "widget", "done",
            "listbox", "cycle",
        ];
        let self_closing = &[
            "set", "unset", "run", "print", "=", "-", "include", "goto",
            "back", "return", "break", "continue", "stop", "audio",
            "masteraudio", "cacheaudio", "waitforaudio", "removeaudiogroup",
            "removeplaylist", "addclass", "removeclass", "toggleclass",
            "copy", "remove", "playlist", "radiobutton", "checkbox",
            "textbox", "numberbox", "textarea", "option", "optionsfrom",
        ];
        let raw = &["script"];

        for &name in block {
            assert_eq!(macro_kind(name), Some(MacroKind::Block), "{name} should be Block");
        }
        for &name in self_closing {
            assert_eq!(macro_kind(name), Some(MacroKind::SelfClosing), "{name} should be SelfClosing");
        }
        for &name in raw {
            assert_eq!(macro_kind(name), Some(MacroKind::Raw), "{name} should be Raw");
        }
    }

    #[test]
    fn unknown_returns_none() {
        assert_eq!(macro_kind("customWidget"), None);
        assert_eq!(macro_kind("person1"), None);
    }

    #[test]
    fn clauses_return_none() {
        assert_eq!(macro_kind("else"), None);
        assert_eq!(macro_kind("elseif"), None);
        assert_eq!(macro_kind("case"), None);
    }

    #[test]
    fn clause_parents() {
        assert_eq!(clause_parent("elseif"), Some("if"));
        assert_eq!(clause_parent("else"), Some("if"));
        assert_eq!(clause_parent("case"), Some("switch"));
        assert_eq!(clause_parent("default"), Some("switch"));
        assert_eq!(clause_parent("if"), None);
    }
}
