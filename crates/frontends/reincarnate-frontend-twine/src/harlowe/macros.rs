//! Harlowe macro classification.
//!
//! Classifies macros by kind to guide parsing: changers attach hooks,
//! commands are standalone, control flow creates branches, and value
//! macros return data.

/// The kind of a Harlowe macro.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MacroKind {
    /// Changers modify the presentation of their attached hook:
    /// `(color:)`, `(text-style:)`, `(font:)`, `(transition:)`, etc.
    Changer,
    /// Commands perform actions: `(set:)`, `(goto:)`, `(display:)`, `(print:)`.
    Command,
    /// Control flow macros create branches: `(if:)`, `(else-if:)`, `(else:)`, `(unless:)`.
    ControlFlow,
    /// Value macros return data: `(str:)`, `(num:)`, `(random:)`, `(a:)`, `(dm:)`.
    Value,
}

/// Classify a macro name into its kind.
pub fn macro_kind(name: &str) -> MacroKind {
    match name {
        // Control flow
        "if" | "else-if" | "elseif" | "else" | "unless" | "for" => MacroKind::ControlFlow,

        // Commands
        "set" | "put" | "move" | "goto" | "go-to" | "display" | "print" | "save-game"
        | "savegame" | "load-game" | "loadgame" | "alert" | "prompt" | "confirm" | "stop"
        | "replace" | "append" | "prepend" | "show" | "hide" | "rerun" | "redo" | "redirect"
        | "link-goto" | "link-undo" | "link-reveal" | "link-repeat" | "link-rerun"
        | "link-replace" | "link-fullscreen" | "click" | "click-replace" | "click-append"
        | "click-prepend" | "click-rerun" | "cycling-link" | "seq-link" | "animate"
        | "goto-url" | "restart" | "reload" | "scroll" | "after" => MacroKind::Command,

        // Changers — includes t8n* aliases and transition-delay/text-rotate aliases
        "color" | "colour" | "text-colour" | "text-color" | "text-style" | "font" | "align"
        | "transition" | "t8n" | "transition-time" | "t8n-time" | "transition-arrive"
        | "transition-depart" | "transition-delay" | "t8n-delay" | "transition-skip" | "t8n-skip"
        | "text-rotate-z" | "text-rotate-x" | "text-rotate-y" | "text-rotate"
        | "hover-style" | "css" | "background" | "box" | "float-box" | "char-style"
        | "line-style" | "page-style" | "opacity" | "text-indent" | "text-size"
        | "collapse" | "nobr" | "verbatim" | "hidden" => MacroKind::Changer,

        // Value macros
        "str" | "string" | "num" | "number" | "a" | "array" | "dm" | "datamap" | "ds"
        | "dataset" | "random" | "either" | "round" | "floor" | "ceil" | "abs" | "min"
        | "max" | "pow" | "sqrt" | "sin" | "cos" | "tan" | "log" | "log10" | "log2"
        | "sign" | "clamp" | "lerp" | "sorted" | "reversed" | "rotated" | "shuffled"
        | "interlaced" | "folded" | "altered" | "count" | "range" | "repeated" | "joined"
        | "some-pass" | "all-pass" | "none-pass" | "find" | "lowercase" | "uppercase"
        | "cond" | "nth" | "substring" | "subarray" | "bit" | "rgb" | "rgba" | "hsl"
        | "hsla" | "gradient" | "lch" | "lcha" | "complement" | "mix"
        | "weekday" | "monthday" | "monthname" | "yearday" | "current-date" | "current-time"
        | "saved-games" | "passage" | "passages" | "visited" | "visits" | "turns" | "history"
        | "hook" | "hooks-named" | "source" | "datanames" | "datavalues" | "dataentries"
        | "dm-names" | "data-names" | "dm-values" | "data-values"
        | "dm-entries" | "data-entries" | "dm-altered" | "datamap-altered"
        | "pass" | "permutations"
        | "v6" | "v8" | "metadata"
        | "macro" | "partial" | "bind" | "bind-2bind" | "2bind" => MacroKind::Value,

        // HAL (Harlowe Audio Library) — third-party audio macros
        "track" | "masteraudio" | "newtrack" | "newplaylist" | "newgroup"
        | "playlist" | "group" => MacroKind::Command,

        // Layout / interactive / state commands
        "columns" | "column" | "enchant" | "enchant-in" | "forget-undos"
        | "forget-visits" | "ignore" => MacroKind::Command,

        // Live is command-like (attaches hook for timed behavior)
        "live" | "event" | "meter" | "dialog" | "dropdown" | "input" | "input-box"
        | "checkbox" | "radio-button" | "force-checkbox" | "force-dropdown"
        | "force-input" => MacroKind::Command,

        // Unknown macros default to Command (standalone, no hook)
        _ => MacroKind::Command,
    }
}

/// Check if a macro is a clause of an if-chain (`else-if`, `else`).
pub fn is_if_clause(name: &str) -> bool {
    matches!(name, "else-if" | "elseif" | "else")
}

/// Check if a macro typically attaches a hook.
pub fn expects_hook(name: &str) -> bool {
    let kind = macro_kind(name);
    matches!(kind, MacroKind::Changer | MacroKind::ControlFlow)
        || matches!(
            name,
            "link"
                | "link-goto"
                | "link-reveal"
                | "link-repeat"
                | "link-rerun"
                | "link-replace"
                | "click"
                | "click-replace"
                | "click-append"
                | "click-prepend"
                | "click-rerun"
                | "live"
                | "event"
                | "after"
                | "for"
                | "dialog"
                | "columns"
                | "cycling-link"
                | "seq-link"
        )
}
