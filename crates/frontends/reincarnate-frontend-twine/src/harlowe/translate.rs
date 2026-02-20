//! Harlowe AST → IR translation.
//!
//! Translates parsed Harlowe passage AST nodes into reincarnate-core IR
//! functions using SystemCall-based dispatch to the Harlowe runtime layer.
//!
//! Passage functions receive an `h: HarloweContext` parameter and produce
//! output via side-effecting `Harlowe.H.*` method calls. No intermediate
//! content arrays or virtual DOM — `h.text("Hello")`, `h.em(child)`, etc.
//!
//! SystemCall namespaces:
//! - `Harlowe.State`: get/set story and temp variables
//! - `Harlowe.H`: content emission (text, br, em, strong, link, etc.)
//! - `Harlowe.Navigation`: goto
//! - `Harlowe.Engine`: changers, data ops, runtime helpers

use std::collections::HashMap;

use reincarnate_core::ir::{BlockId, CaptureMode, CmpKind, Function, FunctionBuilder, FunctionSig, MethodKind, Type, ValueId, Visibility};

use super::ast::*;
use super::macros::{self, MacroKind};

/// Result of translating a Harlowe passage.
pub struct TranslateResult {
    /// The main passage function.
    pub func: Function,
    /// Callback functions generated for link hooks, live intervals, etc.
    pub callbacks: Vec<Function>,
    /// Optional storylet condition function generated from `(storylet: when expr)`.
    /// Standalone exported function `(_rt) -> bool` for the storylet system.
    pub storylet_cond: Option<Function>,
}

/// Sanitize a passage name into a valid function name.
pub fn passage_func_name(name: &str) -> String {
    let sanitized: String = name
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() || c == '_' { c } else { '_' })
        .collect();
    format!("passage_{sanitized}")
}

/// Translate a parsed Harlowe passage AST into an IR Function.
pub fn translate_passage(name: &str, ast: &PassageAst, source: &str) -> TranslateResult {
    let func_name = passage_func_name(name);
    let sig = FunctionSig {
        params: vec![Type::Dynamic],
        return_ty: Type::Void,
        defaults: vec![],
        has_rest_param: false,
    };
    let mut fb = FunctionBuilder::new(&func_name, sig, Visibility::Public);
    let h_param = fb.param(0);
    fb.name_value(h_param, "h".to_string());
    let mut ctx = TranslateCtx {
        fb,
        temp_vars: HashMap::new(),
        func_name: func_name.clone(),
        callback_count: 0,
        callbacks: Vec::new(),
        set_target: None,
        passage_name: name.to_string(),
        source: source.to_string(),
        storylet_cond: None,
        in_macro_body: false,
    };

    ctx.emit_content(&ast.body);
    ctx.fb.ret(None);

    let callbacks = std::mem::take(&mut ctx.callbacks);
    let storylet_cond = ctx.storylet_cond.take();

    TranslateResult {
        func: ctx.fb.build(),
        callbacks,
        storylet_cond,
    }
}

struct TranslateCtx {
    fb: FunctionBuilder,
    temp_vars: HashMap<String, ValueId>,
    func_name: String,
    callback_count: usize,
    callbacks: Vec<Function>,
    /// Inside `(set: $x to ...)`, holds the target expression so that
    /// `it` resolves to $x's current value rather than the global `it`.
    set_target: Option<Expr>,
    /// Passage name, for diagnostic messages.
    passage_name: String,
    /// Raw passage source text, for span-based diagnostic context.
    source: String,
    /// Storylet condition function, if this passage contains `(storylet: when expr)`.
    storylet_cond: Option<Function>,
    /// True when lowering inside a `(macro:)` body.
    /// In this context, `(output: value)` emits `Op::Return(value)` rather than printing.
    in_macro_body: bool,
}

/// Extract a short snippet of source text around a span for diagnostic messages.
/// Shows up to 60 chars of context, trimming leading/trailing whitespace.
fn source_context(source: &str, start: usize, end: usize) -> String {
    let src_bytes = source.as_bytes();
    let len = src_bytes.len();
    // Find line start/end
    let line_start = src_bytes[..start.min(len)]
        .iter()
        .rposition(|&b| b == b'\n')
        .map(|i| i + 1)
        .unwrap_or(0);
    let line_end = src_bytes[end.min(len)..]
        .iter()
        .position(|&b| b == b'\n')
        .map(|i| end + i)
        .unwrap_or(len);
    let line = &source[line_start..line_end.min(len)];
    let trimmed = line.trim();
    if trimmed.len() > 80 {
        format!("{}…", &trimmed[..80])
    } else {
        trimmed.to_string()
    }
}

impl TranslateCtx {
    // ── Content emission (side-effect only) ─────────────────────────

    /// Emit content nodes as h.* calls in sequence. Side-effect only.
    fn emit_content(&mut self, nodes: &[Node]) {
        let mut i = 0;
        while i < nodes.len() {
            match &nodes[i].kind {
                NodeKind::HtmlOpen { tag, attrs } => {
                    let tag = tag.clone();
                    let attrs = attrs.clone();
                    let (children_end, _) =
                        self.emit_html_element(nodes, i, &tag, &attrs);
                    i = children_end;
                }
                NodeKind::HtmlClose(_) => {
                    // Stray close tag — skip
                    i += 1;
                }
                // A changer without a hook in statement position may start a composition
                // chain: `(ch1)+(ch2)[hook]`. Detect and emit as a styled() side effect.
                NodeKind::Macro(mac)
                    if mac.hook.is_none()
                        && macros::macro_kind(&mac.name) == MacroKind::Changer =>
                {
                    if let Some((_val, end)) = self.try_lower_changer_chain(nodes, i) {
                        // The styled() IR instruction is preserved by DCE (has_side_effects).
                        i = end;
                    } else {
                        self.emit_node(&nodes[i]);
                        i += 1;
                    }
                }
                _ => {
                    self.emit_node(&nodes[i]);
                    i += 1;
                }
            }
        }
    }

    /// Emit an HTML element: create element with children, append to current container.
    /// Returns (next_index_after_close, ()).
    fn emit_html_element(
        &mut self,
        nodes: &[Node],
        open_idx: usize,
        tag: &str,
        attrs: &[(String, String)],
    ) -> (usize, ()) {
        // Check for shorthand tags (strong, em, del, sup, sub)
        let method = match tag {
            "strong" | "em" | "del" | "sup" | "sub" => tag,
            _ => "el",
        };

        // Collect children as values (for nesting via arguments)
        let (children_end, child_vals) =
            self.collect_html_children_as_values(nodes, open_idx + 1, tag);

        if method == "el" {
            let tag_val = self.fb.const_string(tag);
            let mut args = vec![tag_val];
            args.extend(child_vals);
            for (k, v) in attrs {
                args.push(self.fb.const_string(k));
                args.push(self.fb.const_string(v));
            }
            self.fb
                .system_call("Harlowe.H", "el", &args, Type::Dynamic);
        } else {
            let mut args = child_vals;
            // Shorthand tags shouldn't have extra attrs, but pass them through el() if present
            if !attrs.is_empty() {
                let tag_val = self.fb.const_string(tag);
                args.insert(0, tag_val);
                for (k, v) in attrs {
                    args.push(self.fb.const_string(k));
                    args.push(self.fb.const_string(v));
                }
                self.fb
                    .system_call("Harlowe.H", "el", &args, Type::Dynamic);
            } else {
                self.fb
                    .system_call("Harlowe.H", method, &args, Type::Dynamic);
            }
        }

        (children_end, ())
    }

    /// Collect children between HtmlOpen and matching HtmlClose as ValueIds
    /// (for use as arguments to element methods).
    fn collect_html_children_as_values(
        &mut self,
        nodes: &[Node],
        start: usize,
        close_tag: &str,
    ) -> (usize, Vec<ValueId>) {
        let mut vals = Vec::new();
        let mut i = start;
        while i < nodes.len() {
            match &nodes[i].kind {
                NodeKind::HtmlClose(tag) if tag == close_tag => {
                    return (i + 1, vals);
                }
                NodeKind::HtmlOpen { tag, attrs } => {
                    let tag = tag.clone();
                    let attrs = attrs.clone();
                    let val = self.lower_html_element_as_value(nodes, i, &tag, &attrs);
                    vals.push(val.0);
                    i = val.1;
                }
                NodeKind::HtmlClose(_) => {
                    // Mismatched close tag — stop here
                    return (i + 1, vals);
                }
                NodeKind::Macro(mac)
                    if mac.hook.is_none()
                        && macros::macro_kind(&mac.name) == MacroKind::Changer =>
                {
                    if let Some((val, end)) = self.try_lower_changer_chain(nodes, i) {
                        vals.push(val);
                        i = end;
                    } else {
                        if let Some(v) = self.lower_node_as_value(&nodes[i]) {
                            vals.push(v);
                        }
                        i += 1;
                    }
                }
                _ => {
                    if let Some(v) = self.lower_node_as_value(&nodes[i]) {
                        vals.push(v);
                    }
                    i += 1;
                }
            }
        }
        (i, vals)
    }

    /// Lower an HTML element as a value (for nesting inside parent elements).
    /// Returns (value, next_index).
    fn lower_html_element_as_value(
        &mut self,
        nodes: &[Node],
        open_idx: usize,
        tag: &str,
        attrs: &[(String, String)],
    ) -> (ValueId, usize) {
        let method = match tag {
            "strong" | "em" | "del" | "sup" | "sub" => tag,
            _ => "el",
        };

        let (children_end, child_vals) =
            self.collect_html_children_as_values(nodes, open_idx + 1, tag);

        let val = if method != "el" && attrs.is_empty() {
            // Shorthand (strong, em, etc.) without extra attrs
            self.fb
                .system_call("Harlowe.H", method, &child_vals, Type::Dynamic)
        } else {
            // Generic el() or shorthand with attrs → fall back to el()
            let tag_val = self.fb.const_string(tag);
            let mut args = vec![tag_val];
            args.extend(child_vals);
            for (k, v) in attrs {
                args.push(self.fb.const_string(k));
                args.push(self.fb.const_string(v));
            }
            self.fb
                .system_call("Harlowe.H", "el", &args, Type::Dynamic)
        };

        (val, children_end)
    }

    /// Emit a single node as a side effect (h.* call).
    fn emit_node(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Text(text) => {
                if !text.is_empty() {
                    let s = self.fb.const_string(text);
                    self.fb
                        .system_call("Harlowe.H", "text", &[s], Type::Dynamic);
                }
            }
            NodeKind::Macro(mac) => {
                self.emit_macro(mac);
            }
            NodeKind::Hook(nodes) => {
                // Bare hook: emit content inline
                self.emit_content(nodes);
            }
            NodeKind::Link(link) => {
                let text = self.fb.const_string(&link.text);
                let passage = self.fb.const_string(&link.passage);
                self.fb
                    .system_call("Harlowe.H", "link", &[text, passage], Type::Dynamic);
            }
            NodeKind::VarInterp(name) => {
                let val = self.load_variable(name);
                self.fb
                    .system_call("Harlowe.H", "printVal", &[val], Type::Dynamic);
            }
            NodeKind::HtmlOpen { tag, attrs } => {
                // Standalone open tag without matching close — emit as element
                self.emit_standalone_element(tag, attrs);
            }
            NodeKind::HtmlClose(_) => {}
            NodeKind::HtmlVoid { tag, attrs } => {
                self.emit_void_element(tag, attrs);
            }
            NodeKind::Markup { tag, body } => {
                self.emit_markup(tag, body);
            }
            NodeKind::ChangerApply { name, hook } => {
                self.emit_changer_apply(name, hook);
            }
            NodeKind::LineBreak => {
                self.fb
                    .system_call("Harlowe.H", "br", &[], Type::Dynamic);
            }
            NodeKind::NamedHook { name, body } => {
                if name.is_empty() {
                    // Anonymous right-sided hook `[content]` — render body inline
                    for node in body {
                        self.emit_node(node);
                    }
                } else {
                    let name_val = self.fb.const_string(name);
                    let children = self.lower_children_as_values(body);
                    let mut args = vec![name_val];
                    args.extend(children);
                    self.fb.system_call("Harlowe.H", "namedHook", &args, Type::Dynamic);
                }
            }
        }
    }

    /// Lower a single node as a value (for use as argument to parent element).
    fn lower_node_as_value(&mut self, node: &Node) -> Option<ValueId> {
        match &node.kind {
            NodeKind::Text(text) => {
                if text.is_empty() {
                    None
                } else {
                    let s = self.fb.const_string(text);
                    Some(self.fb.system_call("Harlowe.H", "text", &[s], Type::Dynamic))
                }
            }
            NodeKind::Macro(mac) => self.lower_macro_as_value(mac),
            NodeKind::Hook(nodes) => {
                // Emit children inline — each produces its own h.* call
                for node in nodes {
                    self.emit_node(node);
                }
                None
            }
            NodeKind::Link(link) => {
                let text = self.fb.const_string(&link.text);
                let passage = self.fb.const_string(&link.passage);
                Some(
                    self.fb
                        .system_call("Harlowe.H", "link", &[text, passage], Type::Dynamic),
                )
            }
            NodeKind::VarInterp(name) => {
                let val = self.load_variable(name);
                Some(
                    self.fb
                        .system_call("Harlowe.H", "printVal", &[val], Type::Dynamic),
                )
            }
            NodeKind::HtmlOpen { tag, attrs } => {
                Some(self.lower_standalone_element_as_value(tag, attrs))
            }
            NodeKind::HtmlClose(_) => None,
            NodeKind::HtmlVoid { tag, attrs } => Some(self.lower_void_element(tag, attrs)),
            NodeKind::Markup { tag, body } => Some(self.lower_markup_as_value(tag, body)),
            NodeKind::ChangerApply { name, hook } => {
                Some(self.lower_changer_apply_as_value(name, hook))
            }
            NodeKind::LineBreak => {
                Some(
                    self.fb
                        .system_call("Harlowe.H", "br", &[], Type::Dynamic),
                )
            }
            NodeKind::NamedHook { name, body } => {
                if name.is_empty() {
                    // Anonymous right-sided hook — emit body inline, no value
                    for node in body {
                        self.emit_node(node);
                    }
                    None
                } else {
                    let name_val = self.fb.const_string(name);
                    let children = self.lower_children_as_values(body);
                    let mut args = vec![name_val];
                    args.extend(children);
                    Some(self.fb.system_call("Harlowe.H", "namedHook", &args, Type::Dynamic))
                }
            }
        }
    }

    // ── Node helpers ────────────────────────────────────────────────

    fn load_variable(&mut self, name: &str) -> ValueId {
        if let Some(stripped) = name.strip_prefix('$') {
            let n = self.fb.const_string(stripped);
            self.fb
                .system_call("Harlowe.State", "get", &[n], Type::Dynamic)
        } else if let Some(stripped) = name.strip_prefix('_') {
            self.get_or_load_temp(stripped)
        } else {
            let n = self.fb.const_string(name);
            self.fb
                .system_call("Harlowe.State", "get", &[n], Type::Dynamic)
        }
    }

    fn emit_standalone_element(&mut self, tag: &str, attrs: &[(String, String)]) {
        let tag_val = self.fb.const_string(tag);
        let mut args = vec![tag_val];
        for (k, v) in attrs {
            args.push(self.fb.const_string(k));
            args.push(self.fb.const_string(v));
        }
        self.fb.system_call("Harlowe.H", "el", &args, Type::Dynamic);
    }

    fn lower_standalone_element_as_value(&mut self, tag: &str, attrs: &[(String, String)]) -> ValueId {
        let tag_val = self.fb.const_string(tag);
        let mut args = vec![tag_val];
        for (k, v) in attrs {
            args.push(self.fb.const_string(k));
            args.push(self.fb.const_string(v));
        }
        self.fb.system_call("Harlowe.H", "el", &args, Type::Dynamic)
    }

    fn emit_void_element(&mut self, tag: &str, attrs: &[(String, String)]) {
        self.lower_void_element(tag, attrs);
    }

    fn lower_void_element(&mut self, tag: &str, attrs: &[(String, String)]) -> ValueId {
        match tag {
            "br" => self.fb.system_call("Harlowe.H", "br", &[], Type::Dynamic),
            "hr" => self.fb.system_call("Harlowe.H", "hr", &[], Type::Dynamic),
            "img" => {
                let src = attrs
                    .iter()
                    .find(|(k, _)| k == "src")
                    .map(|(_, v)| v.as_str())
                    .unwrap_or("");
                let src_val = self.fb.const_string(src);
                self.fb
                    .system_call("Harlowe.H", "img", &[src_val], Type::Dynamic)
            }
            _ => {
                let tag_val = self.fb.const_string(tag);
                let mut args = vec![tag_val];
                for (k, v) in attrs {
                    args.push(self.fb.const_string(k));
                    args.push(self.fb.const_string(v));
                }
                self.fb
                    .system_call("Harlowe.H", "voidEl", &args, Type::Dynamic)
            }
        }
    }

    fn emit_markup(&mut self, tag: &str, body: &[Node]) {
        self.lower_markup_as_value(tag, body);
    }

    fn lower_markup_as_value(&mut self, tag: &str, body: &[Node]) -> ValueId {
        let method = match tag {
            "strong" | "em" | "del" | "sup" | "sub" => tag,
            _ => "el",
        };
        let children: Vec<ValueId> = self.lower_children_as_values(body);
        if method == "el" {
            let tag_val = self.fb.const_string(tag);
            let mut args = vec![tag_val];
            args.extend(children);
            self.fb.system_call("Harlowe.H", "el", &args, Type::Dynamic)
        } else {
            self.fb
                .system_call("Harlowe.H", method, &children, Type::Dynamic)
        }
    }

    fn emit_changer_apply(&mut self, name: &str, hook: &[Node]) {
        self.lower_changer_apply_as_value(name, hook);
    }

    fn lower_changer_apply_as_value(&mut self, name: &str, hook: &[Node]) -> ValueId {
        let changer = if let Some(stripped) = name.strip_prefix('$') {
            let n = self.fb.const_string(stripped);
            self.fb
                .system_call("Harlowe.State", "get", &[n], Type::Dynamic)
        } else {
            let stripped = name.strip_prefix('_').unwrap_or(name);
            self.get_or_load_temp(stripped)
        };
        let children: Vec<ValueId> = self.lower_children_as_values(hook);
        let mut args = vec![changer];
        args.extend(children);
        self.fb
            .system_call("Harlowe.H", "styled", &args, Type::Dynamic)
    }

    /// Lower children as values (for element method arguments).
    fn lower_children_as_values(&mut self, nodes: &[Node]) -> Vec<ValueId> {
        let mut vals = Vec::new();
        let mut i = 0;
        while i < nodes.len() {
            match &nodes[i].kind {
                NodeKind::HtmlOpen { tag, attrs } => {
                    let tag = tag.clone();
                    let attrs = attrs.clone();
                    let (val, end) =
                        self.lower_html_element_as_value(nodes, i, &tag, &attrs);
                    vals.push(val);
                    i = end;
                }
                NodeKind::HtmlClose(_) => {
                    i += 1;
                }
                // A changer macro without a hook in content position may be part of a
                // changer-composition chain: `(ch1)+(ch2)[hook]`. Detect and merge.
                NodeKind::Macro(mac)
                    if mac.hook.is_none()
                        && macros::macro_kind(&mac.name) == MacroKind::Changer =>
                {
                    if let Some((val, end)) = self.try_lower_changer_chain(nodes, i) {
                        vals.push(val);
                        i = end;
                    } else {
                        if let Some(v) = self.lower_node_as_value(&nodes[i]) {
                            vals.push(v);
                        }
                        i += 1;
                    }
                }
                _ => {
                    if let Some(v) = self.lower_node_as_value(&nodes[i]) {
                        vals.push(v);
                    }
                    i += 1;
                }
            }
        }
        vals
    }

    // ── Macro emission (side-effect context) ────────────────────────

    fn emit_macro(&mut self, mac: &MacroNode) {
        // Dynamic macro call: `($storyVar: args)` or `(_tempVar: args)` in statement position.
        // The callee variable holds a custom macro closure created with `(macro:)`.
        if let Some(var_name) = mac.name.strip_prefix('$') {
            let callee_val = {
                let n = self.fb.const_string(var_name);
                self.fb.system_call("Harlowe.State", "get", &[n], Type::Dynamic)
            };
            let arg_vals: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
            self.fb.call_indirect(callee_val, &arg_vals, Type::Dynamic);
            return;
        }
        if let Some(var_name) = mac.name.strip_prefix('_') {
            let callee_val = self.get_or_load_temp(var_name);
            let arg_vals: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
            self.fb.call_indirect(callee_val, &arg_vals, Type::Dynamic);
            return;
        }
        match mac.name.as_str() {
            // State (side effects)
            "set" => self.lower_set(mac),
            "put" => self.lower_put(mac),

            // Control flow — emit directly (no value)
            "if" | "unless" => { self.emit_if(mac); }
            "for" | "loop" => { self.emit_for(mac); }

            // Navigation (side effect)
            "goto" | "go-to" => self.lower_goto(mac),

            // Display (side effect — emits inline)
            "display" => self.emit_display(mac),

            // Print (side effect — emits text)
            "print" => self.emit_print(mac),

            // Links (side effect — emits link element)
            "link" | "link-replace" => { self.emit_link_macro(mac); }
            "link-goto" => { self.emit_link_goto(mac); }
            "link-rerun" | "link-repeat" | "linkrepeat" => self.lower_link_rerun(mac),
            "link-reveal" => self.lower_link_reveal(mac),
            "link-reveal-goto" => self.lower_link_reveal_goto(mac),
            "link-undo" => self.lower_link_undo(mac),

            // URL navigation
            "goto-url" | "openurl" | "open-url" => self.lower_goto_url(mac),

            // Changers (side effect when hook present, otherwise create changer value)
            "color" | "colour" | "text-colour" | "text-color" | "text-style" | "font"
            | "align" | "transition" | "t8n" | "transition-time" | "t8n-time"
            | "transition-arrive" | "transition-depart" | "transition-delay" | "t8n-delay"
            | "t8n-skip" | "text-rotate-z" | "text-rotate" | "hover-style" | "css"
            | "background" | "opacity" | "text-size" | "collapse" | "nobr" | "verbatim"
            | "hidden" => {
                self.emit_changer(mac);
            }

            // Timed
            "live" => { self.emit_live(mac); }
            "stop" => {
                self.fb
                    .system_call("Harlowe.H", "requestStop", &[], Type::Void);
            }

            // Value macros — print result
            "str" | "string" | "num" | "number" | "random" | "either" | "a" | "array"
            | "dm" | "datamap" | "ds" | "dataset" | "upperfirst" | "lowerfirst" => {
                self.emit_value_macro_standalone(mac);
            }

            // Save/load (side effects, with aliases)
            "save-game" | "savegame" => self.lower_save_game(mac),
            "load-game" | "loadgame" => self.lower_load_game(mac),

            // Alert/prompt/confirm (side effects)
            "alert" => { self.lower_simple_command(mac, "Harlowe.Engine", "alert"); }
            "prompt" => { self.lower_simple_command(mac, "Harlowe.Engine", "prompt"); }
            "confirm" => { self.lower_simple_command(mac, "Harlowe.Engine", "confirm"); }

            // DOM manipulation (side effects)
            "replace" | "append" | "prepend" | "show" | "hide" | "rerun" => {
                self.lower_dom_macro(mac);
            }

            // Click (side effects)
            "click" | "click-replace" | "click-append" | "click-prepend" | "click-rerun" => {
                self.lower_click_macro(mac);
            }

            // State management
            "forget-undos" => {
                let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
                self.fb.system_call(
                    "Harlowe.Engine",
                    "forget_undos",
                    &args,
                    Type::Void,
                );
            }
            "forget-visits" => {
                self.fb.system_call(
                    "Harlowe.Engine",
                    "forget_visits",
                    &[],
                    Type::Void,
                );
            }
            "ignore" => {
                // No-op — deliberately discards its arguments
            }

            // Storylet system (Harlowe 3.3+)
            "storylet" => self.lower_storylet_macro(mac),
            "exclusivity" => {
                // No-op for decompilation: exclusivity affects storylet scheduling,
                // not individual passage rendering.
            }

            // Sidebar icon macros (Harlowe 3.3+) — no-op: sidebar already works
            "icon-undo" | "icon-redo" | "icon-restart" => {}

            // HAL (Harlowe Audio Library) macros
            "track" | "playlist" | "group" => self.lower_hal_named_command(mac),
            "masteraudio" => self.lower_hal_master_audio(mac),
            "newtrack" => self.lower_hal_simple_command(mac, "define_track"),
            "newplaylist" => self.lower_hal_simple_command(mac, "define_playlist"),
            "newgroup" => self.lower_hal_simple_command(mac, "define_group"),

            // Undo — navigate back one turn
            "undo" => {
                self.fb
                    .system_call("Harlowe.Navigation", "undo", &[], Type::Void);
            }

            // Restart / reload
            "restart" | "reload" => {
                self.fb
                    .system_call("Harlowe.Navigation", "restart", &[], Type::Void);
            }

            // scroll — scroll to an element or the top
            "scroll" => self.lower_simple_command(mac, "Harlowe.Engine", "scroll_macro"),

            // animate — apply a CSS animation to a named hook
            "animate" => self.lower_simple_command(mac, "Harlowe.Engine", "animate_macro"),

            // link-fullscreen — link that toggles fullscreen
            "link-fullscreen" => {
                self.lower_simple_command(mac, "Harlowe.Engine", "link_fullscreen")
            }

            // after — show hook after a delay
            "after" => self.lower_after_macro(mac),

            // Interactive input macros
            "dropdown" | "checkbox" | "input-box" => self.lower_input_macro(mac),

            // Cycling/sequence links — interactive links that rotate through choices
            "cycling-link" | "seq-link" => self.lower_cycling_link(mac),

            // else-if / elseif appearing standalone (outside an if-chain) — treat as if
            "else-if" | "elseif" => {
                self.emit_if(mac);
            }

            // else appearing standalone (outside an if-chain) — emit hook unconditionally
            "else" => {
                if let Some(ref hook) = mac.hook {
                    self.emit_content(hook);
                }
            }

            // Columns layout
            "columns" => self.lower_columns_macro(mac),

            // Dialog
            "dialog" => self.lower_dialog_macro(mac),

            // Enchant
            "enchant" | "enchant-in" => self.lower_enchant_macro(mac),

            // Meter
            "meter" => self.lower_meter_macro(mac),
            "column" => {
                self.fb
                    .system_call("Harlowe.H", "columnBreak", &[], Type::Void);
            }

            // Custom macro definition — `(macro: type _p, ...)[body]`.
            // Standalone (as a statement) is a no-op; only useful as a value (via assignment).
            "macro" => {}

            // Custom macro return — `(output: value)` inside a `(macro:)` body.
            // Acts as `return value` from the macro function.
            "output" | "output-data" => {
                if self.in_macro_body {
                    if let Some(arg) = mac.args.first() {
                        let val = self.lower_expr(arg);
                        self.fb.ret(Some(val));
                    } else {
                        self.fb.ret(None);
                    }
                } else {
                    self.emit_value_macro_standalone(mac);
                }
            }

            // Unknown — route via macro_kind() before falling to unknown
            _ => match macros::macro_kind(&mac.name) {
                MacroKind::Changer => { self.emit_changer(mac); }
                MacroKind::Value => { self.emit_value_macro_standalone(mac); }
                _ => self.lower_unknown_macro(mac),
            },
        }
    }

    // ── Macro lowering as value (for nesting inside elements) ───────

    fn lower_macro_as_value(&mut self, mac: &MacroNode) -> Option<ValueId> {
        // Dynamic macro call in value position: `($storyVar: args)` or `(_tempVar: args)`.
        if let Some(var_name) = mac.name.strip_prefix('$') {
            let callee_val = {
                let n = self.fb.const_string(var_name);
                self.fb.system_call("Harlowe.State", "get", &[n], Type::Dynamic)
            };
            let arg_vals: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
            return Some(self.fb.call_indirect(callee_val, &arg_vals, Type::Dynamic));
        }
        if let Some(var_name) = mac.name.strip_prefix('_') {
            let callee_val = self.get_or_load_temp(var_name);
            let arg_vals: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
            return Some(self.fb.call_indirect(callee_val, &arg_vals, Type::Dynamic));
        }
        match mac.name.as_str() {
            // State macros don't produce values
            "set" | "put" => {
                self.emit_macro(mac);
                None
            }

            // Control flow — not yet supported as value child
            "if" | "unless" => {
                self.emit_if(mac);
                None
            }
            "for" | "loop" => {
                self.emit_for(mac);
                None
            }

            "goto" | "go-to" => {
                self.lower_goto(mac);
                None
            }

            "display" => {
                self.emit_display(mac);
                None
            }

            "print" => {
                if let Some(arg) = mac.args.first() {
                    let val = self.lower_expr(arg);
                    Some(self.fb.system_call("Harlowe.H", "printVal", &[val], Type::Dynamic))
                } else {
                    None
                }
            }

            "link" | "link-replace" => Some(self.lower_link_macro_as_value(mac)),
            "link-goto" => Some(self.lower_link_goto_as_value(mac)),
            "link-reveal" => { self.lower_link_reveal(mac); None }
            "link-reveal-goto" => { self.lower_link_reveal_goto(mac); None }
            "link-undo" => { self.lower_link_undo(mac); None }
            "link-rerun" | "link-repeat" | "linkrepeat" => { self.lower_link_rerun(mac); None }
            "goto-url" | "openurl" | "open-url" => { self.lower_goto_url(mac); None }

            "color" | "colour" | "text-colour" | "text-color" | "text-style" | "font"
            | "align" | "transition" | "t8n" | "transition-time" | "t8n-time"
            | "transition-arrive" | "transition-depart" | "transition-delay" | "t8n-delay"
            | "t8n-skip" | "text-rotate-z" | "text-rotate" | "hover-style" | "css"
            | "background" | "opacity" | "text-size" | "collapse" | "nobr" | "verbatim"
            | "hidden" => {
                self.lower_changer_as_value(mac)
            }

            "live" => Some(self.lower_live_as_value(mac)),

            "stop" => {
                self.fb.system_call("Harlowe.H", "requestStop", &[], Type::Void);
                None
            }

            "str" | "string" | "num" | "number" | "random" | "either" | "a" | "array"
            | "dm" | "datamap" | "ds" | "dataset" | "upperfirst" | "lowerfirst" => {
                Some(self.lower_value_macro_as_value(mac))
            }

            // Storylet system
            "open-storylets" | "storylets-of" => {
                Some(self.lower_open_storylets(mac))
            }

            // Custom macro definition: `(macro: type _p, ...)[body]` → closure value.
            "macro" => mac.hook.as_deref().map(|body| {
                self.lower_macro_definition(mac, body)
            }),

            // Custom macro return: `(output: value)` inside a macro body → return value.
            "output" | "output-data" => {
                if self.in_macro_body {
                    if let Some(arg) = mac.args.first() {
                        let val = self.lower_expr(arg);
                        self.fb.ret(Some(val));
                    } else {
                        self.fb.ret(None);
                    }
                    None
                } else {
                    Some(self.lower_value_macro_as_value(mac))
                }
            }

            // Unknown — route via macro_kind() before falling back
            _ => match macros::macro_kind(&mac.name) {
                MacroKind::Changer => self.lower_changer_as_value(mac),
                MacroKind::Value => Some(self.lower_value_macro_as_value(mac)),
                _ => {
                    self.emit_macro(mac);
                    None
                }
            },
        }
    }

    // ── (set:) ─────────────────────────────────────────────────────

    fn lower_set(&mut self, mac: &MacroNode) {
        for arg in &mac.args {
            self.lower_assignment(arg);
        }
    }

    fn lower_put(&mut self, mac: &MacroNode) {
        for arg in &mac.args {
            self.lower_assignment(arg);
        }
    }

    fn lower_assignment(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Assign { target, value } => {
                let prev_target = self.set_target.replace(*target.clone());
                let val = self.lower_expr(value);
                self.set_target = prev_target;
                self.store_to_target(target, val);
            }
            _ => {
                self.lower_expr(expr);
            }
        }
    }

    fn store_to_target(&mut self, target: &Expr, value: ValueId) {
        match &target.kind {
            ExprKind::StoryVar(name) => {
                let n = self.fb.const_string(name.as_str());
                self.fb
                    .system_call("Harlowe.State", "set", &[n, value], Type::Void);
            }
            ExprKind::TempVar(name) => {
                let alloc = self.get_or_create_temp(name);
                self.fb.store(alloc, value);
            }
            ExprKind::Possessive { object, property } => {
                let obj = self.lower_expr(object);
                let prop = self.lower_expr(property);
                self.fb.system_call(
                    "Harlowe.Engine",
                    "set_property",
                    &[obj, prop, value],
                    Type::Void,
                );
            }
            _ => {
                self.lower_expr(target);
            }
        }
    }

    // ── (if:) / (unless:) / (else-if:) / (else:) ──────────────────

    /// Emit if/unless as control flow — no value, just side effects.
    fn emit_if(&mut self, mac: &MacroNode) {
        let merge_block = self.fb.create_block();

        // Main condition
        let cond = if mac.args.is_empty() {
            self.fb.const_bool(true)
        } else {
            let raw_cond = self.lower_expr(&mac.args[0]);
            if mac.name == "unless" {
                self.fb.system_call(
                    "Harlowe.Engine",
                    "not",
                    &[raw_cond],
                    Type::Bool,
                )
            } else {
                raw_cond
            }
        };

        let then_block = self.fb.create_block();
        let else_block = self.fb.create_block();

        self.fb
            .br_if(cond, then_block, &[], else_block, &[]);

        // Then body (hook)
        self.fb.switch_to_block(then_block);
        if let Some(ref hook) = mac.hook {
            self.emit_content(hook);
        }
        self.fb.br(merge_block, &[]);

        // Else chain
        self.fb.switch_to_block(else_block);
        self.emit_if_clauses(&mac.clauses, 0, merge_block);

        self.fb.switch_to_block(merge_block);
    }

    fn emit_if_clauses(&mut self, clauses: &[IfClause], index: usize, merge_block: BlockId) {
        if index >= clauses.len() {
            // No else — just jump to merge
            self.fb.br(merge_block, &[]);
            return;
        }

        let clause = &clauses[index];

        if clause.kind == "else" {
            self.emit_content(&clause.body);
            self.fb.br(merge_block, &[]);
            return;
        }

        // else-if
        let cond = if let Some(ref cond_expr) = clause.cond {
            self.lower_expr(cond_expr)
        } else {
            self.fb.const_bool(true)
        };

        let then_block = self.fb.create_block();
        let next_else = self.fb.create_block();

        self.fb
            .br_if(cond, then_block, &[], next_else, &[]);

        self.fb.switch_to_block(then_block);
        self.emit_content(&clause.body);
        self.fb.br(merge_block, &[]);

        self.fb.switch_to_block(next_else);
        self.emit_if_clauses(clauses, index + 1, merge_block);
    }

    // ── (for:) ─────────────────────────────────────────────────────

    fn emit_for(&mut self, mac: &MacroNode) {
        let hook = match &mac.hook {
            Some(h) => h.clone(),
            None => return,
        };

        // First arg must be a lambda: `each _var [where cond]`
        let (loop_var, filter) = if let Some(first) = mac.args.first() {
            match &first.kind {
                ExprKind::Lambda { var, filter } => (var.clone(), filter.as_deref().cloned()),
                _ => {
                    self.lower_unknown_macro(mac);
                    return;
                }
            }
        } else {
            self.lower_unknown_macro(mac);
            return;
        };

        // Remaining args are the iterable values (each optionally wrapped in Spread).
        // A single `...array` arg → use the array directly as the iterable.
        // Multiple values → wrap in an array via Harlowe.Engine.array.
        let item_args: Vec<ValueId> = mac.args[1..]
            .iter()
            .map(|a| match &a.kind {
                ExprKind::Spread(inner) => self.lower_expr(inner),
                _ => self.lower_expr(a),
            })
            .collect();

        let iterable = if item_args.len() == 1 {
            item_args[0]
        } else if item_args.is_empty() {
            // No items — nothing to iterate
            return;
        } else {
            let name = self.fb.const_string("a");
            let mut args = vec![name];
            args.extend(item_args);
            self.fb
                .system_call("Harlowe.Engine", "value_macro", &args, Type::Dynamic)
        };

        // Build callback: (h: Context, _item: any) => void
        let cb_name = self.make_callback_name("for");
        let cb_ref = self.build_for_callback(&cb_name, &loop_var, filter.as_ref(), &hook);

        // Pass h so the runtime can forward it to the callback
        let h = self.fb.param(0);
        self.fb.system_call(
            "Harlowe.Engine",
            "for_each",
            &[iterable, cb_ref, h],
            Type::Void,
        );
    }

    /// Build a predicate/transform callback for collection ops.
    ///
    /// `param_ty` is the inferred element type of the collection (Dynamic if unknown).
    /// `return_ty` is the callback return type (Bool for predicates, Dynamic for transforms).
    ///
    /// Binds `_var` to the item parameter and evaluates `filter_expr`, or returns
    /// `true` (for Bool returns) / the item (for Dynamic returns) if no filter.
    fn build_lambda_callback(
        &mut self,
        var: &str,
        filter: Option<&Expr>,
        param_ty: Type,
        return_ty: Type,
    ) -> ValueId {
        let cb_name = self.make_callback_name("lambda");

        // --- 1. Capture outer temp vars by value ---
        let (outer_temps, capture_spec) = self.collect_outer_captures();
        let capture_vals: Vec<ValueId> = outer_temps.iter().map(|(_, v)| *v).collect();

        // --- 2. Build the closure FunctionBuilder ---
        let sig = FunctionSig {
            params: vec![param_ty.clone(), Type::Int(32)],
            return_ty: return_ty.clone(),
            defaults: vec![],
            has_rest_param: false,
        };
        let mut cb_fb = FunctionBuilder::new(&cb_name, sig, Visibility::Public);
        cb_fb.set_method_kind(MethodKind::Closure);
        let item_param = cb_fb.param(0);
        let pos_param = cb_fb.param(1);

        // Register capture params (appended after regular params in entry block).
        let cap_ids = cb_fb.add_capture_params(capture_spec);

        // --- 3. Lower the body in the closure context ---
        let saved_fb = std::mem::replace(&mut self.fb, cb_fb);
        let saved_temps = std::mem::take(&mut self.temp_vars);

        // Pre-populate temp_vars from capture params so outer var references resolve.
        self.init_captured_temps(&outer_temps, &cap_ids);

        // Bind the loop variable to the item parameter.
        let alloc = self.fb.alloc(param_ty);
        self.fb.name_value(alloc, format!("_{var}"));
        self.fb.store(alloc, item_param);
        self.temp_vars.insert(var.to_string(), alloc);

        // Bind `pos` to the 1-based position parameter so `pos` inside the body resolves.
        let pos_alloc = self.fb.alloc(Type::Int(32));
        self.fb.name_value(pos_alloc, "_pos".to_string());
        self.fb.store(pos_alloc, pos_param);
        self.temp_vars.insert("pos".to_string(), pos_alloc);

        let result = if let Some(filter_expr) = filter {
            self.lower_expr(filter_expr)
        } else {
            // No filter: pass all items.
            match return_ty {
                Type::Bool => self.fb.const_bool(true),
                _ => {
                    // Identity transform: return the item unchanged.
                    self.fb.load(alloc, Type::Dynamic)
                }
            }
        };
        self.fb.ret(Some(result));

        let cb_fb = std::mem::replace(&mut self.fb, saved_fb);
        self.temp_vars = saved_temps;

        let mut func = cb_fb.build();
        func.method_kind = MethodKind::Closure;
        self.callbacks.push(func);

        // --- 4. Emit Op::MakeClosure ---
        self.fb.make_closure(&cb_name, &capture_vals, Type::Dynamic)
    }

    /// Build a two-parameter fold callback `(item, acc) => body`.
    /// Used for `(folded:)` — `each _item making _acc via expr`.
    /// The runtime calls `fn(item, acc)` and uses the return value as the new accumulator.
    fn build_fold_callback(&mut self, item_var: &str, acc_var: &str, body: &Expr) -> ValueId {
        let cb_name = self.make_callback_name("fold");

        // --- 1. Capture outer temp vars by value ---
        let (outer_temps, capture_spec) = self.collect_outer_captures();
        let capture_vals: Vec<ValueId> = outer_temps.iter().map(|(_, v)| *v).collect();

        // --- 2. Build the closure FunctionBuilder ---
        let sig = FunctionSig {
            params: vec![Type::Dynamic, Type::Dynamic],
            return_ty: Type::Dynamic,
            defaults: vec![],
            has_rest_param: false,
        };
        let mut cb_fb = FunctionBuilder::new(&cb_name, sig, Visibility::Public);
        cb_fb.set_method_kind(MethodKind::Closure);
        let item_param = cb_fb.param(0);
        let acc_param = cb_fb.param(1);

        // Register capture params (appended after regular params).
        let cap_ids = cb_fb.add_capture_params(capture_spec);

        // --- 3. Lower body in the closure context ---
        let saved_fb = std::mem::replace(&mut self.fb, cb_fb);
        let saved_temps = std::mem::take(&mut self.temp_vars);

        self.init_captured_temps(&outer_temps, &cap_ids);

        // Bind item parameter.
        let item_alloc = self.fb.alloc(Type::Dynamic);
        self.fb.name_value(item_alloc, format!("_{item_var}"));
        self.fb.store(item_alloc, item_param);
        self.temp_vars.insert(item_var.to_string(), item_alloc);

        // Bind accumulator parameter.
        let acc_alloc = self.fb.alloc(Type::Dynamic);
        self.fb.name_value(acc_alloc, format!("_{acc_var}"));
        self.fb.store(acc_alloc, acc_param);
        self.temp_vars.insert(acc_var.to_string(), acc_alloc);

        let result = self.lower_expr(body);
        self.fb.ret(Some(result));

        let cb_fb = std::mem::replace(&mut self.fb, saved_fb);
        self.temp_vars = saved_temps;

        let mut func = cb_fb.build();
        func.method_kind = MethodKind::Closure;
        self.callbacks.push(func);

        // --- 4. Emit Op::MakeClosure ---
        self.fb.make_closure(&cb_name, &capture_vals, Type::Dynamic)
    }

    /// Infer the element type from a list of lowered collection values.
    ///
    /// If any value has type `Array(T)`, returns `T`. Otherwise returns `Dynamic`.
    /// Used to give lambda parameters a more precise type than `Dynamic`.
    fn infer_element_type(&self, item_vals: &[ValueId]) -> Type {
        for &val in item_vals {
            if let Type::Array(elem_ty) = self.fb.value_type(val) {
                return *elem_ty;
            }
        }
        Type::Dynamic
    }

    /// Lower a collection op whose first argument is a predicate lambda.
    ///
    /// Items (args[1..]) are lowered first so their types are known, then the
    /// element type is inferred and the lambda is built with that param type.
    fn lower_predicate_collection_op(
        &mut self,
        name: &str,
        args: &[Expr],
        pred_return_ty: Type,
    ) -> ValueId {
        // Lower items first to learn element type.
        let item_vals: Vec<ValueId> = args
            .get(1..)
            .unwrap_or_default()
            .iter()
            .map(|a| self.lower_expr(a))
            .collect();
        let elem_ty = self.infer_element_type(&item_vals);

        // Lower the predicate, inlining lambda type information if available.
        let pred_val = match args.first() {
            Some(pred_expr) => {
                if let ExprKind::Lambda { var, filter } = &pred_expr.kind {
                    self.build_lambda_callback(
                        var,
                        filter.as_deref(),
                        elem_ty,
                        pred_return_ty,
                    )
                } else if let ExprKind::ViaLambda(body) = &pred_expr.kind {
                    self.build_lambda_callback("it", Some(body), elem_ty, pred_return_ty)
                } else {
                    self.lower_expr(pred_expr)
                }
            }
            None => self.fb.const_bool(true),
        };

        let n = self.fb.const_string(name);
        let mut call_args = vec![n, pred_val];
        call_args.extend(item_vals);
        self.fb
            .system_call("Harlowe.Engine", "collection_op", &call_args, Type::Dynamic)
    }

    fn build_for_callback(
        &mut self,
        name: &str,
        loop_var: &str,
        filter: Option<&Expr>,
        body: &[Node],
    ) -> ValueId {
        // --- 1. Capture outer temp vars by value ---
        let (outer_temps, capture_spec) = self.collect_outer_captures();
        let capture_vals: Vec<ValueId> = outer_temps.iter().map(|(_, v)| *v).collect();

        // --- 2. Build the closure FunctionBuilder ---
        let sig = FunctionSig {
            params: vec![Type::Dynamic, Type::Dynamic],
            return_ty: Type::Void,
            defaults: vec![],
            has_rest_param: false,
        };
        let mut cb_fb = FunctionBuilder::new(name, sig, Visibility::Public);
        cb_fb.set_method_kind(MethodKind::Closure);
        let h_param = cb_fb.param(0);
        cb_fb.name_value(h_param, "h".to_string());
        let item_param = cb_fb.param(1);

        // Register capture params.
        let cap_ids = cb_fb.add_capture_params(capture_spec);

        // --- 3. Lower body in the closure context ---
        let saved_fb = std::mem::replace(&mut self.fb, cb_fb);
        let saved_temps = std::mem::take(&mut self.temp_vars);

        // Pre-populate temp_vars from capture params.
        self.init_captured_temps(&outer_temps, &cap_ids);

        // Store the loop variable param into an alloc so temp var accesses work.
        let alloc = self.fb.alloc(Type::Dynamic);
        self.fb.name_value(alloc, format!("_{loop_var}"));
        self.fb.store(alloc, item_param);
        self.temp_vars.insert(loop_var.to_string(), alloc);

        // If a `where` filter was given, skip items that don't pass it.
        if let Some(filter_expr) = filter {
            let cond = self.lower_expr(filter_expr);
            let body_block = self.fb.create_block();
            let skip_block = self.fb.create_block();
            self.fb.br_if(cond, body_block, &[], skip_block, &[]);
            self.fb.switch_to_block(skip_block);
            self.fb.ret(None);
            self.fb.switch_to_block(body_block);
        }

        self.emit_content(body);
        self.fb.ret(None);

        let cb_fb = std::mem::replace(&mut self.fb, saved_fb);
        self.temp_vars = saved_temps;

        let mut func = cb_fb.build();
        func.method_kind = MethodKind::Closure;
        self.callbacks.push(func);

        // --- 4. Emit Op::MakeClosure ---
        self.fb.make_closure(name, &capture_vals, Type::Dynamic)
    }

    // ── (goto:) ────────────────────────────────────────────────────

    fn lower_goto(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let target = self.lower_expr(arg);
            self.fb
                .system_call("Harlowe.Navigation", "goto", &[target], Type::Void);
        }
    }

    // ── (display:) ─────────────────────────────────────────────────

    fn emit_display(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let target = self.lower_expr(arg);
            self.fb.system_call(
                "Harlowe.H",
                "displayPassage",
                &[target],
                Type::Void,
            );
        }
    }

    // ── (print:) ───────────────────────────────────────────────────

    fn emit_print(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let val = self.lower_expr(arg);
            self.fb
                .system_call("Harlowe.H", "printVal", &[val], Type::Dynamic);
        }
    }

    // ── (link:) ────────────────────────────────────────────────────

    fn emit_link_macro(&mut self, mac: &MacroNode) {
        self.lower_link_macro_as_value(mac);
    }

    fn lower_link_macro_as_value(&mut self, mac: &MacroNode) -> ValueId {
        if let Some(arg) = mac.args.first() {
            let text = self.lower_expr(arg);

            if let Some(ref hook) = mac.hook {
                let cb_name = self.make_callback_name("link");
                let cb_ref = self.build_callback(&cb_name, hook);
                self.fb.system_call(
                    "Harlowe.H",
                    "linkCb",
                    &[text, cb_ref],
                    Type::Dynamic,
                )
            } else {
                self.fb
                    .system_call("Harlowe.H", "printVal", &[text], Type::Dynamic)
            }
        } else {
            // No args — emit nothing, return a dummy
            self.fb.const_bool(false)
        }
    }

    fn emit_link_goto(&mut self, mac: &MacroNode) {
        self.lower_link_goto_as_value(mac);
    }

    fn lower_link_goto_as_value(&mut self, mac: &MacroNode) -> ValueId {
        if mac.args.len() >= 2 {
            let text = self.lower_expr(&mac.args[0]);
            let passage = self.lower_expr(&mac.args[1]);
            self.fb.system_call(
                "Harlowe.H",
                "link",
                &[text, passage],
                Type::Dynamic,
            )
        } else if let Some(arg) = mac.args.first() {
            let text = self.lower_expr(arg);
            self.fb.system_call(
                "Harlowe.H",
                "link",
                &[text, text],
                Type::Dynamic,
            )
        } else {
            self.fb.const_bool(false)
        }
    }

    // ── Changers ───────────────────────────────────────────────────

    fn emit_changer(&mut self, mac: &MacroNode) {
        self.lower_changer_as_value(mac);
    }

    /// In Harlowe passage content, `(ch1)+(ch2)[hook]` composes changers.
    /// Detect that pattern starting at `start` and emit as a single `styled()` call.
    /// Returns `(value, end_index)` on success, or `None` if the pattern doesn't match.
    fn try_lower_changer_chain(
        &mut self,
        nodes: &[Node],
        start: usize,
    ) -> Option<(ValueId, usize)> {
        // Scan to determine the span of the chain, borrowing `nodes` immutably.
        let mut changer_indices: Vec<usize> = Vec::new();
        let mut i = start;

        loop {
            match nodes.get(i).map(|n| &n.kind) {
                Some(NodeKind::Macro(mac))
                    if macros::macro_kind(&mac.name) == MacroKind::Changer =>
                {
                    let has_hook = mac.hook.is_some();
                    changer_indices.push(i);
                    i += 1;
                    if has_hook {
                        break; // terminal changer
                    }
                    // Consume `+` separator (may have surrounding whitespace)
                    match nodes.get(i).map(|n| &n.kind) {
                        Some(NodeKind::Text(t)) if t.trim() == "+" => {
                            i += 1;
                        }
                        _ => break,
                    }
                }
                _ => break,
            }
        }

        let last_idx = *changer_indices.last().unwrap();
        let has_hook = matches!(
            &nodes[last_idx].kind,
            NodeKind::Macro(m) if m.hook.is_some()
        );
        // If no terminal hook, try to use remaining nodes as the implicit hook.
        // This handles: `(ch1) + (ch2) + remaining_content` inside a named hook body.
        // Also handles the single-changer case: `(ch)[remaining_content]` (pending changer).
        if !has_hook && i >= nodes.len() {
            return None;
        }
        // Single changer with its own explicit hook is handled by normal changer lowering.
        if changer_indices.len() < 2 && has_hook {
            return None;
        }

        // Build changer objects for all nodes.
        // For the terminal-hooked case, all but the last supply the changer value;
        // for the implicit-hook case, ALL supply changer values.
        let terminal_end = if has_hook {
            changer_indices.len() - 1
        } else {
            changer_indices.len()
        };
        let mut changer_vals: Vec<ValueId> = Vec::new();
        for &idx in &changer_indices[..terminal_end] {
            // Clone to avoid long borrow while calling `self` methods.
            let mac = match &nodes[idx].kind {
                NodeKind::Macro(m) => m.clone(),
                _ => unreachable!(),
            };
            // Build just the changer object (hook-less version).
            let mac_no_hook = MacroNode { hook: None, ..mac };
            if let Some(v) = self.lower_changer_as_value(&mac_no_hook) {
                changer_vals.push(v);
            }
        }

        // If the last changer has an explicit hook, add its changer value and collect its children.
        // Otherwise, the remaining sibling nodes form the implicit hook.
        let hook_nodes: Vec<Node> = if has_hook {
            // Clone the terminal changer to build both its changer object and hook children.
            let terminal_mac = match &nodes[last_idx].kind {
                NodeKind::Macro(m) => m.clone(),
                _ => unreachable!(),
            };
            let terminal_no_hook = MacroNode {
                hook: None,
                ..terminal_mac.clone()
            };
            if let Some(v) = self.lower_changer_as_value(&terminal_no_hook) {
                changer_vals.push(v);
            }
            terminal_mac.hook.as_ref().unwrap().clone()
        } else {
            // No explicit hook — the remaining sibling nodes form the implicit hook.
            nodes[i..].to_vec()
        };

        // Compose all changers left-to-right via `plus()`.
        let composed = changer_vals
            .into_iter()
            .reduce(|a, b| {
                self.fb
                    .system_call("Harlowe.Engine", "plus", &[a, b], Type::Dynamic)
            })
            .expect("at least one changer");

        // Apply composed changer to the hook children.
        let children = self.lower_children_as_values(&hook_nodes);
        let mut args = vec![composed];
        args.extend(children);
        let result = self
            .fb
            .system_call("Harlowe.H", "styled", &args, Type::Dynamic);

        // When using remaining sibling nodes as implicit hook, consume all of them.
        let end = if has_hook { i } else { nodes.len() };
        Some((result, end))
    }

    fn lower_changer_as_value(&mut self, mac: &MacroNode) -> Option<ValueId> {
        let builder_name = match mac.name.as_str() {
            "color" | "colour" | "text-colour" | "text-color" => "color",
            "background" => "background",
            "text-style" => "textStyle",
            "font" => "font",
            "align" => "align",
            "opacity" => "opacity",
            "css" => "css",
            "transition" | "t8n" | "transition-arrive" => "transition",
            "transition-depart" => "transitionDepart",
            "transition-time" | "t8n-time" => "transitionTime",
            "transition-delay" | "t8n-delay" => "transitionDelay",
            "transition-skip" | "t8n-skip" => "transitionSkip",
            "hidden" => "hidden",
            "text-size" => "textSize",
            "text-rotate-z" | "text-rotate" => "textRotateZ",
            "collapse" => "collapse",
            "nobr" => "nobr",
            "verbatim" => "verbatim",
            "hover-style" => "hoverStyle",
            _ => {
                // Unknown changer — fall back to generic styled
                if let Some(ref hook) = mac.hook {
                    let changer_name = self.fb.const_string(&mac.name);
                    let args: Vec<ValueId> =
                        mac.args.iter().map(|a| self.lower_expr(a)).collect();
                    let mut changer_args = vec![changer_name];
                    changer_args.extend(args);
                    let changer = self.fb.system_call(
                        "Harlowe.Engine",
                        "create_changer",
                        &changer_args,
                        Type::Dynamic,
                    );
                    let children: Vec<ValueId> = self.lower_children_as_values(hook);
                    let mut styled_args = vec![changer];
                    styled_args.extend(children);
                    return Some(self.fb.system_call(
                        "Harlowe.H",
                        "styled",
                        &styled_args,
                        Type::Dynamic,
                    ));
                }
                // Changer in expression context (no hook)
                let changer_name = self.fb.const_string(&mac.name);
                let args: Vec<ValueId> =
                    mac.args.iter().map(|a| self.lower_expr(a)).collect();
                let mut call_args = vec![changer_name];
                call_args.extend(args);
                return Some(self.fb.system_call(
                    "Harlowe.Engine",
                    "create_changer",
                    &call_args,
                    Type::Dynamic,
                ));
            }
        };

        if let Some(ref hook) = mac.hook {
            // Changer with hook → h.color("red", child1, child2)
            let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
            let children: Vec<ValueId> = self.lower_children_as_values(hook);

            let call_args = match builder_name {
                "hidden" | "collapse" | "nobr" | "verbatim" => children,
                _ => {
                    let mut a = args;
                    a.extend(children);
                    a
                }
            };

            Some(self.fb.system_call(
                "Harlowe.H",
                builder_name,
                &call_args,
                Type::Dynamic,
            ))
        } else {
            // Changer in expression context (no hook) → create_changer value
            let changer_name = self.fb.const_string(&mac.name);
            let args: Vec<ValueId> =
                mac.args.iter().map(|a| self.lower_expr(a)).collect();
            let mut call_args = vec![changer_name];
            call_args.extend(args);
            Some(self.fb.system_call(
                "Harlowe.Engine",
                "create_changer",
                &call_args,
                Type::Dynamic,
            ))
        }
    }

    // ── (live:) ────────────────────────────────────────────────────

    fn emit_live(&mut self, mac: &MacroNode) {
        self.lower_live_as_value(mac);
    }

    fn lower_live_as_value(&mut self, mac: &MacroNode) -> ValueId {
        let interval = if let Some(arg) = mac.args.first() {
            self.lower_expr(arg)
        } else {
            self.fb.const_float(1.0)
        };

        if let Some(ref hook) = mac.hook {
            let cb_name = self.make_callback_name("live");
            let cb_ref = self.build_callback(&cb_name, hook);
            self.fb.system_call(
                "Harlowe.H",
                "live",
                &[interval, cb_ref],
                Type::Dynamic,
            )
        } else {
            self.fb.const_bool(false)
        }
    }

    // ── Value macros (standalone) ──────────────────────────────────

    fn emit_value_macro_standalone(&mut self, mac: &MacroNode) {
        let val = self.lower_value_macro_as_value(mac);
        // If it has a hook, the styled call already emitted; otherwise print it
        if mac.hook.is_none() {
            self.fb
                .system_call("Harlowe.H", "printVal", &[val], Type::Dynamic);
        }
    }

    fn lower_value_macro_as_value(&mut self, mac: &MacroNode) -> ValueId {
        let name = self.fb.const_string(&mac.name);
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        let mut call_args = vec![name];
        call_args.extend(args);
        let result = self.fb.system_call(
            "Harlowe.Engine",
            "value_macro",
            &call_args,
            Type::Dynamic,
        );
        if let Some(ref hook) = mac.hook {
            // Value macro with hook — use styled()
            let children: Vec<ValueId> = self.lower_children_as_values(hook);
            let mut styled_args = vec![result];
            styled_args.extend(children);
            self.fb.system_call(
                "Harlowe.H",
                "styled",
                &styled_args,
                Type::Dynamic,
            )
        } else {
            result
        }
    }

    // ── Save/load ──────────────────────────────────────────────────

    fn lower_save_game(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let slot = self.lower_expr(arg);
            self.fb
                .system_call("Harlowe.Engine", "save_game", &[slot], Type::Dynamic);
        }
    }

    fn lower_load_game(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let slot = self.lower_expr(arg);
            self.fb
                .system_call("Harlowe.Engine", "load_game", &[slot], Type::Void);
        }
    }

    // ── Simple commands ────────────────────────────────────────────

    fn lower_simple_command(&mut self, mac: &MacroNode, system: &str, method: &str) {
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        self.fb
            .system_call(system, method, &args, Type::Dynamic);
    }

    // ── DOM macros ─────────────────────────────────────────────────

    fn lower_dom_macro(&mut self, mac: &MacroNode) {
        let method = self.fb.const_string(&mac.name);
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        let mut call_args = vec![method];
        call_args.extend(args);

        if let Some(ref hook) = mac.hook {
            let cb_name = self.make_callback_name("dom");
            let cb_ref = self.build_callback(&cb_name, hook);
            call_args.push(cb_ref);
        }

        self.fb
            .system_call("Harlowe.Engine", "dom_macro", &call_args, Type::Void);
    }

    // ── Click macros ───────────────────────────────────────────────

    fn lower_click_macro(&mut self, mac: &MacroNode) {
        let method = self.fb.const_string(&mac.name);
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        let mut call_args = vec![method];
        call_args.extend(args);

        if let Some(ref hook) = mac.hook {
            let cb_name = self.make_callback_name("click");
            let cb_ref = self.build_callback(&cb_name, hook);
            call_args.push(cb_ref);
        }

        self.fb
            .system_call("Harlowe.Engine", "click_macro", &call_args, Type::Void);
    }

    // ── Columns ────────────────────────────────────────────────

    fn lower_columns_macro(&mut self, mac: &MacroNode) {
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        if let Some(ref hook) = mac.hook {
            let cb_name = self.make_callback_name("columns");
            let cb_ref = self.build_callback(&cb_name, hook);
            let mut call_args = args;
            call_args.push(cb_ref);
            self.fb.system_call(
                "Harlowe.Engine",
                "columns_macro",
                &call_args,
                Type::Void,
            );
        }
    }

    // ── Meter ──────────────────────────────────────────────────

    fn lower_meter_macro(&mut self, mac: &MacroNode) {
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        self.fb.system_call(
            "Harlowe.Engine",
            "meter_macro",
            &args,
            Type::Void,
        );
    }

    // ── Enchant ────────────────────────────────────────────────

    fn lower_enchant_macro(&mut self, mac: &MacroNode) {
        let method = if mac.name == "enchant-in" { "enchant_in" } else { "enchant" };
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        self.fb.system_call(
            "Harlowe.Engine",
            method,
            &args,
            Type::Void,
        );
    }

    // ── Dialog ─────────────────────────────────────────────────

    fn lower_dialog_macro(&mut self, mac: &MacroNode) {
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        if let Some(ref hook) = mac.hook {
            let cb_name = self.make_callback_name("dialog");
            let cb_ref = self.build_callback(&cb_name, hook);
            let mut call_args = args;
            call_args.push(cb_ref);
            self.fb.system_call(
                "Harlowe.Engine",
                "dialog_macro",
                &call_args,
                Type::Void,
            );
        }
    }

    // ── link-rerun ─────────────────────────────────────────────────

    fn lower_link_rerun(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let text = self.lower_expr(arg);
            if let Some(ref hook) = mac.hook {
                let cb_name = self.make_callback_name("link_rerun");
                let cb_ref = self.build_callback(&cb_name, hook);
                self.fb
                    .system_call("Harlowe.Engine", "link_rerun", &[text, cb_ref], Type::Void);
            } else {
                self.fb
                    .system_call("Harlowe.H", "printVal", &[text], Type::Dynamic);
            }
        }
    }

    // ── link-reveal / link-reveal-goto / link-undo / goto-url ──────

    /// `(link-reveal: text)[hook]` — link that replaces itself with hook content on click.
    fn lower_link_reveal(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let text = self.lower_expr(arg);
            if let Some(ref hook) = mac.hook {
                let cb_name = self.make_callback_name("link_reveal");
                let cb_ref = self.build_callback(&cb_name, hook);
                self.fb
                    .system_call("Harlowe.Engine", "link_reveal", &[text, cb_ref], Type::Void);
            } else {
                self.fb
                    .system_call("Harlowe.H", "printVal", &[text], Type::Dynamic);
            }
        }
    }

    /// `(link-reveal-goto: text, passage)[hook]` — link that reveals hook content then navigates.
    fn lower_link_reveal_goto(&mut self, mac: &MacroNode) {
        let text = mac.args.first().map(|a| self.lower_expr(a)).unwrap_or_else(|| self.fb.const_string(""));
        let passage = mac.args.get(1).map(|a| self.lower_expr(a)).unwrap_or_else(|| self.fb.const_string(""));
        if let Some(ref hook) = mac.hook {
            let cb_name = self.make_callback_name("link_reveal_goto");
            let cb_ref = self.build_callback(&cb_name, hook);
            self.fb.system_call(
                "Harlowe.Engine",
                "link_reveal_goto",
                &[text, passage, cb_ref],
                Type::Void,
            );
        } else {
            self.fb
                .system_call("Harlowe.Engine", "link_reveal_goto", &[text, passage], Type::Void);
        }
    }

    /// `(link-undo: text)` — link that undoes the last turn.
    fn lower_link_undo(&mut self, mac: &MacroNode) {
        let text = mac.args.first().map(|a| self.lower_expr(a)).unwrap_or_else(|| self.fb.const_string("Undo"));
        self.fb
            .system_call("Harlowe.Engine", "link_undo", &[text], Type::Void);
    }

    /// `(goto-url: url)` — open a URL in a new browser tab.
    fn lower_goto_url(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let url = self.lower_expr(arg);
            self.fb
                .system_call("Harlowe.Engine", "goto_url", &[url], Type::Void);
        }
    }

    // ── Storylet system ────────────────────────────────────────────

    /// `(open-storylets:)` / `(open-storylets: where lambda)` —
    /// returns an array of passage info objects for available storylets.
    /// Optional `where` lambda filters the results.
    fn lower_open_storylets(&mut self, mac: &MacroNode) -> ValueId {
        if let Some(arg) = mac.args.first() {
            // Filter predicate provided (e.g. `where its tags contains "tag"`)
            let filter = self.lower_expr(arg);
            self.fb.system_call("Harlowe.Engine", "open_storylets", &[filter], Type::Dynamic)
        } else {
            self.fb.system_call("Harlowe.Engine", "open_storylets", &[], Type::Dynamic)
        }
    }

    /// `(storylet: when expr)` — marks this passage as a storylet.
    ///
    /// Builds a standalone condition function `storylet_cond_<passage>(_rt) -> bool`
    /// and stores it in `self.storylet_cond` for export.  Also emits a runtime
    /// registration call so late (post-first-render) checks still work.
    fn lower_storylet_macro(&mut self, mac: &MacroNode) {
        let cond_arg = match mac.args.first() {
            Some(a) => a.clone(),
            None => return,
        };

        // Build a standalone condition function (no `h` param — only `_rt` via
        // emit.rs's stateful-module prepend).
        let cond_name = format!("storylet_cond_{}", self.func_name);
        let cond_sig = FunctionSig {
            params: vec![],
            return_ty: Type::Bool,
            defaults: vec![],
            has_rest_param: false,
        };
        let cond_fb = FunctionBuilder::new(&cond_name, cond_sig, Visibility::Public);

        // Evaluate the condition inside the new FunctionBuilder context.
        let saved_fb = std::mem::replace(&mut self.fb, cond_fb);
        let saved_temps = std::mem::take(&mut self.temp_vars);

        let result = self.lower_expr(&cond_arg);
        self.fb.ret(Some(result));

        let cond_fb = std::mem::replace(&mut self.fb, saved_fb);
        self.temp_vars = saved_temps;

        self.storylet_cond = Some(cond_fb.build());
    }

    // ── after ──────────────────────────────────────────────────────

    fn lower_after_macro(&mut self, mac: &MacroNode) {
        let delay = if let Some(arg) = mac.args.first() {
            self.lower_expr(arg)
        } else {
            self.fb.const_float(0.0)
        };
        if let Some(ref hook) = mac.hook {
            let cb_name = self.make_callback_name("after");
            let cb_ref = self.build_callback(&cb_name, hook);
            self.fb
                .system_call("Harlowe.Engine", "after_macro", &[delay, cb_ref], Type::Void);
        }
    }

    // ── Input macros ───────────────────────────────────────────────

    fn lower_input_macro(&mut self, mac: &MacroNode) {
        let method = self.fb.const_string(&mac.name);
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        let mut call_args = vec![method];
        call_args.extend(args);
        self.fb
            .system_call("Harlowe.Engine", "input_macro", &call_args, Type::Void);
    }

    /// `(cycling-link: [bind $var,] opt1, opt2, ...)` /
    /// `(seq-link: [bind $var,] opt1, opt2, ...)` —
    /// creates a link that cycles (or sequences) through a list of options,
    /// optionally binding the current choice to a story variable.
    fn lower_cycling_link(&mut self, mac: &MacroNode) {
        let is_cycling = mac.name == "cycling-link";
        let cycling_val = self.fb.const_bool(is_cycling);
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        let mut call_args = vec![cycling_val];
        call_args.extend(args);
        self.fb
            .system_call("Harlowe.Engine", "cycling_link", &call_args, Type::Void);
    }

    // ── HAL (Harlowe Audio Library) macros ─────────────────────────

    /// `(track: name, command, ...args)`, `(playlist: ...)`, `(group: ...)` —
    /// forward to the audio runtime using the macro name as the method.
    fn lower_hal_named_command(&mut self, mac: &MacroNode) {
        let method = mac.name.as_str();
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        self.fb.system_call("Harlowe.Audio", method, &args, Type::Void);
    }

    /// `(masteraudio: command, ...args)` → `Harlowe.Audio.master_audio(command, ...args)`
    fn lower_hal_master_audio(&mut self, mac: &MacroNode) {
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        self.fb.system_call("Harlowe.Audio", "master_audio", &args, Type::Void);
    }

    /// `(newtrack: name, ...urls)`, `(newplaylist: ...)`, `(newgroup: ...)` —
    /// forward to the named method on the audio runtime.
    fn lower_hal_simple_command(&mut self, mac: &MacroNode, method: &str) {
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        self.fb.system_call("Harlowe.Audio", method, &args, Type::Void);
    }

    // ── Unknown macros ─────────────────────────────────────────────

    fn lower_unknown_macro(&mut self, mac: &MacroNode) {
        // Emit a diagnostic with surrounding source context.
        let ctx_snippet = source_context(&self.source, mac.span.start, mac.span.end);
        eprintln!(
            "warning: unknown macro '({:}:)' in passage '{}'\n  {}",
            mac.name, self.passage_name, ctx_snippet
        );

        let name = self.fb.const_string(&mac.name);
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        let mut call_args = vec![name];
        call_args.extend(args);
        self.fb
            .system_call("Harlowe.Engine", "unknown_macro", &call_args, Type::Dynamic);

        if let Some(ref hook) = mac.hook {
            self.emit_content(hook);
        }
    }

    // ── Callback building ──────────────────────────────────────────

    fn make_callback_name(&mut self, kind: &str) -> String {
        let name = format!("{}_{kind}_{}", self.func_name, self.callback_count);
        self.callback_count += 1;
        name
    }

    fn build_callback(&mut self, name: &str, body: &[Node]) -> ValueId {
        // --- 1. Capture outer temp vars by value ---
        let (outer_temps, capture_spec) = self.collect_outer_captures();
        let capture_vals: Vec<ValueId> = outer_temps.iter().map(|(_, v)| *v).collect();

        // --- 2. Build the closure FunctionBuilder ---
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void,
            defaults: vec![],
            has_rest_param: false,
        };
        let mut cb_fb = FunctionBuilder::new(name, sig, Visibility::Public);
        cb_fb.set_method_kind(MethodKind::Closure);
        let h_param = cb_fb.param(0);
        cb_fb.name_value(h_param, "h".to_string());

        // Register capture params.
        let cap_ids = cb_fb.add_capture_params(capture_spec);

        // --- 3. Lower body in the closure context ---
        let saved_fb = std::mem::replace(&mut self.fb, cb_fb);
        let saved_temps = std::mem::take(&mut self.temp_vars);

        // Pre-populate temp_vars from capture params.
        self.init_captured_temps(&outer_temps, &cap_ids);

        self.emit_content(body);
        self.fb.ret(None);

        let cb_fb = std::mem::replace(&mut self.fb, saved_fb);
        self.temp_vars = saved_temps;

        let mut func = cb_fb.build();
        func.method_kind = MethodKind::Closure;
        self.callbacks.push(func);

        // --- 4. Emit Op::MakeClosure ---
        self.fb.make_closure(name, &capture_vals, Type::Dynamic)
    }

    /// Lower `(macro: type _p0, type _p1, ...)[body]` to a TypeScript arrow function closure.
    ///
    /// Parameters are extracted from the args list as temp-var expressions (type keywords are
    /// ignored — they alternate with param names). The body is lowered with `in_macro_body: true`
    /// so that `(output: value)` emits `Op::Return(value)`.
    fn lower_macro_definition(&mut self, mac: &MacroNode, body: &[Node]) -> ValueId {
        // Extract parameter names: skip type-annotation idents, collect temp var names.
        let params: Vec<String> = mac
            .args
            .iter()
            .filter_map(|a| {
                if let ExprKind::TempVar(name) = &a.kind {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();

        let cb_name = self.make_callback_name("macro");
        self.build_macro_closure(&cb_name, &params, body)
    }

    /// Build a closure for a `(macro:)` body: takes `params` as arguments, returns `Dynamic`.
    /// Inside the body, `(output:)` emits `Op::Return(value)` via the `in_macro_body` flag.
    fn build_macro_closure(&mut self, name: &str, params: &[String], body: &[Node]) -> ValueId {
        // --- 1. Capture outer temp vars ---
        let (outer_temps, capture_spec) = self.collect_outer_captures();
        let capture_vals: Vec<ValueId> = outer_temps.iter().map(|(_, v)| *v).collect();

        // --- 2. Build the FunctionBuilder: one param per declared macro parameter ---
        let param_types = vec![Type::Dynamic; params.len()];
        let sig = FunctionSig {
            params: param_types,
            return_ty: Type::Dynamic,
            defaults: vec![],
            has_rest_param: false,
        };
        let mut cb_fb = FunctionBuilder::new(name, sig, Visibility::Public);
        cb_fb.set_method_kind(MethodKind::Closure);

        // Register capture params.
        let cap_ids = cb_fb.add_capture_params(capture_spec);

        // --- 3. Lower body in the closure context ---
        let saved_fb = std::mem::replace(&mut self.fb, cb_fb);
        let saved_temps = std::mem::take(&mut self.temp_vars);
        let saved_in_macro_body = self.in_macro_body;
        self.in_macro_body = true;

        // Pre-populate temp_vars from capture params.
        self.init_captured_temps(&outer_temps, &cap_ids);

        // Bind each declared parameter: param → Alloc slot (same pattern as loop vars).
        // Mem2Reg will eliminate the alloc/store/load chain.
        for (i, param_name) in params.iter().enumerate() {
            let pv = self.fb.param(i);
            self.fb.name_value(pv, format!("_{param_name}"));
            let alloc = self.fb.alloc(Type::Dynamic);
            self.fb.name_value(alloc, format!("_{param_name}"));
            self.fb.store(alloc, pv);
            self.temp_vars.insert(param_name.clone(), alloc);
        }

        self.emit_content(body);
        // Implicit return void if no (output:) was reached.
        // Using ret(None) prevents BoolLiteralReturn from inferring Bool return type.
        self.fb.ret(None);

        let cb_fb = std::mem::replace(&mut self.fb, saved_fb);
        self.temp_vars = saved_temps;
        self.in_macro_body = saved_in_macro_body;

        let mut func = cb_fb.build();
        func.method_kind = MethodKind::Closure;
        self.callbacks.push(func);

        // --- 4. Emit Op::MakeClosure ---
        self.fb.make_closure(name, &capture_vals, Type::Dynamic)
    }

    // ── Temp variable helpers ──────────────────────────────────────

    fn get_or_create_temp(&mut self, name: &str) -> ValueId {
        if let Some(&alloc) = self.temp_vars.get(name) {
            return alloc;
        }
        let alloc = self.fb.alloc(Type::Dynamic);
        self.fb.name_value(alloc, format!("_{name}"));
        self.temp_vars.insert(name.to_string(), alloc);
        alloc
    }

    fn get_or_load_temp(&mut self, name: &str) -> ValueId {
        let alloc = self.get_or_create_temp(name);
        self.fb.load(alloc, Type::Dynamic)
    }

    /// Load all outer temp var values in the current builder (creation-time snapshot).
    ///
    /// Returns `(outer_temps, capture_spec)` for use with `add_capture_params` and
    /// `init_captured_temps`.  The first element is `(name, loaded_val)` pairs;
    /// the second is the corresponding `(cap_name, ty, mode)` triples.
    #[allow(clippy::type_complexity)]
    fn collect_outer_captures(
        &mut self,
    ) -> (Vec<(String, ValueId)>, Vec<(String, Type, CaptureMode)>) {
        let outer_temps: Vec<(String, ValueId)> = self
            .temp_vars
            .iter()
            .map(|(name, &alloc_id)| {
                let val = self.fb.load(alloc_id, Type::Dynamic);
                (name.clone(), val)
            })
            .collect();
        let spec: Vec<(String, Type, CaptureMode)> = outer_temps
            .iter()
            .map(|(name, _)| (format!("cap_{name}"), Type::Dynamic, CaptureMode::ByValue))
            .collect();
        (outer_temps, spec)
    }

    /// After swapping to a new FunctionBuilder, pre-populate `temp_vars` with
    /// alloc/store chains backed by the given capture param values.
    ///
    /// Mem2Reg will later promote the single-store allocs to direct references.
    fn init_captured_temps(&mut self, outer_temps: &[(String, ValueId)], cap_ids: &[ValueId]) {
        for (i, (name, _)) in outer_temps.iter().enumerate() {
            let alloc = self.fb.alloc(Type::Dynamic);
            self.fb.store(alloc, cap_ids[i]);
            self.temp_vars.insert(name.clone(), alloc);
        }
    }

    // ── Expression lowering ────────────────────────────────────────

    fn lower_expr(&mut self, expr: &Expr) -> ValueId {
        match &expr.kind {
            ExprKind::Number(n) => self.fb.const_float(*n),
            ExprKind::Str(s) => self.fb.const_string(s.as_str()),
            ExprKind::Bool(b) => self.fb.const_bool(*b),
            ExprKind::It => {
                // Inside a `via` lambda, `it` is the closure parameter stored as "it".
                if let Some(&alloc) = self.temp_vars.get("it") {
                    return self.fb.load(alloc, Type::Dynamic);
                }
                if let Some(ref target) = self.set_target.clone() {
                    self.lower_expr(target)
                } else {
                    self.fb
                        .system_call("Harlowe.State", "get_it", &[], Type::Dynamic)
                }
            }
            ExprKind::StoryVar(name) => {
                let n = self.fb.const_string(name.as_str());
                self.fb
                    .system_call("Harlowe.State", "get", &[n], Type::Dynamic)
            }
            ExprKind::TempVar(name) => self.get_or_load_temp(name),
            ExprKind::Ident(name) => {
                // Harlowe keyword variables: special built-in values usable without ()
                match name.as_str() {
                    "visits" => self
                        .fb
                        .system_call("Harlowe.State", "current_visits", &[], Type::Dynamic),
                    "time" => self
                        .fb
                        .system_call("Harlowe.State", "elapsed_time", &[], Type::Dynamic),
                    "turns" => self
                        .fb
                        .system_call("Harlowe.State", "turns", &[], Type::Dynamic),
                    // `pos` — 1-based position inside lambda callbacks (via, for, enchant).
                    // When we're inside a lambda, it's bound in temp_vars as "pos".
                    "pos" => {
                        if let Some(&alloc) = self.temp_vars.get("pos") {
                            self.fb.load(alloc, Type::Int(32))
                        } else {
                            self.fb.const_string("pos")
                        }
                    }
                    _ => self.fb.const_string(name.as_str()),
                }
            }
            ExprKind::ColorLiteral(color) => self.fb.const_string(color.as_str()),
            ExprKind::TimeLiteral(secs) => self.fb.const_float(*secs),
            ExprKind::Ordinal(ord) => self.lower_ordinal(ord),
            ExprKind::Binary { op, left, right } => self.lower_binary(op, left, right),
            ExprKind::Unary { op, operand } => self.lower_unary(op, operand),
            ExprKind::Assign { target, value } => {
                let val = self.lower_expr(value);
                self.store_to_target(target, val);
                val
            }
            ExprKind::Call { name, args } => self.lower_call(name, args),
            ExprKind::Possessive { object, property } => {
                let obj = self.lower_expr(object);
                let prop = self.lower_expr(property);
                self.fb.system_call(
                    "Harlowe.Engine",
                    "get_property",
                    &[obj, prop],
                    Type::Dynamic,
                )
            }
            ExprKind::Of { property, object } => {
                let obj = self.lower_expr(object);
                let prop = self.lower_expr(property);
                self.fb.system_call(
                    "Harlowe.Engine",
                    "get_property",
                    &[obj, prop],
                    Type::Dynamic,
                )
            }
            ExprKind::Paren(inner) => self.lower_expr(inner),
            // Named hook selector `?name` → CSS selector string for querySelectorAll
            ExprKind::HookSelector(name) => {
                let sel = format!("tw-hook[name='{name}']");
                self.fb.const_string(sel.as_str())
            }
            // Spread: strip the wrapper and lower the inner expression.
            // (for:) handles Spread explicitly before calling lower_expr.)
            ExprKind::Spread(inner) => self.lower_expr(inner),
            // Lambda: build a predicate callback `(item) => filter_expr`.
            // When a lambda appears as an argument to a predicate collection op,
            // `lower_predicate_collection_op` handles it directly with inferred types.
            // This fallback handles lambdas in other contexts (e.g. `(altered:)`,
            // dynamic macro args) where no context type is available.
            ExprKind::Lambda { var, filter } => {
                self.build_lambda_callback(var, filter.as_deref(), Type::Dynamic, Type::Dynamic)
            }
            ExprKind::ViaLambda(body) => {
                // `via expr` — transform lambda: `(it) => expr`.
                self.build_lambda_callback("it", Some(body), Type::Dynamic, Type::Dynamic)
            }
            ExprKind::FoldLambda {
                item_var,
                acc_var,
                body,
            } => self.build_fold_callback(item_var, acc_var, body),
            // Dynamic macro call: `($var: args)` — callee holds a custom macro function.
            ExprKind::DynCall { callee, args } => {
                let callee_val = self.lower_expr(callee);
                let arg_vals: Vec<ValueId> = args.iter().map(|a| self.lower_expr(a)).collect();
                self.fb.call_indirect(callee_val, &arg_vals, Type::Dynamic)
            }
            // `(macro: type _param, ...)[body]` in expression position.
            // Re-parse the hook body text (captured by the expression lexer) and build a closure.
            ExprKind::MacroDef { params, hook_source } => {
                let body_ast = super::parser::parse(hook_source);
                let param_names: Vec<String> = params
                    .iter()
                    .filter_map(|p| {
                        if let ExprKind::TempVar(name) = &p.kind {
                            Some(name.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                let cb_name = self.make_callback_name("macro");
                self.build_macro_closure(&cb_name, &param_names, &body_ast.body)
            }
            // `bind $var` / `2bind $var` — produce a { get, set } bind-ref object.
            // The runtime uses this to initialize the input and update the variable on change.
            ExprKind::Bind { two_way, target } => {
                let name = match &target.kind {
                    ExprKind::StoryVar(n) => n.clone(),
                    ExprKind::TempVar(n) => format!("_{n}"),
                    _ => {
                        // Unusual: fall back to just reading the target value.
                        return self.lower_expr(target);
                    }
                };
                let name_val = self.fb.const_string(&name);
                let two_way_val = self.fb.const_bool(*two_way);
                // Determine if it's a story or temp var.
                let is_story = matches!(&target.kind, ExprKind::StoryVar(_));
                let is_story_val = self.fb.const_bool(is_story);
                self.fb.system_call(
                    "Harlowe.State",
                    "bind_ref",
                    &[name_val, is_story_val, two_way_val],
                    Type::Dynamic,
                )
            }
            ExprKind::Error(_) => self.fb.const_bool(false),
        }
    }

    fn lower_ordinal(&mut self, ord: &Ordinal) -> ValueId {
        match ord {
            // 1-based forward: runtime does `obj[prop - 1]`
            Ordinal::Nth(n) => self.fb.const_int(*n as i64),
            // Reverse: runtime does `obj[obj.length + prop]` (prop is negative)
            Ordinal::NthLast(n) => self.fb.const_int(-(*n as i64)),
            // last = -1
            Ordinal::Last => self.fb.const_int(-1),
            Ordinal::Length => self.fb.const_string("length"),
            Ordinal::Range { from, to } => {
                let from_val = self.lower_range_end(from);
                let to_val = self.lower_range_end(to);
                self.fb.system_call(
                    "Harlowe.Engine",
                    "make_range",
                    &[from_val, to_val],
                    Type::Dynamic,
                )
            }
        }
    }

    fn lower_range_end(&mut self, end: &RangeEnd) -> ValueId {
        match end {
            RangeEnd::Nth(n) => self.fb.const_int(*n as i64),
            RangeEnd::NthLast(n) => self.fb.const_int(-(*n as i64)),
            RangeEnd::Last => self.fb.const_int(-1),
        }
    }

    fn lower_binary(&mut self, op: &BinaryOp, left: &Expr, right: &Expr) -> ValueId {
        match op {
            BinaryOp::And => return self.lower_logical_and(left, right),
            BinaryOp::Or => return self.lower_logical_or(left, right),
            // `is odd` / `is even` — Harlowe datatype-check shorthand (no article).
            // These identifiers become string constants when lowered normally, producing
            // `value === "odd"` which TypeScript correctly flags as a type error.
            BinaryOp::Is | BinaryOp::IsNot
                if matches!(&right.kind, ExprKind::Ident(n) if n == "odd" || n == "even") =>
            {
                let lhs = self.lower_expr(left);
                let rhs = self.lower_expr(right);
                let is_a =
                    self.fb
                        .system_call("Harlowe.Engine", "is_a", &[lhs, rhs], Type::Bool);
                return if matches!(op, BinaryOp::Is) { is_a } else { self.fb.not(is_a) };
            }
            _ => {}
        }

        let lhs = self.lower_expr(left);
        let rhs = self.lower_expr(right);

        match op {
            BinaryOp::Add | BinaryOp::Plus => {
                self.fb
                    .system_call("Harlowe.Engine", "plus", &[lhs, rhs], Type::Dynamic)
            }
            BinaryOp::Sub => {
                self.fb
                    .system_call("Harlowe.Engine", "minus", &[lhs, rhs], Type::Dynamic)
            }
            BinaryOp::Mul => self.fb.mul(lhs, rhs),
            BinaryOp::Div => self.fb.div(lhs, rhs),
            BinaryOp::Mod => self.fb.rem(lhs, rhs),
            BinaryOp::Is => self.fb.cmp(CmpKind::Eq, lhs, rhs),
            BinaryOp::IsNot => self.fb.cmp(CmpKind::Ne, lhs, rhs),
            BinaryOp::Lt => self.fb.cmp(CmpKind::Lt, lhs, rhs),
            BinaryOp::Lte => self.fb.cmp(CmpKind::Le, lhs, rhs),
            BinaryOp::Gt => self.fb.cmp(CmpKind::Gt, lhs, rhs),
            BinaryOp::Gte => self.fb.cmp(CmpKind::Ge, lhs, rhs),
            BinaryOp::Contains => {
                self.fb
                    .system_call("Harlowe.Engine", "contains", &[lhs, rhs], Type::Bool)
            }
            BinaryOp::IsIn => {
                self.fb
                    .system_call("Harlowe.Engine", "is_in", &[lhs, rhs], Type::Bool)
            }
            BinaryOp::IsNotIn => {
                let is_in =
                    self.fb
                        .system_call("Harlowe.Engine", "is_in", &[lhs, rhs], Type::Bool);
                self.fb.not(is_in)
            }
            BinaryOp::IsA => {
                self.fb
                    .system_call("Harlowe.Engine", "is_a", &[lhs, rhs], Type::Bool)
            }
            BinaryOp::IsNotA => {
                let is_a =
                    self.fb
                        .system_call("Harlowe.Engine", "is_a", &[lhs, rhs], Type::Bool);
                self.fb.not(is_a)
            }
            BinaryOp::And | BinaryOp::Or => unreachable!(),
        }
    }

    fn lower_unary(&mut self, op: &UnaryOp, operand: &Expr) -> ValueId {
        let val = self.lower_expr(operand);
        match op {
            UnaryOp::Not => {
                self.fb
                    .system_call("Harlowe.Engine", "not", &[val], Type::Bool)
            }
            UnaryOp::Neg => {
                let zero = self.fb.const_float(0.0);
                self.fb.sub(zero, val)
            }
        }
    }

    fn lower_logical_and(&mut self, left: &Expr, right: &Expr) -> ValueId {
        let lhs = self.lower_expr(left);

        let rhs_block = self.fb.create_block();
        let merge_block = self.fb.create_block();
        let merge_params = self.fb.add_block_params(merge_block, &[Type::Dynamic]);
        let merge_param = merge_params[0];

        self.fb
            .br_if(lhs, rhs_block, &[], merge_block, &[lhs]);

        self.fb.switch_to_block(rhs_block);
        let rhs = self.lower_expr(right);
        self.fb.br(merge_block, &[rhs]);

        self.fb.switch_to_block(merge_block);
        merge_param
    }

    fn lower_logical_or(&mut self, left: &Expr, right: &Expr) -> ValueId {
        let lhs = self.lower_expr(left);

        let rhs_block = self.fb.create_block();
        let merge_block = self.fb.create_block();
        let merge_params = self.fb.add_block_params(merge_block, &[Type::Dynamic]);
        let merge_param = merge_params[0];

        self.fb
            .br_if(lhs, merge_block, &[lhs], rhs_block, &[]);

        self.fb.switch_to_block(rhs_block);
        let rhs = self.lower_expr(right);
        self.fb.br(merge_block, &[rhs]);

        self.fb.switch_to_block(merge_block);
        merge_param
    }

    // ── Inline macro calls in expressions ──────────────────────────

    fn lower_call(&mut self, name: &str, args: &[Expr]) -> ValueId {
        // Predicate collection ops: lower items FIRST to infer element type, then
        // build the lambda callback with the inferred param type.
        // This is IR-level type inference — the lambda's FunctionSig gets the right
        // param type regardless of backend.
        match name {
            "find" | "some-pass" | "all-pass" | "none-pass" | "count" => {
                return self.lower_predicate_collection_op(name, args, Type::Bool);
            }
            "altered" | "sorted-by" => {
                return self.lower_predicate_collection_op(name, args, Type::Dynamic);
            }
            _ => {}
        }

        let lowered_args: Vec<ValueId> = args.iter().map(|a| self.lower_expr(a)).collect();

        match name {
            "random" => {
                self.fb
                    .system_call("Harlowe.Engine", "random", &lowered_args, Type::Dynamic)
            }
            "either" => {
                self.fb
                    .system_call("Harlowe.Engine", "either", &lowered_args, Type::Dynamic)
            }
            "str" | "string" => {
                self.fb
                    .system_call("Harlowe.Engine", "str", &lowered_args, Type::Dynamic)
            }
            "num" | "number" => {
                self.fb
                    .system_call("Harlowe.Engine", "num", &lowered_args, Type::Dynamic)
            }
            "a" | "array" => {
                self.fb
                    .system_call("Harlowe.Engine", "array", &lowered_args, Type::Dynamic)
            }
            "dm" | "datamap" => {
                self.fb
                    .system_call("Harlowe.Engine", "datamap", &lowered_args, Type::Dynamic)
            }
            "ds" | "dataset" => {
                self.fb
                    .system_call("Harlowe.Engine", "dataset", &lowered_args, Type::Dynamic)
            }
            "round" | "floor" | "ceil" | "abs" | "min" | "max" | "sqrt" | "sin" | "cos"
            | "tan" | "log" | "log10" | "log2" | "exp" | "pow" | "sign" | "clamp" | "lerp"
            | "trunc" => {
                let n = self.fb.const_string(name);
                let mut call_args = vec![n];
                call_args.extend(lowered_args);
                self.fb.system_call(
                    "Harlowe.Engine",
                    "math",
                    &call_args,
                    Type::Dynamic,
                )
            }
            "upperfirst" | "lowerfirst" | "str-reversed" | "string-reversed"
            | "trimmed" | "words" | "str-nth" | "string-nth"
            | "str-repeated" | "string-repeated"
            | "str-find" | "string-find"
            | "str-replaced" | "string-replaced" | "replaced"
            | "digit-format" | "plural" => {
                let n = self.fb.const_string(name);
                let mut call_args = vec![n];
                call_args.extend(lowered_args);
                self.fb.system_call("Harlowe.Engine", "str_op", &call_args, Type::Dynamic)
            }
            "sorted" | "reversed" | "rotated" | "shuffled" | "range" | "folded"
            | "interlaced" | "repeated" | "joined" | "subarray" | "substring" | "lowercase"
            | "uppercase" | "datanames" | "datavalues" | "dataentries" => {
                let n = self.fb.const_string(name);
                let mut call_args = vec![n];
                call_args.extend(lowered_args);
                self.fb.system_call(
                    "Harlowe.Engine",
                    "collection_op",
                    &call_args,
                    Type::Dynamic,
                )
            }
            "open-storylets" | "storylets-of" => {
                self.fb.system_call(
                    "Harlowe.Engine",
                    "open_storylets",
                    &lowered_args,
                    Type::Dynamic,
                )
            }
            "saved-games" => {
                self.fb.system_call(
                    "Harlowe.Engine",
                    "saved_games",
                    &lowered_args,
                    Type::Dynamic,
                )
            }
            "passage" => {
                self.fb.system_call(
                    "Harlowe.Engine",
                    "current_passage",
                    &lowered_args,
                    Type::Dynamic,
                )
            }
            "visits" | "turns" | "time" | "history" => {
                let n = self.fb.const_string(name);
                self.fb
                    .system_call("Harlowe.Engine", "meta", &[n], Type::Dynamic)
            }
            "rgb" | "rgba" | "hsl" | "hsla" | "lch" | "lcha" | "complement" | "mix" => {
                let n = self.fb.const_string(name);
                let mut call_args = vec![n];
                call_args.extend(lowered_args);
                self.fb.system_call(
                    "Harlowe.Engine",
                    "color_op",
                    &call_args,
                    Type::Dynamic,
                )
            }
            // `(macro:)` without a hook body in expression position — should not happen
            // if the expression parser correctly captures `[body]` via `ExprKind::MacroDef`.
            // Emit a no-op closure that returns false as a safe fallback.
            "macro" => self.fb.const_bool(false),
            name if macros::macro_kind(name) == MacroKind::Changer => {
                let n = self.fb.const_string(name);
                let mut call_args = vec![n];
                call_args.extend(lowered_args);
                self.fb.system_call(
                    "Harlowe.Engine",
                    "create_changer",
                    &call_args,
                    Type::Dynamic,
                )
            }
            _ => {
                let n = self.fb.const_string(name);
                let mut call_args = vec![n];
                call_args.extend(lowered_args);
                self.fb.system_call(
                    "Harlowe.Engine",
                    "call",
                    &call_args,
                    Type::Dynamic,
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::harlowe::parser;

    #[test]
    fn test_plain_text_emits_h_text() {
        let ast = parser::parse("Hello world");
        let result = translate_passage("test", &ast, "");
        assert_eq!(result.func.name, "passage_test");
        // Passage takes h param (Dynamic), returns void
        assert_eq!(result.func.sig.params.len(), 1);
        assert_eq!(result.func.sig.params[0], Type::Dynamic);
        assert_eq!(result.func.sig.return_ty, Type::Void);
    }

    #[test]
    fn test_h_text_syscall() {
        use reincarnate_core::ir::inst::Op;
        let ast = parser::parse("Hello");
        let result = translate_passage("test_text", &ast, "");
        let func = &result.func;
        let has_h_text = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.H" && method == "text")
            })
        });
        assert!(has_h_text, "should have Harlowe.H.text call");
    }

    #[test]
    fn test_set_produces_state_set() {
        let ast = parser::parse("(set: $x to 1)");
        let result = translate_passage("test_set", &ast, "");
        assert_eq!(result.func.name, "passage_test_set");
    }

    #[test]
    fn test_if_creates_blocks() {
        let ast = parser::parse("(if: $x is 1)[yes](else:)[no]");
        let result = translate_passage("test_if", &ast, "");
        // Should have multiple blocks for the if/else
        assert!(result.func.blocks.len() >= 3);
    }

    #[test]
    fn test_link_produces_syscall() {
        use reincarnate_core::ir::inst::Op;
        let ast = parser::parse("[[Start->Begin]]");
        let result = translate_passage("test_link", &ast, "");
        let func = &result.func;
        let has_h_link = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.H" && method == "link")
            })
        });
        assert!(has_h_link, "should have Harlowe.H.link call");
    }

    #[test]
    fn test_link_macro_creates_callback() {
        let ast = parser::parse("(link: \"Continue\")[(goto: \"Next\")]");
        let result = translate_passage("test_link_macro", &ast, "");
        assert_eq!(result.callbacks.len(), 1);
        // Callback takes h param (Dynamic), returns void
        assert_eq!(result.callbacks[0].sig.params.len(), 1);
        assert_eq!(result.callbacks[0].sig.params[0], Type::Dynamic);
        assert_eq!(result.callbacks[0].sig.return_ty, Type::Void);
    }

    #[test]
    fn test_goto_produces_navigation() {
        let ast = parser::parse("(goto: \"Some Passage\")");
        let result = translate_passage("test_goto", &ast, "");
        assert_eq!(result.func.name, "passage_test_goto");
    }

    #[test]
    fn test_color_changer() {
        use reincarnate_core::ir::inst::Op;
        let ast = parser::parse("(color: green)[Hello]");
        let result = translate_passage("test_color", &ast, "");
        let func = &result.func;
        let has_h_color = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.H" && method == "color")
            })
        });
        assert!(has_h_color, "should have Harlowe.H.color call");
    }

    #[test]
    fn test_passage_func_name_sanitization() {
        assert_eq!(passage_func_name("Room 1 entry"), "passage_Room_1_entry");
        assert_eq!(passage_func_name("Scene 3-check"), "passage_Scene_3_check");
    }

    #[test]
    fn test_complex_passage() {
        let src = r#"(set: $location to "the plaza")
You're at the **plaza**

(if: $fearStat < 70)[Normal text.](else:)[(color: magenta+white)[Spooked text.]]"#;
        let ast = parser::parse(src);
        let result = translate_passage("complex", &ast, src);
        assert!(!result.func.blocks.is_empty());
    }

    #[test]
    fn test_live_creates_callback() {
        let ast = parser::parse("(live: 2s)[(stop:)]");
        let result = translate_passage("test_live", &ast, "");
        assert_eq!(result.callbacks.len(), 1);
        // Callback takes h param (Dynamic)
        assert_eq!(result.callbacks[0].sig.params.len(), 1);
        assert_eq!(result.callbacks[0].sig.params[0], Type::Dynamic);
    }

    #[test]
    fn test_live_emits_syscall_with_interval() {
        use reincarnate_core::ir::inst::Op;
        let ast = parser::parse("(live: 2s)[hello]");
        let result = translate_passage("test_live_syscall", &ast, "");
        let func = &result.func;
        let has_live = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, args, .. }
                    if system == "Harlowe.H" && method == "live" && args.len() == 2)
            })
        });
        assert!(has_live, "should emit Harlowe.H.live(interval, callback)");
    }

    #[test]
    fn test_stop_emits_request_stop_in_callback() {
        use reincarnate_core::ir::inst::Op;
        let ast = parser::parse("(live: 1s)[(stop:)]");
        let result = translate_passage("test_stop_syscall", &ast, "");
        // (stop:) must appear inside the callback, not the main function
        assert_eq!(result.callbacks.len(), 1);
        let cb = &result.callbacks[0];
        let has_stop = cb.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&cb.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.H" && method == "requestStop")
            })
        });
        assert!(has_stop, "requestStop should be emitted inside the live callback");
    }

    #[test]
    fn test_live_without_hook_produces_no_callback() {
        let ast = parser::parse("(live: 1s)");
        let result = translate_passage("test_live_no_hook", &ast, "");
        assert_eq!(
            result.callbacks.len(),
            0,
            "(live:) without a hook should produce no callback"
        );
    }

    #[test]
    fn test_changer_plus_uses_engine_plus_syscall() {
        use reincarnate_core::ir::inst::Op;
        // (color: red) + (text-style: "bold") must route through Harlowe.Engine.plus(),
        // not JS `+`. Changer composition is type-dispatched at runtime.
        let ast = parser::parse("(set: $ch to (color: red) + (text-style: \"bold\"))");
        let result = translate_passage("test_changer_plus", &ast, "");
        let func = &result.func;
        let has_plus = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.Engine" && method == "plus")
            })
        });
        assert!(
            has_plus,
            "changer + changer should emit Harlowe.Engine.plus(), not raw JS +"
        );
    }

    #[test]
    fn test_it_in_set_reads_target_variable() {
        use reincarnate_core::ir::inst::Op;
        let ast = parser::parse("(set: $x to it + 1)");
        let result = translate_passage("test_it_set", &ast, "");
        let func = &result.func;
        let has_get_it = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.State" && method == "get_it")
            })
        });
        assert!(!has_get_it, "should not use get_it() inside (set:)");
        let has_get_x = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.State" && method == "get")
            })
        });
        assert!(has_get_x, "should read target variable with get()");
    }

    #[test]
    fn test_lambda_in_find_builds_callback() {
        use reincarnate_core::ir::inst::Op;
        // (find: each _x where _x > 5, ...$arr) should produce:
        // - a lambda callback: Dynamic param (since $arr is Dynamic, elem type unknown)
        //   but Bool return type (inferred because `find` is a predicate op)
        // - a collection_op("find", ...) syscall in the main func
        let ast = parser::parse("(set: $found to (find: each _x where _x > 5, ...$arr))");
        let result = translate_passage("test_lambda_find", &ast, "");
        // Exactly one callback: the lambda predicate
        assert_eq!(
            result.callbacks.len(),
            1,
            "should produce exactly one lambda callback"
        );
        let cb = &result.callbacks[0];
        assert_eq!(cb.sig.params.len(), 2, "lambda callback takes (item, pos) params");
        assert_eq!(cb.sig.params[0], Type::Dynamic);
        assert_eq!(cb.sig.params[1], Type::Int(32), "second param is 1-based pos");
        assert_eq!(cb.sig.return_ty, Type::Bool);
        // Main func should have a collection_op("find", ...) call
        let func = &result.func;
        let has_find = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.Engine" && method == "collection_op")
            })
        });
        assert!(has_find, "should have Harlowe.Engine.collection_op call");
    }

    #[test]
    fn test_sorted_by_produces_callback() {
        use reincarnate_core::ir::inst::Op;
        // (sorted-by: each _x via _x, 3, 1, 2) — produces a collection_op call with a callback
        let ast = parser::parse("(set: $r to (sorted-by: each _x via _x, 3, 1, 2))");
        let result = translate_passage("test_sorted_by", &ast, "");
        // Should produce at least one callback (the via lambda)
        assert!(
            !result.callbacks.is_empty(),
            "sorted-by should produce a lambda callback"
        );
        // Should have a collection_op syscall
        let all_ops: Vec<_> = result
            .func
            .blocks
            .values()
            .flat_map(|b| b.insts.iter().map(|&id| &result.func.insts[id].op))
            .collect();
        let has_collection_op = all_ops.iter().any(|op| {
            matches!(op, Op::SystemCall { system, method, .. }
                if system == "Harlowe.Engine" && method == "collection_op")
        });
        assert!(has_collection_op, "sorted-by should emit collection_op call");
    }

    #[test]
    fn test_folded_produces_two_param_callback() {
        // (folded: each _x making _acc via _x + _acc, 0, 1, 2, 3)
        // The fold callback takes two params (item, acc)
        let ast = parser::parse(
            "(set: $r to (folded: each _x making _acc via _x + _acc, 0, 1, 2, 3))",
        );
        let result = translate_passage("test_folded", &ast, "");
        assert!(
            !result.callbacks.is_empty(),
            "folded should produce a fold callback"
        );
        let cb = &result.callbacks[0];
        assert_eq!(cb.sig.params.len(), 2, "fold callback should take 2 params (item, acc)");
    }

    #[test]
    fn test_interlaced_repeated_emit_collection_op() {
        use reincarnate_core::ir::inst::Op;
        for (macro_call, label) in [
            ("(set: $r to (interlaced: (a: 1, 2), (a: 3, 4)))", "interlaced"),
            ("(set: $r to (repeated: 3, \"x\"))", "repeated"),
        ] {
            let ast = parser::parse(macro_call);
            let result = translate_passage(label, &ast, "");
            let has_collection_op = result.func.blocks.values().any(|b| {
                b.insts.iter().any(|&id| {
                    matches!(&result.func.insts[id].op, Op::SystemCall { system, method, .. }
                        if system == "Harlowe.Engine" && method == "collection_op")
                })
            });
            assert!(has_collection_op, "{label} should emit collection_op call");
        }
    }

    #[test]
    fn test_no_content_array_in_ir() {
        use reincarnate_core::ir::inst::Op;
        let ast = parser::parse("Hello **world**");
        let result = translate_passage("test_no_array", &ast, "");
        let func = &result.func;
        // Should NOT have content_array, new_buffer, push, text_node
        let has_old_output = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.Output"
                       && (method == "content_array" || method == "new_buffer"
                           || method == "push" || method == "text_node"))
            })
        });
        assert!(!has_old_output, "should not have any Harlowe.Output array calls");
    }

    #[test]
    fn test_changer_chain_in_content_composed_via_plus() {
        use reincarnate_core::ir::inst::Op;
        // (align: "=><=")+(color: red)[content] in passage content:
        // the two changers must be composed via Harlowe.Engine.plus() and the result
        // applied to a styled() call. The "+" between them is changer composition, not text.
        let ast = parser::parse("(align: \"=><=\")+(color: red)[hello]");
        let result = translate_passage("test_chain", &ast, "");
        let func = &result.func;
        let has_plus = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.Engine" && method == "plus")
            })
        });
        assert!(
            has_plus,
            "(ch1)+(ch2)[hook] in content should compose via Harlowe.Engine.plus()"
        );
        // The composed changer must be applied to the hook via styled().
        let has_styled = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.H" && method == "styled")
            })
        });
        assert!(has_styled, "composed changer must be applied via h.styled()");
    }
}
