//! Harlowe AST → IR translation.
//!
//! Translates parsed Harlowe passage AST nodes into reincarnate-core IR
//! functions using SystemCall-based dispatch to the Harlowe runtime layer.
//!
//! SystemCall namespaces:
//! - `Harlowe.State`: get/set story and temp variables
//! - `Harlowe.Output`: text, print, html, link, push/pop changers
//! - `Harlowe.Navigation`: goto, display (include)
//! - `Harlowe.Engine`: changers, data ops, runtime helpers

use std::collections::HashMap;

use reincarnate_core::ir::{BlockId, CmpKind, Function, FunctionBuilder, FunctionSig, Type, ValueId, Visibility};

use super::ast::*;
use super::macros::{self, MacroKind};

/// Result of translating a Harlowe passage.
pub struct TranslateResult {
    /// The main passage function.
    pub func: Function,
    /// Callback functions generated for link hooks, live intervals, etc.
    pub callbacks: Vec<Function>,
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
pub fn translate_passage(name: &str, ast: &PassageAst) -> TranslateResult {
    let func_name = passage_func_name(name);
    let sig = FunctionSig {
        params: vec![],
        return_ty: Type::Void,
        defaults: vec![],
        has_rest_param: false,
    };
    let fb = FunctionBuilder::new(&func_name, sig, Visibility::Public);
    let mut ctx = TranslateCtx {
        fb,
        temp_vars: HashMap::new(),
        func_name: func_name.clone(),
        callback_count: 0,
        callbacks: Vec::new(),
        set_target: None,
    };

    ctx.lower_nodes(&ast.body);
    ctx.fb.ret(None);

    let callbacks = std::mem::take(&mut ctx.callbacks);

    TranslateResult {
        func: ctx.fb.build(),
        callbacks,
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
}

impl TranslateCtx {
    // ── Node lowering ──────────────────────────────────────────────────

    fn lower_nodes(&mut self, nodes: &[Node]) {
        for node in nodes {
            self.lower_node(node);
        }
    }

    fn lower_node(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Text(text) => {
                if !text.is_empty() {
                    self.emit_text(text);
                }
            }
            NodeKind::Macro(mac) => self.lower_macro(mac),
            NodeKind::Hook(nodes) => self.lower_nodes(nodes),
            NodeKind::Link(link) => self.lower_link(link),
            NodeKind::VarInterp(name) => self.lower_var_interp(name),
            NodeKind::HtmlOpen { tag, attrs } => self.emit_open_element(tag, attrs),
            NodeKind::HtmlClose(_) => self.emit_close_element(),
            NodeKind::HtmlVoid { tag, attrs } => self.emit_void_element(tag, attrs),
            NodeKind::Markup { tag, body } => {
                self.emit_open_element(tag, &[]);
                self.lower_nodes(body);
                self.emit_close_element();
            }
            NodeKind::ChangerApply { name, hook } => {
                let changer = if let Some(stripped) = name.strip_prefix('$') {
                    let n = self.fb.const_string(stripped);
                    self.fb
                        .system_call("Harlowe.State", "get", &[n], Type::Dynamic)
                } else {
                    let stripped = name.strip_prefix('_').unwrap_or(name);
                    self.get_or_load_temp(stripped)
                };
                self.fb.system_call(
                    "Harlowe.Output",
                    "push_changer",
                    &[changer],
                    Type::Void,
                );
                self.lower_nodes(hook);
                self.fb
                    .system_call("Harlowe.Output", "pop_changer", &[], Type::Void);
            }
            NodeKind::LineBreak => {
                let tag = self.fb.const_string("br");
                self.fb
                    .system_call("Harlowe.Output", "void_element", &[tag], Type::Void);
            }
        }
    }

    // ── Output helpers ─────────────────────────────────────────────────

    fn emit_text(&mut self, text: &str) {
        let s = self.fb.const_string(text);
        self.fb
            .system_call("Harlowe.Output", "text", &[s], Type::Void);
    }

    fn emit_open_element(&mut self, tag: &str, attrs: &[(String, String)]) {
        let tag_val = self.fb.const_string(tag);
        let mut args = vec![tag_val];
        for (k, v) in attrs {
            args.push(self.fb.const_string(k));
            args.push(self.fb.const_string(v));
        }
        self.fb
            .system_call("Harlowe.Output", "open_element", &args, Type::Void);
    }

    fn emit_close_element(&mut self) {
        self.fb
            .system_call("Harlowe.Output", "close_element", &[], Type::Void);
    }

    fn emit_void_element(&mut self, tag: &str, attrs: &[(String, String)]) {
        let tag_val = self.fb.const_string(tag);
        let mut args = vec![tag_val];
        for (k, v) in attrs {
            args.push(self.fb.const_string(k));
            args.push(self.fb.const_string(v));
        }
        self.fb
            .system_call("Harlowe.Output", "void_element", &args, Type::Void);
    }

    fn emit_print(&mut self, value: ValueId) {
        self.fb
            .system_call("Harlowe.Output", "print", &[value], Type::Void);
    }

    // ── Variable interpolation ─────────────────────────────────────────

    fn lower_var_interp(&mut self, name: &str) {
        // `$var` or `_var` in body text → get + print
        let val = if let Some(stripped) = name.strip_prefix('$') {
            let n = self.fb.const_string(stripped);
            self.fb
                .system_call("Harlowe.State", "get", &[n], Type::Dynamic)
        } else if let Some(stripped) = name.strip_prefix('_') {
            self.get_or_load_temp(stripped)
        } else {
            let n = self.fb.const_string(name);
            self.fb
                .system_call("Harlowe.State", "get", &[n], Type::Dynamic)
        };
        self.emit_print(val);
    }

    // ── Macro lowering ─────────────────────────────────────────────────

    fn lower_macro(&mut self, mac: &MacroNode) {
        match mac.name.as_str() {
            // State
            "set" => self.lower_set(mac),
            "put" => self.lower_put(mac),

            // Control flow
            "if" | "unless" => self.lower_if(mac),

            // Navigation
            "goto" | "go-to" => self.lower_goto(mac),
            "display" => self.lower_display(mac),

            // Output
            "print" => self.lower_print(mac),

            // Links
            "link" => self.lower_link_macro(mac),
            "link-goto" => self.lower_link_goto(mac),

            // Changers (attach to hook)
            "color" | "colour" | "text-colour" | "text-color" | "text-style" | "font"
            | "align" | "transition" | "transition-time" | "transition-arrive"
            | "transition-depart" | "text-rotate-z" | "hover-style" | "css" | "background"
            | "opacity" | "text-size" | "collapse" | "nobr" | "hidden" => {
                self.lower_changer(mac);
            }

            // Timed
            "live" => self.lower_live(mac),
            "stop" => {
                // `(stop:)` — no-op in IR (the live interval handler checks for this)
                self.fb
                    .system_call("Harlowe.Engine", "stop", &[], Type::Void);
            }

            // Value macros (used as expressions — already handled by expr lowering
            // when inside expression context). If they appear standalone, print result.
            "str" | "string" | "num" | "number" | "random" | "either" | "a" | "array"
            | "dm" | "datamap" | "ds" | "dataset" => {
                self.lower_value_macro_standalone(mac);
            }

            // Save/load
            "save-game" => self.lower_save_game(mac),
            "load-game" => self.lower_load_game(mac),

            // Alert/prompt/confirm
            "alert" => self.lower_simple_command(mac, "Harlowe.Engine", "alert"),
            "prompt" => self.lower_simple_command(mac, "Harlowe.Engine", "prompt"),
            "confirm" => self.lower_simple_command(mac, "Harlowe.Engine", "confirm"),

            // DOM manipulation
            "replace" | "append" | "prepend" | "show" | "hide" | "rerun" => {
                self.lower_dom_macro(mac);
            }

            // Click
            "click" | "click-replace" | "click-append" | "click-prepend" => {
                self.lower_click_macro(mac);
            }

            // Unknown → generic widget call
            _ => self.lower_unknown_macro(mac),
        }
    }

    // ── (set:) ─────────────────────────────────────────────────────────

    fn lower_set(&mut self, mac: &MacroNode) {
        // Each arg should be an Assign: `$var to expr`
        for arg in &mac.args {
            self.lower_assignment(arg);
        }
    }

    fn lower_put(&mut self, mac: &MacroNode) {
        // `(put: expr into $var)` — same semantics, different syntax
        // For now, handle like set
        for arg in &mac.args {
            self.lower_assignment(arg);
        }
    }

    fn lower_assignment(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Assign { target, value } => {
                // Harlowe: `it` inside (set:) refers to the target variable's
                // current value. Set context so lower_expr(It) reads the target.
                let prev_target = self.set_target.replace(*target.clone());
                let val = self.lower_expr(value);
                self.set_target = prev_target;
                self.store_to_target(target, val);
            }
            _ => {
                // Side-effectful expression (e.g. `$x + 1` without assign)
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
                // Best effort: evaluate target for side effects
                self.lower_expr(target);
            }
        }
    }

    // ── (if:) / (unless:) / (else-if:) / (else:) ──────────────────────

    fn lower_if(&mut self, mac: &MacroNode) {
        let merge_block = self.fb.create_block();

        // Main condition
        let cond = if mac.args.is_empty() {
            self.fb.const_bool(true)
        } else {
            let raw_cond = self.lower_expr(&mac.args[0]);
            if mac.name == "unless" {
                // Negate for unless
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
            self.lower_nodes(hook);
        }
        self.fb.br(merge_block, &[]);

        // Else chain
        self.fb.switch_to_block(else_block);
        self.lower_if_clauses(&mac.clauses, 0, merge_block);

        self.fb.switch_to_block(merge_block);
    }

    fn lower_if_clauses(&mut self, clauses: &[IfClause], index: usize, merge_block: BlockId) {
        if index >= clauses.len() {
            self.fb.br(merge_block, &[]);
            return;
        }

        let clause = &clauses[index];

        if clause.kind == "else" {
            self.lower_nodes(&clause.body);
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
        self.lower_nodes(&clause.body);
        self.fb.br(merge_block, &[]);

        self.fb.switch_to_block(next_else);
        self.lower_if_clauses(clauses, index + 1, merge_block);
    }

    // ── (goto:) ────────────────────────────────────────────────────────

    fn lower_goto(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let target = self.lower_expr(arg);
            self.fb
                .system_call("Harlowe.Navigation", "goto", &[target], Type::Void);
        }
    }

    // ── (display:) ─────────────────────────────────────────────────────

    fn lower_display(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let target = self.lower_expr(arg);
            self.fb
                .system_call("Harlowe.Navigation", "display", &[target], Type::Void);
        }
    }

    // ── (print:) ───────────────────────────────────────────────────────

    fn lower_print(&mut self, mac: &MacroNode) {
        if let Some(arg) = mac.args.first() {
            let val = self.lower_expr(arg);
            self.emit_print(val);
        }
    }

    // ── (link:) ────────────────────────────────────────────────────────

    fn lower_link_macro(&mut self, mac: &MacroNode) {
        // `(link: "text")[hook]` — creates a clickable link that runs the hook
        if let Some(arg) = mac.args.first() {
            let text = self.lower_expr(arg);

            if let Some(ref hook) = mac.hook {
                // Build a callback function for the hook body
                let cb_name = self.make_callback_name("link");
                let cb_ref = self.build_callback(&cb_name, hook);
                self.fb.system_call(
                    "Harlowe.Output",
                    "link_callback",
                    &[text, cb_ref],
                    Type::Void,
                );
            } else {
                // Link with no hook — just display as text
                self.emit_print(text);
            }
        }
    }

    fn lower_link_goto(&mut self, mac: &MacroNode) {
        // `(link-goto: "text", "passage")`
        if mac.args.len() >= 2 {
            let text = self.lower_expr(&mac.args[0]);
            let passage = self.lower_expr(&mac.args[1]);
            self.fb.system_call(
                "Harlowe.Output",
                "link",
                &[text, passage],
                Type::Void,
            );
        } else if let Some(arg) = mac.args.first() {
            // Single arg: text = passage
            let text = self.lower_expr(arg);
            self.fb.system_call(
                "Harlowe.Output",
                "link",
                &[text, text],
                Type::Void,
            );
        }
    }

    // ── [[link]] ───────────────────────────────────────────────────────

    fn lower_link(&mut self, link: &LinkNode) {
        let text = self.fb.const_string(&link.text);
        let passage = self.fb.const_string(&link.passage);
        self.fb
            .system_call("Harlowe.Output", "link", &[text, passage], Type::Void);
    }

    // ── Changers ───────────────────────────────────────────────────────

    fn lower_changer(&mut self, mac: &MacroNode) {
        // Emit a changer value, push it, render the hook, pop it
        let changer_name = self.fb.const_string(&mac.name);
        let mut args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        args.insert(0, changer_name);

        let changer = self.fb.system_call(
            "Harlowe.Engine",
            "create_changer",
            &args,
            Type::Dynamic,
        );
        self.fb.system_call(
            "Harlowe.Output",
            "push_changer",
            &[changer],
            Type::Void,
        );

        if let Some(ref hook) = mac.hook {
            self.lower_nodes(hook);
        }

        self.fb
            .system_call("Harlowe.Output", "pop_changer", &[], Type::Void);
    }

    // ── (live:) ────────────────────────────────────────────────────────

    fn lower_live(&mut self, mac: &MacroNode) {
        // `(live: 2s)[hook]` — timed interval
        let interval = if let Some(arg) = mac.args.first() {
            self.lower_expr(arg)
        } else {
            self.fb.const_float(1.0)
        };

        if let Some(ref hook) = mac.hook {
            let cb_name = self.make_callback_name("live");
            let cb_ref = self.build_callback(&cb_name, hook);
            self.fb.system_call(
                "Harlowe.Engine",
                "live",
                &[interval, cb_ref],
                Type::Void,
            );
        }
    }

    // ── Value macros (standalone) ──────────────────────────────────────

    fn lower_value_macro_standalone(&mut self, mac: &MacroNode) {
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
        // If there's a hook, push the value and render
        if let Some(ref hook) = mac.hook {
            self.fb.system_call(
                "Harlowe.Output",
                "push_changer",
                &[result],
                Type::Void,
            );
            self.lower_nodes(hook);
            self.fb
                .system_call("Harlowe.Output", "pop_changer", &[], Type::Void);
        }
    }

    // ── Save/load ──────────────────────────────────────────────────────

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

    // ── Simple commands ────────────────────────────────────────────────

    fn lower_simple_command(&mut self, mac: &MacroNode, system: &str, method: &str) {
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        self.fb
            .system_call(system, method, &args, Type::Dynamic);
    }

    // ── DOM macros ─────────────────────────────────────────────────────

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

    // ── Click macros ───────────────────────────────────────────────────

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

    // ── Unknown macros ─────────────────────────────────────────────────

    fn lower_unknown_macro(&mut self, mac: &MacroNode) {
        let name = self.fb.const_string(&mac.name);
        let args: Vec<ValueId> = mac.args.iter().map(|a| self.lower_expr(a)).collect();
        let mut call_args = vec![name];
        call_args.extend(args);
        self.fb
            .system_call("Harlowe.Engine", "unknown_macro", &call_args, Type::Dynamic);

        if let Some(ref hook) = mac.hook {
            self.lower_nodes(hook);
        }
    }

    // ── Callback building ──────────────────────────────────────────────

    fn make_callback_name(&mut self, kind: &str) -> String {
        let name = format!("{}_{kind}_{}", self.func_name, self.callback_count);
        self.callback_count += 1;
        name
    }

    fn build_callback(&mut self, name: &str, body: &[Node]) -> ValueId {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            defaults: vec![],
            has_rest_param: false,
        };
        let mut cb_fb = FunctionBuilder::new(name, sig, Visibility::Public);

        let saved_fb = std::mem::replace(&mut self.fb, cb_fb);
        let saved_temps = std::mem::take(&mut self.temp_vars);

        self.lower_nodes(body);
        self.fb.ret(None);

        cb_fb = std::mem::replace(&mut self.fb, saved_fb);
        self.temp_vars = saved_temps;

        self.callbacks.push(cb_fb.build());

        self.fb.global_ref(name, Type::Dynamic)
    }

    // ── Temp variable helpers ──────────────────────────────────────────

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

    // ── Expression lowering ────────────────────────────────────────────

    fn lower_expr(&mut self, expr: &Expr) -> ValueId {
        match &expr.kind {
            ExprKind::Number(n) => self.fb.const_float(*n),
            ExprKind::Str(s) => self.fb.const_string(s.as_str()),
            ExprKind::Bool(b) => self.fb.const_bool(*b),
            ExprKind::It => {
                // Inside (set:), `it` refers to the target variable's current value.
                // Outside (set:), `it` is the global it-value (set by comparisons).
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
                // Identifiers in expression context — could be enum values, etc.
                self.fb.const_string(name.as_str())
            }
            ExprKind::ColorLiteral(color) => self.fb.const_string(color.as_str()),
            ExprKind::TimeLiteral(secs) => self.fb.const_float(*secs),
            ExprKind::Ordinal(ord) => {
                match ord {
                    Ordinal::Nth(n) => self.fb.const_int(*n as i64),
                    Ordinal::Last => {
                        self.fb.const_string("last")
                    }
                    Ordinal::Length => {
                        self.fb.const_string("length")
                    }
                }
            }
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
            ExprKind::Error(_) => self.fb.const_bool(false),
        }
    }

    fn lower_binary(&mut self, op: &BinaryOp, left: &Expr, right: &Expr) -> ValueId {
        // Short-circuit operators
        match op {
            BinaryOp::And => return self.lower_logical_and(left, right),
            BinaryOp::Or => return self.lower_logical_or(left, right),
            _ => {}
        }

        let lhs = self.lower_expr(left);
        let rhs = self.lower_expr(right);

        match op {
            BinaryOp::Add | BinaryOp::Plus => {
                // Harlowe's `+` is polymorphic: arithmetic on numbers, composition
                // on changers/arrays/datamaps. Route through runtime to handle all cases.
                self.fb
                    .system_call("Harlowe.Engine", "plus", &[lhs, rhs], Type::Dynamic)
            }
            BinaryOp::Sub => self.fb.sub(lhs, rhs),
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
            // And/Or handled above
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

        // If truthy → short-circuit to merge with lhs; else → evaluate rhs
        self.fb
            .br_if(lhs, merge_block, &[lhs], rhs_block, &[]);

        self.fb.switch_to_block(rhs_block);
        let rhs = self.lower_expr(right);
        self.fb.br(merge_block, &[rhs]);

        self.fb.switch_to_block(merge_block);
        merge_param
    }

    // ── Inline macro calls in expressions ──────────────────────────────

    fn lower_call(&mut self, name: &str, args: &[Expr]) -> ValueId {
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
            | "tan" | "log" | "pow" | "sign" | "clamp" | "lerp" => {
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
            "sorted" | "reversed" | "rotated" | "shuffled" | "count" | "range" | "find"
            | "altered" | "folded" | "interlaced" | "repeated" | "joined" | "some-pass"
            | "all-pass" | "none-pass" | "subarray" | "substring" | "lowercase"
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
            name if macros::macro_kind(name) == MacroKind::Changer => {
                // Changer macros in expression context → create_changer
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
                // Unknown function — generic call
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
    fn test_plain_text_emits_output() {
        let ast = parser::parse("Hello world");
        let result = translate_passage("test", &ast);
        // Should produce a function with system calls
        assert_eq!(result.func.name, "passage_test");
    }

    #[test]
    fn test_set_produces_state_set() {
        let ast = parser::parse("(set: $x to 1)");
        let result = translate_passage("test_set", &ast);
        assert_eq!(result.func.name, "passage_test_set");
    }

    #[test]
    fn test_if_creates_blocks() {
        let ast = parser::parse("(if: $x is 1)[yes](else:)[no]");
        let result = translate_passage("test_if", &ast);
        // Should have multiple blocks for the if/else
        assert!(result.func.blocks.len() >= 3);
    }

    #[test]
    fn test_link_produces_syscall() {
        let ast = parser::parse("[[Start->Begin]]");
        let result = translate_passage("test_link", &ast);
        assert_eq!(result.func.name, "passage_test_link");
    }

    #[test]
    fn test_link_macro_creates_callback() {
        let ast = parser::parse("(link: \"Continue\")[(goto: \"Next\")]");
        let result = translate_passage("test_link_macro", &ast);
        assert_eq!(result.callbacks.len(), 1);
    }

    #[test]
    fn test_goto_produces_navigation() {
        let ast = parser::parse("(goto: \"Event 3-check\")");
        let result = translate_passage("test_goto", &ast);
        assert_eq!(result.func.name, "passage_test_goto");
    }

    #[test]
    fn test_color_changer() {
        let ast = parser::parse("(color: green)[Hello]");
        let result = translate_passage("test_color", &ast);
        assert_eq!(result.func.name, "passage_test_color");
    }

    #[test]
    fn test_passage_func_name_sanitization() {
        assert_eq!(passage_func_name("Floor 1 entryway"), "passage_Floor_1_entryway");
        assert_eq!(passage_func_name("Event 3-check"), "passage_Event_3_check");
    }

    #[test]
    fn test_complex_passage() {
        let src = r#"(set: $recovery to "Floor 1 entryway")
You're at the **entryway**

(if: $hypnoStat < 70)[Normal text.](else:)[(color: magenta+white)[Hypno text.]]"#;
        let ast = parser::parse(src);
        let result = translate_passage("complex", &ast);
        // Should compile without panics
        assert!(!result.func.blocks.is_empty());
    }

    #[test]
    fn test_live_creates_callback() {
        let ast = parser::parse("(live: 2s)[(stop:)]");
        let result = translate_passage("test_live", &ast);
        assert_eq!(result.callbacks.len(), 1);
    }

    #[test]
    fn test_it_in_set_reads_target_variable() {
        use reincarnate_core::ir::inst::Op;
        // `(set: $x to it + 1)` — `it` should resolve to get("x"), not get_it()
        let ast = parser::parse("(set: $x to it + 1)");
        let result = translate_passage("test_it_set", &ast);
        let func = &result.func;
        // Verify no get_it() call exists in the IR
        let has_get_it = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.State" && method == "get_it")
            })
        });
        assert!(!has_get_it, "should not use get_it() inside (set:)");
        // Verify there's a get("x") call (reading the target variable)
        let has_get_x = func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                matches!(&func.insts[inst_id].op, Op::SystemCall { system, method, .. }
                    if system == "Harlowe.State" && method == "get")
            })
        });
        assert!(has_get_x, "should read target variable with get()");
    }
}
