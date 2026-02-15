//! SugarCube AST → reincarnate-core IR translation.
//!
//! Translates parsed SugarCube passage ASTs into IR `Function`s. Each passage
//! becomes a free function (`passage_<name>()`), and widget definitions become
//! separate callable functions.
//!
//! The translation uses SystemCalls for all runtime interactions:
//! - `SugarCube.State` — story variable get/set (`$var`)
//! - `SugarCube.Output` — text/print/html/link/break rendering
//! - `SugarCube.Navigation` — goto/back/return
//! - `SugarCube.Audio` — audio macros
//! - `SugarCube.DOM` — DOM manipulation macros
//! - `SugarCube.Widget` — unknown macro invocation (widget call)
//! - `SugarCube.Engine` — engine operations (eval, clone, etc.)

use std::collections::HashMap;

use reincarnate_core::ir::{
    BlockId, CmpKind, Function, FunctionBuilder, FunctionSig, Type, ValueId, Visibility,
};

use super::ast::*;

/// Translation context for a single passage/widget function.
pub struct TranslateCtx {
    /// The IR function builder.
    pub fb: FunctionBuilder,
    /// Map from temp variable name → alloc ValueId.
    temp_vars: HashMap<String, ValueId>,
    /// Whether we're inside a `<<nobr>>` / `<<silently>>` block.
    suppress_output: bool,
    /// Extracted widget definitions (name → body nodes) accumulated during translation.
    pub widgets: Vec<(String, Vec<Node>)>,
}

impl TranslateCtx {
    pub fn new(name: &str) -> Self {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void,
            defaults: vec![],
            has_rest_param: false,
        };
        let fb = FunctionBuilder::new(name, sig, Visibility::Public);
        Self {
            fb,
            temp_vars: HashMap::new(),
            suppress_output: false,
            widgets: Vec::new(),
        }
    }

    // ── Output helpers ─────────────────────────────────────────────────

    fn emit_text(&mut self, text: &str) {
        if self.suppress_output {
            return;
        }
        let s = self.fb.const_string(text);
        self.fb
            .system_call("SugarCube.Output", "text", &[s], Type::Void);
    }

    fn emit_print(&mut self, value: ValueId) {
        if self.suppress_output {
            return;
        }
        self.fb
            .system_call("SugarCube.Output", "print", &[value], Type::Void);
    }

    fn emit_html(&mut self, html: &str) {
        if self.suppress_output {
            return;
        }
        let s = self.fb.const_string(html);
        self.fb
            .system_call("SugarCube.Output", "html", &[s], Type::Void);
    }

    fn emit_line_break(&mut self) {
        if self.suppress_output {
            return;
        }
        self.fb
            .system_call("SugarCube.Output", "break", &[], Type::Void);
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

    // ── Expression lowering ────────────────────────────────────────────

    pub fn lower_expr(&mut self, expr: &Expr) -> ValueId {
        match &expr.kind {
            ExprKind::Literal(lit) => self.lower_literal(lit),
            ExprKind::Str(s) => self.fb.const_string(s.as_str()),
            ExprKind::Ident(name) => {
                // Bare identifier — could be a global function ref or runtime lookup
                let n = self.fb.const_string(name.as_str());
                self.fb
                    .system_call("SugarCube.Engine", "resolve", &[n], Type::Dynamic)
            }
            ExprKind::StoryVar(name) => {
                let n = self.fb.const_string(name.as_str());
                self.fb
                    .system_call("SugarCube.State", "get", &[n], Type::Dynamic)
            }
            ExprKind::TempVar(name) => {
                let alloc = self.get_or_create_temp(name);
                self.fb.load(alloc, Type::Dynamic)
            }
            ExprKind::Member { object, property } => {
                let obj = self.lower_expr(object);
                self.fb.get_field(obj, property.as_str(), Type::Dynamic)
            }
            ExprKind::Index { object, index } => {
                let obj = self.lower_expr(object);
                let idx = self.lower_expr(index);
                self.fb.get_index(obj, idx, Type::Dynamic)
            }
            ExprKind::Call { callee, args } => {
                let arg_vals: Vec<ValueId> = args.iter().map(|a| self.lower_expr(a)).collect();
                let callee_val = self.lower_expr(callee);
                self.fb
                    .call_indirect(callee_val, &arg_vals, Type::Dynamic)
            }
            ExprKind::New { callee, args } => {
                let callee_val = self.lower_expr(callee);
                let mut all_args = vec![callee_val];
                for a in args {
                    all_args.push(self.lower_expr(a));
                }
                self.fb
                    .system_call("SugarCube.Engine", "new", &all_args, Type::Dynamic)
            }
            ExprKind::Unary { op, operand } => {
                let val = self.lower_expr(operand);
                self.lower_unary_op(*op, val, operand, true)
            }
            ExprKind::Postfix { op, operand } => {
                let val = self.lower_expr(operand);
                self.lower_unary_op(*op, val, operand, false)
            }
            ExprKind::Binary { op, left, right } => {
                self.lower_binary_op(*op, left, right)
            }
            ExprKind::Ternary {
                cond,
                then_expr,
                else_expr,
            } => self.lower_ternary(cond, then_expr, else_expr),
            ExprKind::Assign { op, target, value } => self.lower_assign(*op, target, value),
            ExprKind::Comma(exprs) => {
                // Evaluate all, return last
                let mut last = self.fb.const_null();
                for e in exprs {
                    last = self.lower_expr(e);
                }
                last
            }
            ExprKind::Array(elements) => {
                let vals: Vec<ValueId> = elements.iter().map(|e| self.lower_expr(e)).collect();
                self.fb.array_init(&vals, Type::Dynamic)
            }
            ExprKind::Object(entries) => {
                let fields: Vec<(String, ValueId)> = entries
                    .iter()
                    .map(|(k, v)| {
                        let key = self.expr_to_field_name(k);
                        let val = self.lower_expr(v);
                        (key, val)
                    })
                    .collect();
                self.fb.struct_init("Object", fields)
            }
            ExprKind::Template { parts } => self.lower_template(parts),
            ExprKind::Arrow { params, body } => {
                // Opaque JS function — store as SystemCall with param/body info
                let param_str = self.fb.const_string(params.join(","));
                let body_val = self.lower_expr(body);
                self.fb.system_call(
                    "SugarCube.Engine",
                    "arrow",
                    &[param_str, body_val],
                    Type::Dynamic,
                )
            }
            ExprKind::Delete(inner) => {
                let val = self.lower_expr(inner);
                self.fb
                    .system_call("SugarCube.Engine", "delete", &[val], Type::Dynamic)
            }
            ExprKind::TypeOf(inner) => {
                let val = self.lower_expr(inner);
                self.fb
                    .system_call("SugarCube.Engine", "typeof", &[val], Type::String)
            }
            ExprKind::Clone(inner) => {
                let val = self.lower_expr(inner);
                self.fb
                    .system_call("SugarCube.Engine", "clone", &[val], Type::Dynamic)
            }
            ExprKind::Def(inner) => {
                let val = self.lower_expr(inner);
                self.fb
                    .system_call("SugarCube.Engine", "def", &[val], Type::Bool)
            }
            ExprKind::Ndef(inner) => {
                let val = self.lower_expr(inner);
                self.fb
                    .system_call("SugarCube.Engine", "ndef", &[val], Type::Bool)
            }
            ExprKind::Paren(inner) => self.lower_expr(inner),
            ExprKind::Error(msg) => {
                // Parse error placeholder — emit a diagnostic and return null
                let m = self.fb.const_string(msg.as_str());
                self.fb
                    .system_call("SugarCube.Engine", "error", &[m], Type::Dynamic)
            }
        }
    }

    fn lower_literal(&mut self, lit: &Literal) -> ValueId {
        match lit {
            Literal::Bool(b) => self.fb.const_bool(*b),
            Literal::Number(n) => self.fb.const_float(n.value()),
            Literal::Null => self.fb.const_null(),
            Literal::Undefined => self.fb.const_null(), // undefined ≈ null in IR
        }
    }

    fn lower_unary_op(
        &mut self,
        op: UnaryOp,
        val: ValueId,
        operand: &Expr,
        is_prefix: bool,
    ) -> ValueId {
        match op {
            UnaryOp::Not => self.fb.not(val),
            UnaryOp::Neg => self.fb.neg(val),
            UnaryOp::Pos => {
                // Numeric coercion — cast to float
                self.fb.coerce(val, Type::Float(64))
            }
            UnaryOp::BitNot => self.fb.bit_not(val),
            UnaryOp::Inc | UnaryOp::Dec => {
                // Pre/post increment/decrement: modify in place, return old or new
                let one = self.fb.const_float(1.0);
                let new_val = if op == UnaryOp::Inc {
                    self.fb.add(val, one)
                } else {
                    self.fb.sub(val, one)
                };
                self.store_to_target(operand, new_val);
                if is_prefix {
                    new_val
                } else {
                    val
                }
            }
        }
    }

    fn lower_binary_op(&mut self, op: BinaryOp, left: &Expr, right: &Expr) -> ValueId {
        // Short-circuit operators need special control flow
        match op {
            BinaryOp::And => return self.lower_logical_and(left, right),
            BinaryOp::Or => return self.lower_logical_or(left, right),
            BinaryOp::NullishCoalesce => return self.lower_nullish_coalesce(left, right),
            _ => {}
        }

        let lhs = self.lower_expr(left);
        let rhs = self.lower_expr(right);

        match op {
            BinaryOp::Add => self.fb.add(lhs, rhs),
            BinaryOp::Sub => self.fb.sub(lhs, rhs),
            BinaryOp::Mul => self.fb.mul(lhs, rhs),
            BinaryOp::Div => self.fb.div(lhs, rhs),
            BinaryOp::Mod => self.fb.rem(lhs, rhs),
            BinaryOp::Exp => {
                self.fb
                    .system_call("SugarCube.Engine", "pow", &[lhs, rhs], Type::Dynamic)
            }
            BinaryOp::Eq => self.fb.cmp(CmpKind::Eq, lhs, rhs),
            BinaryOp::Neq => self.fb.cmp(CmpKind::Ne, lhs, rhs),
            BinaryOp::StrictEq => self.fb.cmp(CmpKind::Eq, lhs, rhs),
            BinaryOp::StrictNeq => self.fb.cmp(CmpKind::Ne, lhs, rhs),
            BinaryOp::Lt => self.fb.cmp(CmpKind::Lt, lhs, rhs),
            BinaryOp::Lte => self.fb.cmp(CmpKind::Le, lhs, rhs),
            BinaryOp::Gt => self.fb.cmp(CmpKind::Gt, lhs, rhs),
            BinaryOp::Gte => self.fb.cmp(CmpKind::Ge, lhs, rhs),
            BinaryOp::BitAnd => self.fb.bit_and(lhs, rhs),
            BinaryOp::BitOr => self.fb.bit_or(lhs, rhs),
            BinaryOp::BitXor => self.fb.bit_xor(lhs, rhs),
            BinaryOp::Shl => self.fb.shl(lhs, rhs),
            BinaryOp::Shr => self.fb.shr(lhs, rhs),
            BinaryOp::UShr => {
                self.fb
                    .system_call("SugarCube.Engine", "ushr", &[lhs, rhs], Type::Dynamic)
            }
            BinaryOp::In => {
                self.fb
                    .system_call("SugarCube.Engine", "in", &[lhs, rhs], Type::Bool)
            }
            BinaryOp::InstanceOf => {
                self.fb
                    .system_call("SugarCube.Engine", "instanceof", &[lhs, rhs], Type::Bool)
            }
            // And/Or/NullishCoalesce handled above
            BinaryOp::And | BinaryOp::Or | BinaryOp::NullishCoalesce => unreachable!(),
        }
    }

    fn lower_logical_and(&mut self, left: &Expr, right: &Expr) -> ValueId {
        let lhs = self.lower_expr(left);

        let rhs_block = self.fb.create_block();
        let merge_block = self.fb.create_block();
        let (_, merge_params) = ((), self.fb.add_block_params(merge_block, &[Type::Dynamic]));
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

        // If truthy, short-circuit to merge; otherwise evaluate rhs
        self.fb
            .br_if(lhs, merge_block, &[lhs], rhs_block, &[]);

        self.fb.switch_to_block(rhs_block);
        let rhs = self.lower_expr(right);
        self.fb.br(merge_block, &[rhs]);

        self.fb.switch_to_block(merge_block);
        merge_param
    }

    fn lower_nullish_coalesce(&mut self, left: &Expr, right: &Expr) -> ValueId {
        let lhs = self.lower_expr(left);
        let is_null = self.fb.system_call(
            "SugarCube.Engine",
            "is_nullish",
            &[lhs],
            Type::Bool,
        );

        let rhs_block = self.fb.create_block();
        let merge_block = self.fb.create_block();
        let merge_params = self.fb.add_block_params(merge_block, &[Type::Dynamic]);
        let merge_param = merge_params[0];

        // If nullish, evaluate rhs; otherwise use lhs
        self.fb
            .br_if(is_null, rhs_block, &[], merge_block, &[lhs]);

        self.fb.switch_to_block(rhs_block);
        let rhs = self.lower_expr(right);
        self.fb.br(merge_block, &[rhs]);

        self.fb.switch_to_block(merge_block);
        merge_param
    }

    fn lower_ternary(
        &mut self,
        cond: &Expr,
        then_expr: &Expr,
        else_expr: &Expr,
    ) -> ValueId {
        let cond_val = self.lower_expr(cond);

        let then_block = self.fb.create_block();
        let else_block = self.fb.create_block();
        let merge_block = self.fb.create_block();
        let merge_params = self.fb.add_block_params(merge_block, &[Type::Dynamic]);
        let merge_param = merge_params[0];

        self.fb
            .br_if(cond_val, then_block, &[], else_block, &[]);

        self.fb.switch_to_block(then_block);
        let then_val = self.lower_expr(then_expr);
        self.fb.br(merge_block, &[then_val]);

        self.fb.switch_to_block(else_block);
        let else_val = self.lower_expr(else_expr);
        self.fb.br(merge_block, &[else_val]);

        self.fb.switch_to_block(merge_block);
        merge_param
    }

    fn lower_assign(
        &mut self,
        compound_op: Option<CompoundOp>,
        target: &Expr,
        value: &Expr,
    ) -> ValueId {
        let rhs = self.lower_expr(value);

        let final_val = if let Some(cop) = compound_op {
            let current = self.lower_expr(target);
            self.apply_compound_op(cop, current, rhs)
        } else {
            rhs
        };

        self.store_to_target(target, final_val);
        final_val
    }

    fn apply_compound_op(&mut self, op: CompoundOp, lhs: ValueId, rhs: ValueId) -> ValueId {
        match op {
            CompoundOp::Add => self.fb.add(lhs, rhs),
            CompoundOp::Sub => self.fb.sub(lhs, rhs),
            CompoundOp::Mul => self.fb.mul(lhs, rhs),
            CompoundOp::Div => self.fb.div(lhs, rhs),
            CompoundOp::Mod => self.fb.rem(lhs, rhs),
            CompoundOp::Exp => {
                self.fb
                    .system_call("SugarCube.Engine", "pow", &[lhs, rhs], Type::Dynamic)
            }
            CompoundOp::BitAnd => self.fb.bit_and(lhs, rhs),
            CompoundOp::BitOr => self.fb.bit_or(lhs, rhs),
            CompoundOp::BitXor => self.fb.bit_xor(lhs, rhs),
            CompoundOp::Shl => self.fb.shl(lhs, rhs),
            CompoundOp::Shr => self.fb.shr(lhs, rhs),
            CompoundOp::UShr => {
                self.fb
                    .system_call("SugarCube.Engine", "ushr", &[lhs, rhs], Type::Dynamic)
            }
            CompoundOp::NullishCoalesce => {
                let is_null = self.fb.system_call(
                    "SugarCube.Engine",
                    "is_nullish",
                    &[lhs],
                    Type::Bool,
                );
                let then_block = self.fb.create_block();
                let merge_block = self.fb.create_block();
                let merge_params =
                    self.fb.add_block_params(merge_block, &[Type::Dynamic]);
                let merge_param = merge_params[0];

                self.fb
                    .br_if(is_null, then_block, &[], merge_block, &[lhs]);

                self.fb.switch_to_block(then_block);
                self.fb.br(merge_block, &[rhs]);

                self.fb.switch_to_block(merge_block);
                merge_param
            }
        }
    }

    /// Store a value to the location described by a target expression.
    fn store_to_target(&mut self, target: &Expr, value: ValueId) {
        match &target.kind {
            ExprKind::StoryVar(name) => {
                let n = self.fb.const_string(name.as_str());
                self.fb
                    .system_call("SugarCube.State", "set", &[n, value], Type::Void);
            }
            ExprKind::TempVar(name) => {
                let alloc = self.get_or_create_temp(name);
                self.fb.store(alloc, value);
            }
            ExprKind::Member { object, property } => {
                let obj = self.lower_expr(object);
                self.fb.set_field(obj, property.as_str(), value);
            }
            ExprKind::Index { object, index } => {
                let obj = self.lower_expr(object);
                let idx = self.lower_expr(index);
                self.fb.set_index(obj, idx, value);
            }
            _ => {
                // Unsupported assignment target — emit as side-effect
                let _ = self.lower_expr(target);
            }
        }
    }

    fn expr_to_field_name(&mut self, key: &Expr) -> String {
        match &key.kind {
            ExprKind::Ident(s) | ExprKind::Str(s) => s.clone(),
            ExprKind::Literal(Literal::Number(n)) => format!("{}", n.value()),
            _ => "__computed__".to_string(),
        }
    }

    fn lower_template(&mut self, parts: &[TemplatePart]) -> ValueId {
        if parts.is_empty() {
            return self.fb.const_string("");
        }

        let mut result: Option<ValueId> = None;
        for part in parts {
            let part_val = match part {
                TemplatePart::Str(s) => self.fb.const_string(s.as_str()),
                TemplatePart::Expr(e) => {
                    let val = self.lower_expr(e);
                    self.fb.system_call(
                        "SugarCube.Engine",
                        "to_string",
                        &[val],
                        Type::String,
                    )
                }
            };
            result = Some(match result {
                Some(acc) => self.fb.add(acc, part_val),
                None => part_val,
            });
        }
        result.unwrap()
    }

    // ── Node lowering ──────────────────────────────────────────────────

    pub fn lower_node(&mut self, node: &Node) {
        match &node.kind {
            NodeKind::Text(text) => {
                if !text.is_empty() {
                    self.emit_text(text);
                }
            }
            NodeKind::Macro(mac) => self.lower_macro(mac),
            NodeKind::Link(link) => self.lower_link(link),
            NodeKind::VarInterp(expr) => {
                let val = self.lower_expr(expr);
                self.emit_print(val);
            }
            NodeKind::Html(html) => {
                if !html.is_empty() {
                    self.emit_html(html);
                }
            }
            NodeKind::Comment(_) => {
                // Comments produce no output
            }
            NodeKind::LineBreak => {
                self.emit_line_break();
            }
        }
    }

    pub fn lower_nodes(&mut self, nodes: &[Node]) {
        for node in nodes {
            self.lower_node(node);
        }
    }

    fn lower_link(&mut self, link: &LinkNode) {
        // Build link text value
        let text_val = match &link.text {
            LinkText::Plain(s) => self.fb.const_string(s.as_str()),
            LinkText::Expr(e) => self.lower_expr(e),
        };

        // Build target value
        let target_val = match &link.target {
            LinkTarget::Name(name) => self.fb.const_string(name.as_str()),
            LinkTarget::Expr(e) => self.lower_expr(e),
        };

        // Lower setter expressions
        let setter_vals: Vec<ValueId> =
            link.setters.iter().map(|e| self.lower_expr(e)).collect();

        let mut args = vec![text_val, target_val];
        args.extend(setter_vals);

        self.fb
            .system_call("SugarCube.Output", "link", &args, Type::Void);
    }

    // ── Macro lowering ─────────────────────────────────────────────────

    /// Get the primary args from a macro. For block macros, args live in the
    /// first clause; for self-closing macros, they're in `mac.args` directly.
    fn primary_args<'a>(&self, mac: &'a MacroNode) -> &'a MacroArgs {
        if mac.clauses.is_empty() {
            &mac.args
        } else {
            // Block macros: args are in clauses[0].args
            // mac.args is always None for block macros
            &mac.clauses[0].args
        }
    }

    pub fn lower_macro(&mut self, mac: &MacroNode) {
        match mac.name.as_str() {
            // Assignment macros
            "set" => self.lower_set(mac),
            "unset" => self.lower_unset(mac),
            "run" => self.lower_run(mac),

            // Output macros
            "print" | "=" | "-" => self.lower_print(mac),

            // Control flow
            "if" => self.lower_if(mac),
            "for" => self.lower_for(mac),
            "switch" => self.lower_switch(mac),
            "break" => {
                // Break is handled in the loop lowering via IR break
                // For now, emit as a system call that the runtime handles
                self.fb
                    .system_call("SugarCube.Engine", "break", &[], Type::Void);
            }
            "continue" => {
                self.fb
                    .system_call("SugarCube.Engine", "continue", &[], Type::Void);
            }

            // Navigation
            "goto" => self.lower_goto(mac),
            "back" | "return" => self.lower_nav(mac),

            // Link/button block macros
            "link" | "linkappend" | "linkprepend" | "linkreplace" | "button" => {
                self.lower_link_macro(mac);
            }

            // Include
            "include" => self.lower_include(mac),

            // Output suppression
            "nobr" => self.lower_nobr(mac),
            "silently" => self.lower_silently(mac),

            // Capture
            "capture" => self.lower_capture(mac),

            // Widget definition
            "widget" => self.lower_widget(mac),

            // Script (raw JS)
            "script" => self.lower_script(mac),

            // Audio macros
            "audio" | "masteraudio" | "cacheaudio" | "waitforaudio"
            | "removeaudiogroup" | "removeplaylist" | "playlist"
            | "createplaylist" | "createaudiogroup" => {
                self.lower_audio_macro(mac);
            }

            // DOM macros
            "replace" | "append" | "prepend" | "remove" | "copy"
            | "addclass" | "removeclass" | "toggleclass" => {
                self.lower_dom_macro(mac);
            }

            // Interactive element macros
            "textbox" | "textarea" | "numberbox" | "radiobutton" | "checkbox"
            | "listbox" | "cycle" => {
                self.lower_input_macro(mac);
            }

            // Timed/repeat macros
            "timed" | "repeat" => self.lower_timed_macro(mac),

            // type macro (animated typing)
            "type" => self.lower_timed_macro(mac),

            // done macro (deferred execution)
            "done" => self.lower_done(mac),

            // stop macro
            "stop" => {
                self.fb.ret(None);
            }

            // Unknown → widget invocation
            _ => self.lower_unknown_macro(mac),
        }
    }

    fn lower_set(&mut self, mac: &MacroNode) {
        match &mac.args {
            MacroArgs::AssignList(exprs) => {
                for expr in exprs {
                    self.lower_expr(expr);
                }
            }
            MacroArgs::Expr(expr) => {
                self.lower_expr(expr);
            }
            _ => {}
        }
    }

    fn lower_unset(&mut self, mac: &MacroNode) {
        match &mac.args {
            MacroArgs::AssignList(exprs) => {
                for expr in exprs {
                    self.lower_unset_expr(expr);
                }
            }
            MacroArgs::Expr(expr) => {
                self.lower_unset_expr(expr);
            }
            _ => {}
        }
    }

    fn lower_unset_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::StoryVar(name) => {
                let n = self.fb.const_string(name.as_str());
                self.fb
                    .system_call("SugarCube.State", "unset", &[n], Type::Void);
            }
            ExprKind::TempVar(name) => {
                // Remove from temp vars by storing null
                let alloc = self.get_or_create_temp(name);
                let null = self.fb.const_null();
                self.fb.store(alloc, null);
            }
            _ => {
                // Just evaluate for side effects
                self.lower_expr(expr);
            }
        }
    }

    fn lower_run(&mut self, mac: &MacroNode) {
        if let MacroArgs::Expr(expr) = &mac.args {
            self.lower_expr(expr);
        }
    }

    fn lower_print(&mut self, mac: &MacroNode) {
        if let MacroArgs::Expr(expr) = &mac.args {
            let val = self.lower_expr(expr);
            self.emit_print(val);
        }
    }

    fn lower_if(&mut self, mac: &MacroNode) {
        if mac.clauses.is_empty() {
            return;
        }

        let merge_block = self.fb.create_block();
        self.lower_if_clauses(&mac.clauses, 0, merge_block);
        self.fb.switch_to_block(merge_block);
    }

    fn lower_if_clauses(
        &mut self,
        clauses: &[MacroClause],
        index: usize,
        merge_block: BlockId,
    ) {
        if index >= clauses.len() {
            // Fell through all clauses — branch to merge
            self.fb.br(merge_block, &[]);
            return;
        }

        let clause = &clauses[index];

        if clause.kind == "else" {
            // Unconditional — lower body and branch to merge
            self.lower_nodes(&clause.body);
            self.fb.br(merge_block, &[]);
            return;
        }

        // if/elseif — evaluate condition
        let cond = match &clause.args {
            MacroArgs::Expr(expr) => self.lower_expr(expr),
            _ => self.fb.const_bool(true),
        };

        let then_block = self.fb.create_block();
        let else_block = self.fb.create_block();

        self.fb
            .br_if(cond, then_block, &[], else_block, &[]);

        // Then branch
        self.fb.switch_to_block(then_block);
        self.lower_nodes(&clause.body);
        self.fb.br(merge_block, &[]);

        // Else branch — continue to next clause
        self.fb.switch_to_block(else_block);
        self.lower_if_clauses(clauses, index + 1, merge_block);
    }

    fn lower_for(&mut self, mac: &MacroNode) {
        let args = self.primary_args(mac).clone();
        match &args {
            MacroArgs::ForCStyle { init, cond, update } => {
                self.lower_for_cstyle(init, cond, update, &mac.clauses);
            }
            MacroArgs::ForRange { var, start, end } => {
                self.lower_for_range(var, start, end, &mac.clauses);
            }
            MacroArgs::ForIn {
                value_var,
                key_var,
                collection,
            } => {
                self.lower_for_in(value_var, key_var.as_deref(), collection, &mac.clauses);
            }
            _ => {
                // Fallback: just lower body
                for clause in &mac.clauses {
                    self.lower_nodes(&clause.body);
                }
            }
        }
    }

    fn lower_for_cstyle(
        &mut self,
        init: &Option<Box<Expr>>,
        cond: &Option<Box<Expr>>,
        update: &Option<Box<Expr>>,
        clauses: &[MacroClause],
    ) {
        // Init
        if let Some(init_expr) = init {
            self.lower_expr(init_expr);
        }

        let header_block = self.fb.create_block();
        let body_block = self.fb.create_block();
        let latch_block = self.fb.create_block();
        let exit_block = self.fb.create_block();

        self.fb.br(header_block, &[]);

        // Header: evaluate condition
        self.fb.switch_to_block(header_block);
        let cond_val = if let Some(cond_expr) = cond {
            self.lower_expr(cond_expr)
        } else {
            self.fb.const_bool(true)
        };
        self.fb
            .br_if(cond_val, body_block, &[], exit_block, &[]);

        // Body
        self.fb.switch_to_block(body_block);
        for clause in clauses {
            self.lower_nodes(&clause.body);
        }
        self.fb.br(latch_block, &[]);

        // Latch: update + loop back
        self.fb.switch_to_block(latch_block);
        if let Some(update_expr) = update {
            self.lower_expr(update_expr);
        }
        self.fb.br(header_block, &[]);

        // Exit
        self.fb.switch_to_block(exit_block);
    }

    fn lower_for_range(
        &mut self,
        var: &str,
        start: &Expr,
        end: &Expr,
        clauses: &[MacroClause],
    ) {
        let start_val = self.lower_expr(start);
        let end_val = self.lower_expr(end);
        let alloc = self.get_or_create_temp(var);
        self.fb.store(alloc, start_val);

        let header_block = self.fb.create_block();
        let body_block = self.fb.create_block();
        let latch_block = self.fb.create_block();
        let exit_block = self.fb.create_block();

        self.fb.br(header_block, &[]);

        // Header: check if current < end
        self.fb.switch_to_block(header_block);
        let current = self.fb.load(alloc, Type::Dynamic);
        let cond = self.fb.cmp(CmpKind::Lt, current, end_val);
        self.fb
            .br_if(cond, body_block, &[], exit_block, &[]);

        // Body
        self.fb.switch_to_block(body_block);
        for clause in clauses {
            self.lower_nodes(&clause.body);
        }
        self.fb.br(latch_block, &[]);

        // Latch: increment
        self.fb.switch_to_block(latch_block);
        let cur = self.fb.load(alloc, Type::Dynamic);
        let one = self.fb.const_float(1.0);
        let next = self.fb.add(cur, one);
        self.fb.store(alloc, next);
        self.fb.br(header_block, &[]);

        // Exit
        self.fb.switch_to_block(exit_block);
    }

    fn lower_for_in(
        &mut self,
        value_var: &str,
        key_var: Option<&str>,
        collection: &Expr,
        clauses: &[MacroClause],
    ) {
        let coll = self.lower_expr(collection);

        // Create iterator via SystemCall
        let iter = self.fb.system_call(
            "SugarCube.Engine",
            "iterate",
            &[coll],
            Type::Dynamic,
        );

        let header_block = self.fb.create_block();
        let body_block = self.fb.create_block();
        let exit_block = self.fb.create_block();

        self.fb.br(header_block, &[]);

        // Header: check iterator.hasNext()
        self.fb.switch_to_block(header_block);
        let has_next = self.fb.system_call(
            "SugarCube.Engine",
            "iterator_has_next",
            &[iter],
            Type::Bool,
        );
        self.fb
            .br_if(has_next, body_block, &[], exit_block, &[]);

        // Body: get next value/key
        self.fb.switch_to_block(body_block);
        let next_val = self.fb.system_call(
            "SugarCube.Engine",
            "iterator_next_value",
            &[iter],
            Type::Dynamic,
        );
        let val_alloc = self.get_or_create_temp(value_var);
        self.fb.store(val_alloc, next_val);

        if let Some(kv) = key_var {
            let next_key = self.fb.system_call(
                "SugarCube.Engine",
                "iterator_next_key",
                &[iter],
                Type::Dynamic,
            );
            let key_alloc = self.get_or_create_temp(kv);
            self.fb.store(key_alloc, next_key);
        }

        for clause in clauses {
            self.lower_nodes(&clause.body);
        }
        self.fb.br(header_block, &[]);

        // Exit
        self.fb.switch_to_block(exit_block);
    }

    fn lower_switch(&mut self, mac: &MacroNode) {
        let args = self.primary_args(mac).clone();
        let switch_val = match &args {
            MacroArgs::Switch(expr) => self.lower_expr(expr),
            _ => self.fb.const_null(),
        };

        let exit_block = self.fb.create_block();

        // Collect case/default clauses
        let mut case_blocks: Vec<(Vec<ValueId>, BlockId)> = Vec::new();
        let mut default_block: Option<BlockId> = None;

        for clause in &mac.clauses {
            let block = self.fb.create_block();

            match clause.kind.as_str() {
                "case" => {
                    let vals = match &clause.args {
                        MacroArgs::CaseValues(exprs) => {
                            exprs.iter().map(|e| self.lower_expr(e)).collect()
                        }
                        _ => vec![],
                    };
                    case_blocks.push((vals, block));
                }
                "default" => {
                    default_block = Some(block);
                }
                _ => {
                    // Treat "switch" (main clause) as the first case if it has a body
                    if !clause.body.is_empty() {
                        case_blocks.push((vec![], block));
                    }
                }
            }
        }

        // Build if-else chain for case matching (since case values are expressions, not constants)
        let default = default_block.unwrap_or(exit_block);
        self.lower_switch_chain(&case_blocks, switch_val, default, 0);

        // Lower each case body
        for (i, clause) in mac.clauses.iter().enumerate() {
            let block = if clause.kind == "default" {
                default_block.unwrap()
            } else if let Some((_, b)) = case_blocks.get(i) {
                *b
            } else {
                continue;
            };

            self.fb.switch_to_block(block);
            self.lower_nodes(&clause.body);
            self.fb.br(exit_block, &[]);
        }

        self.fb.switch_to_block(exit_block);
    }

    fn lower_switch_chain(
        &mut self,
        cases: &[(Vec<ValueId>, BlockId)],
        switch_val: ValueId,
        default_block: BlockId,
        index: usize,
    ) {
        if index >= cases.len() {
            self.fb.br(default_block, &[]);
            return;
        }

        let (vals, target) = &cases[index];
        if vals.is_empty() {
            // No case values — skip to next
            self.lower_switch_chain(cases, switch_val, default_block, index + 1);
            return;
        }

        // Check if switch_val matches any of the case values
        let mut combined: Option<ValueId> = None;
        for &v in vals {
            let eq = self.fb.cmp(CmpKind::Eq, switch_val, v);
            combined = Some(match combined {
                Some(prev) => {
                    // OR the conditions together using a block-based short circuit
                    // For simplicity, use a non-short-circuit OR via bit_or on bools
                    self.fb.bit_or(prev, eq)
                }
                None => eq,
            });
        }

        let cond = combined.unwrap();
        let next_block = self.fb.create_block();
        self.fb
            .br_if(cond, *target, &[], next_block, &[]);
        self.fb.switch_to_block(next_block);
        self.lower_switch_chain(cases, switch_val, default_block, index + 1);
    }

    fn lower_goto(&mut self, mac: &MacroNode) {
        if let MacroArgs::Expr(expr) = &mac.args {
            let target = self.lower_expr(expr);
            self.fb.system_call(
                "SugarCube.Navigation",
                "goto",
                &[target],
                Type::Void,
            );
        } else if let MacroArgs::Raw(s) = &mac.args {
            let target = self.fb.const_string(s.trim());
            self.fb.system_call(
                "SugarCube.Navigation",
                "goto",
                &[target],
                Type::Void,
            );
        }
    }

    fn lower_nav(&mut self, mac: &MacroNode) {
        let method = mac.name.as_str();
        match &mac.args {
            MacroArgs::Expr(expr) => {
                let val = self.lower_expr(expr);
                self.fb.system_call(
                    "SugarCube.Navigation",
                    method,
                    &[val],
                    Type::Void,
                );
            }
            MacroArgs::Raw(s) if !s.trim().is_empty() => {
                let val = self.fb.const_string(s.trim());
                self.fb.system_call(
                    "SugarCube.Navigation",
                    method,
                    &[val],
                    Type::Void,
                );
            }
            _ => {
                self.fb.system_call(
                    "SugarCube.Navigation",
                    method,
                    &[],
                    Type::Void,
                );
            }
        }
    }

    fn lower_link_macro(&mut self, mac: &MacroNode) {
        let variant = mac.name.as_str();

        // Extract link text and optional passage target from args
        let args = self.primary_args(mac).clone();
        let (text_val, passage_val) = match &args {
            MacroArgs::LinkArgs { text, passage } => {
                let t = match text {
                    LinkText::Plain(s) => self.fb.const_string(s.as_str()),
                    LinkText::Expr(e) => self.lower_expr(e),
                };
                let p = passage.as_ref().map(|e| self.lower_expr(e));
                (t, p)
            }
            MacroArgs::Expr(expr) => {
                let t = self.lower_expr(expr);
                (t, None)
            }
            MacroArgs::Raw(s) if !s.trim().is_empty() => {
                let t = self.fb.const_string(s.trim());
                (t, None)
            }
            _ => {
                let t = self.fb.const_string("");
                (t, None)
            }
        };

        let variant_val = self.fb.const_string(variant);
        let mut args = vec![variant_val, text_val];
        if let Some(p) = passage_val {
            args.push(p);
        }

        // Start link block
        self.fb.system_call(
            "SugarCube.Output",
            "link_block_start",
            &args,
            Type::Void,
        );

        // Lower body
        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }

        // End link block
        self.fb.system_call(
            "SugarCube.Output",
            "link_block_end",
            &[],
            Type::Void,
        );
    }

    fn lower_include(&mut self, mac: &MacroNode) {
        match &mac.args {
            MacroArgs::Expr(expr) => {
                let target = self.lower_expr(expr);
                self.fb.system_call(
                    "SugarCube.Navigation",
                    "include",
                    &[target],
                    Type::Void,
                );
            }
            MacroArgs::Raw(s) if !s.trim().is_empty() => {
                let target = self.fb.const_string(s.trim());
                self.fb.system_call(
                    "SugarCube.Navigation",
                    "include",
                    &[target],
                    Type::Void,
                );
            }
            _ => {}
        }
    }

    fn lower_nobr(&mut self, mac: &MacroNode) {
        // <<nobr>> suppresses whitespace line breaks (but doesn't suppress all output)
        // For IR purposes, we just lower the body normally
        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }
    }

    fn lower_silently(&mut self, mac: &MacroNode) {
        let was_suppressed = self.suppress_output;
        self.suppress_output = true;
        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }
        self.suppress_output = was_suppressed;
    }

    fn lower_capture(&mut self, mac: &MacroNode) {
        // <<capture>> creates a closure scope for the listed variables.
        // For IR, we just lower the body — the runtime handles scoping.
        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }
    }

    fn lower_widget(&mut self, mac: &MacroNode) {
        let args = self.primary_args(mac).clone();
        if let MacroArgs::WidgetDef { name } = &args {
            // Collect body nodes for the widget
            let mut body_nodes = Vec::new();
            for clause in &mac.clauses {
                body_nodes.extend(clause.body.clone());
            }
            self.widgets.push((name.clone(), body_nodes));
        }
    }

    fn lower_script(&mut self, mac: &MacroNode) {
        // <<script>> contains raw JS. The first clause body is typically a single
        // Text node with the raw script content.
        for clause in &mac.clauses {
            for node in &clause.body {
                if let NodeKind::Text(code) = &node.kind {
                    let code_val = self.fb.const_string(code.as_str());
                    self.fb.system_call(
                        "SugarCube.Engine",
                        "eval",
                        &[code_val],
                        Type::Void,
                    );
                }
            }
        }
    }

    fn lower_audio_macro(&mut self, mac: &MacroNode) {
        let method = mac.name.as_str();
        let args = self.collect_macro_args(mac);
        self.fb
            .system_call("SugarCube.Audio", method, &args, Type::Void);
    }

    fn lower_dom_macro(&mut self, mac: &MacroNode) {
        let method = mac.name.as_str();
        let args = self.collect_macro_args(mac);

        // Start DOM block
        self.fb
            .system_call("SugarCube.DOM", format!("{method}_start"), &args, Type::Void);

        // Lower body if it's a block macro
        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }

        // End DOM block
        self.fb
            .system_call("SugarCube.DOM", format!("{method}_end"), &[], Type::Void);
    }

    fn lower_input_macro(&mut self, mac: &MacroNode) {
        let method = mac.name.as_str();
        let args = self.collect_macro_args(mac);
        self.fb
            .system_call("SugarCube.Input", method, &args, Type::Void);

        // Lower body for block variants (listbox, cycle)
        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }
    }

    fn lower_timed_macro(&mut self, mac: &MacroNode) {
        let method = mac.name.as_str();
        let args = self.collect_macro_args(mac);

        self.fb.system_call(
            "SugarCube.Output",
            format!("{method}_start"),
            &args,
            Type::Void,
        );

        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }

        self.fb.system_call(
            "SugarCube.Output",
            format!("{method}_end"),
            &[],
            Type::Void,
        );
    }

    fn lower_done(&mut self, mac: &MacroNode) {
        self.fb.system_call(
            "SugarCube.Engine",
            "done_start",
            &[],
            Type::Void,
        );

        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }

        self.fb.system_call(
            "SugarCube.Engine",
            "done_end",
            &[],
            Type::Void,
        );
    }

    fn lower_unknown_macro(&mut self, mac: &MacroNode) {
        // Unknown macro → widget invocation
        let name_val = self.fb.const_string(mac.name.as_str());
        let args = self.collect_macro_args(mac);
        let mut all_args = vec![name_val];
        all_args.extend(args);

        self.fb.system_call(
            "SugarCube.Widget",
            "call",
            &all_args,
            Type::Void,
        );

        // If it has a body, lower it within the widget's content block
        if !mac.clauses.is_empty() {
            self.fb.system_call(
                "SugarCube.Widget",
                "content_start",
                &[],
                Type::Void,
            );
            for clause in &mac.clauses {
                self.lower_nodes(&clause.body);
            }
            self.fb.system_call(
                "SugarCube.Widget",
                "content_end",
                &[],
                Type::Void,
            );
        }
    }

    /// Collect macro arguments as a vec of ValueIds.
    fn collect_macro_args(&mut self, mac: &MacroNode) -> Vec<ValueId> {
        let args = self.primary_args(mac).clone();
        match &args {
            MacroArgs::None => vec![],
            MacroArgs::Expr(expr) => vec![self.lower_expr(expr)],
            MacroArgs::AssignList(exprs) => {
                exprs.iter().map(|e| self.lower_expr(e)).collect()
            }
            MacroArgs::Raw(s) => {
                if s.trim().is_empty() {
                    vec![]
                } else {
                    vec![self.fb.const_string(s.trim())]
                }
            }
            MacroArgs::LinkArgs { text, passage } => {
                let mut args = vec![match text {
                    LinkText::Plain(s) => self.fb.const_string(s.as_str()),
                    LinkText::Expr(e) => self.lower_expr(e),
                }];
                if let Some(p) = passage {
                    args.push(self.lower_expr(p));
                }
                args
            }
            MacroArgs::Switch(expr) => vec![self.lower_expr(expr)],
            MacroArgs::CaseValues(exprs) => {
                exprs.iter().map(|e| self.lower_expr(e)).collect()
            }
            MacroArgs::WidgetDef { name } => {
                vec![self.fb.const_string(name.as_str())]
            }
            MacroArgs::ForCStyle { init, cond, update } => {
                let mut args = Vec::new();
                if let Some(i) = init {
                    args.push(self.lower_expr(i));
                }
                if let Some(c) = cond {
                    args.push(self.lower_expr(c));
                }
                if let Some(u) = update {
                    args.push(self.lower_expr(u));
                }
                args
            }
            MacroArgs::ForRange { var, start, end } => {
                let v = self.fb.const_string(var.as_str());
                let s = self.lower_expr(start);
                let e = self.lower_expr(end);
                vec![v, s, e]
            }
            MacroArgs::ForIn {
                value_var,
                key_var,
                collection,
            } => {
                let v = self.fb.const_string(value_var.as_str());
                let mut args = vec![v];
                if let Some(k) = key_var {
                    args.push(self.fb.const_string(k.as_str()));
                }
                args.push(self.lower_expr(collection));
                args
            }
        }
    }
}

/// Translate a parsed passage AST into an IR Function.
pub fn translate_passage(name: &str, ast: &PassageAst) -> (Function, Vec<(String, Vec<Node>)>) {
    let func_name = passage_func_name(name);
    let mut ctx = TranslateCtx::new(&func_name);

    ctx.lower_nodes(&ast.body);

    // Terminate the function if not already terminated
    ctx.fb.ret(None);

    let widgets = std::mem::take(&mut ctx.widgets);
    (ctx.fb.build(), widgets)
}

/// Translate a widget body into an IR Function.
pub fn translate_widget(name: &str, body: &[Node]) -> Function {
    let func_name = format!("widget_{name}");
    let mut ctx = TranslateCtx::new(&func_name);

    ctx.lower_nodes(body);
    ctx.fb.ret(None);

    ctx.fb.build()
}

/// Translate a user `<script>` block into an IR Function that evals the code.
pub fn translate_user_script(index: usize, code: &str) -> Function {
    let func_name = format!("__user_script_{index}");
    let mut ctx = TranslateCtx::new(&func_name);
    let code_val = ctx.fb.const_string(code);
    ctx.fb
        .system_call("SugarCube.Engine", "eval", &[code_val], Type::Void);
    ctx.fb.ret(None);
    ctx.fb.build()
}

/// Convert a passage name to a function name.
pub fn passage_func_name(name: &str) -> String {
    let sanitized: String = name
        .chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
        .collect();
    format!("passage_{sanitized}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sugarcube::parser;

    /// Helper: parse a passage and translate it, returning the Function.
    fn translate(source: &str) -> Function {
        let ast = parser::parse(source);
        assert!(ast.errors.is_empty(), "parse errors: {:?}", ast.errors);
        let (func, _widgets) = translate_passage("test", &ast);
        func
    }

    #[test]
    fn plain_text_emits_output() {
        let func = translate("Hello, world!");
        // Should have at least the entry block with some instructions
        assert!(!func.blocks.values().next().unwrap().insts.is_empty());
    }

    #[test]
    fn story_var_read() {
        let func = translate("<<print $name>>");
        // Should produce SystemCall instructions for state get + output print
        let inst_count: usize = func.blocks.values().map(|b| b.insts.len()).sum();
        assert!(inst_count >= 2, "expected at least 2 instructions, got {inst_count}");
    }

    #[test]
    fn temp_var_alloc_and_use() {
        let func = translate("<<set _x to 42>><<print _x>>");
        let inst_count: usize = func.blocks.values().map(|b| b.insts.len()).sum();
        assert!(inst_count >= 3, "expected at least 3 instructions, got {inst_count}");
    }

    #[test]
    fn if_else_creates_blocks() {
        let func = translate("<<if $x>>yes<<else>>no<</if>>");
        // Should have multiple blocks for the if/else branches
        assert!(func.blocks.len() >= 3, "expected at least 3 blocks, got {}", func.blocks.len());
    }

    #[test]
    fn link_produces_syscall() {
        let func = translate("[[Go somewhere|destination]]");
        let inst_count: usize = func.blocks.values().map(|b| b.insts.len()).sum();
        assert!(inst_count >= 1, "expected at least 1 instruction for link");
    }

    #[test]
    fn for_loop_creates_blocks() {
        let func = translate("<<for _i to 0; _i lt 10; _i to _i + 1>>item<</for>>");
        // header + body + latch + exit = at least 4 blocks beyond entry
        assert!(func.blocks.len() >= 4, "expected at least 4 blocks, got {}", func.blocks.len());
    }

    #[test]
    fn widget_extraction() {
        let source = "<<widget \"myWidget\">>body<</widget>>";
        let ast = parser::parse(source);
        assert!(ast.errors.is_empty(), "parse errors: {:?}", ast.errors);
        // Verify parser produces a widget macro node
        assert_eq!(ast.body.len(), 1, "expected 1 node, got: {:#?}", ast.body);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "widget");
            // Block macros: args are in clauses[0].args, not m.args
            assert!(!m.clauses.is_empty(), "expected clauses");
            assert!(
                matches!(&m.clauses[0].args, MacroArgs::WidgetDef { name } if name == "myWidget"),
                "unexpected clause args: {:?}", m.clauses[0].args
            );
        } else {
            panic!("expected Macro node, got: {:?}", ast.body[0].kind);
        }
        let (_func, widgets) = translate_passage("test", &ast);
        assert_eq!(widgets.len(), 1);
        assert_eq!(widgets[0].0, "myWidget");
    }

    #[test]
    fn passage_func_name_sanitization() {
        assert_eq!(passage_func_name("Start"), "passage_Start");
        assert_eq!(passage_func_name("My Passage"), "passage_My_Passage");
        assert_eq!(passage_func_name("a-b.c"), "passage_a_b_c");
    }
}
