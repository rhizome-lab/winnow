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

use oxc_allocator::Allocator;
use oxc_ast::ast as js;
use oxc_parser::Parser as OxcParser;
use oxc_span::SourceType;

use reincarnate_core::ir::{
    BlockId, CmpKind, Function, FunctionBuilder, FunctionSig, MethodKind, Type, ValueId,
    Visibility,
};

use super::ast::*;
use super::preprocess::{self, Preprocessed};

/// Translation context for a single passage/widget function.
pub struct TranslateCtx {
    /// The IR function builder.
    pub fb: FunctionBuilder,
    /// Map from temp variable name → alloc ValueId.
    temp_vars: HashMap<String, ValueId>,
    /// Map from parameter name → ValueId (for arrow function parameters).
    local_params: HashMap<String, ValueId>,
    /// Whether we're inside a `<<nobr>>` / `<<silently>>` block.
    suppress_output: bool,
    /// Whether line breaks are suppressed (inside `<<nobr>>`).
    suppress_line_breaks: bool,
    /// Extracted widget definitions (name, body nodes, source) accumulated during translation.
    pub widgets: Vec<(String, Vec<Node>, String)>,
    /// The passage/widget function name (used to generate unique setter names).
    func_name: String,
    /// Counter for generating unique setter callback names.
    setter_count: usize,
    /// Counter for generating unique arrow function names.
    arrow_count: usize,
    /// Setter callback functions generated for link setters.
    pub setter_callbacks: Vec<Function>,
    /// The full passage source text (for slicing Expr byte ranges).
    source: String,
}

impl TranslateCtx {
    pub fn new(name: &str, source: &str) -> Self {
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
            local_params: HashMap::new(),
            suppress_output: false,
            suppress_line_breaks: false,
            widgets: Vec::new(),
            func_name: name.to_string(),
            setter_count: 0,
            arrow_count: 0,
            setter_callbacks: Vec::new(),
            source: source.to_string(),
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

    fn emit_structured_open(
        &mut self,
        tag: &str,
        attrs: &[(String, String)],
        dynamic_attrs: &[(String, String)],
    ) {
        if self.suppress_output {
            return;
        }
        let tag_val = self.fb.const_string(tag);
        let mut args = vec![tag_val];
        for (k, v) in attrs {
            args.push(self.fb.const_string(k));
            args.push(self.fb.const_string(v));
        }
        self.fb
            .system_call("SugarCube.Output", "open_element", &args, Type::Void);

        // Apply dynamic @attr="expr" attributes
        self.emit_dynamic_attrs(dynamic_attrs);
    }

    fn emit_structured_close(&mut self) {
        if self.suppress_output {
            return;
        }
        self.fb
            .system_call("SugarCube.Output", "close_element", &[], Type::Void);
    }

    fn emit_structured_void(
        &mut self,
        tag: &str,
        attrs: &[(String, String)],
        dynamic_attrs: &[(String, String)],
    ) {
        if self.suppress_output {
            return;
        }
        let tag_val = self.fb.const_string(tag);
        let mut args = vec![tag_val];
        for (k, v) in attrs {
            args.push(self.fb.const_string(k));
            args.push(self.fb.const_string(v));
        }
        self.fb
            .system_call("SugarCube.Output", "void_element", &args, Type::Void);

        // Apply dynamic @attr="expr" attributes
        self.emit_dynamic_attrs(dynamic_attrs);
    }

    fn emit_dynamic_attrs(&mut self, dynamic_attrs: &[(String, String)]) {
        if dynamic_attrs.is_empty() {
            return;
        }
        for (attr_name, expr_str) in dynamic_attrs {
            let val = self.lower_raw_expr_str(expr_str);
            let name_val = self.fb.const_string(attr_name);
            self.fb.system_call(
                "SugarCube.Output",
                "set_attribute",
                &[name_val, val],
                Type::Void,
            );
        }
    }

    fn emit_line_break(&mut self) {
        if self.suppress_output || self.suppress_line_breaks {
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

    // ── Expression lowering (oxc-based) ───────────────────────────────

    /// Lower an `Expr` (byte range) by extracting the source text and parsing with oxc.
    pub fn lower_expr(&mut self, expr: &Expr) -> ValueId {
        let src = self.source.clone();
        let text = &src[expr.start..expr.end];
        self.lower_raw_expr_str(text)
    }

    /// Lower a raw expression string through the preprocess → oxc → IR pipeline.
    fn lower_raw_expr_str(&mut self, text: &str) -> ValueId {
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return self.fb.const_null();
        }

        // Preprocess SugarCube keywords → JS
        let pp = preprocess::preprocess(trimmed);

        // Parse with oxc
        let allocator = Allocator::default();
        let source_type = SourceType::mjs();
        let result = OxcParser::new(&allocator, &pp.js, source_type).parse_expression();

        match result {
            Ok(oxc_expr) => self.lower_oxc_expr(&oxc_expr, &pp),
            Err(_) => {
                // Parse error — emit runtime error
                eprintln!(
                    "warning: oxc parse error in {}: {:?} (source: {})",
                    self.func_name, &pp.js, trimmed
                );
                let m = self.fb.const_string(format!("parse error: {trimmed}"));
                self.fb
                    .system_call("SugarCube.Engine", "error", &[m], Type::Dynamic)
            }
        }
    }

    /// Walk an oxc AST expression and produce IR.
    fn lower_oxc_expr(&mut self, expr: &js::Expression<'_>, pp: &Preprocessed) -> ValueId {
        match expr {
            js::Expression::BooleanLiteral(lit) => self.fb.const_bool(lit.value),
            js::Expression::NullLiteral(_) => self.fb.const_null(),
            js::Expression::NumericLiteral(lit) => self.fb.const_float(lit.value),
            js::Expression::BigIntLiteral(lit) => {
                // Approximate as float
                let val: f64 = lit.raw.as_ref().and_then(|r| r.parse().ok()).unwrap_or(0.0);
                self.fb.const_float(val)
            }
            js::Expression::StringLiteral(lit) => {
                self.fb.const_string(lit.value.as_str())
            }
            js::Expression::RegExpLiteral(lit) => {
                // new RegExp(pattern, flags)
                let pattern = self.fb.const_string(lit.regex.pattern.text.as_str());
                let flags_str = lit.regex.flags.to_string();
                let flags = self.fb.const_string(&flags_str);
                let regexp_name = self.fb.const_string("RegExp");
                let regexp = self.fb.system_call(
                    "SugarCube.Engine",
                    "resolve",
                    &[regexp_name],
                    Type::Dynamic,
                );
                let mut new_args = vec![regexp, pattern];
                if !flags_str.is_empty() {
                    new_args.push(flags);
                }
                self.fb
                    .system_call("SugarCube.Engine", "new", &new_args, Type::Dynamic)
            }
            js::Expression::TemplateLiteral(tl) => {
                self.lower_template_literal(tl, pp)
            }
            js::Expression::Identifier(ident) => {
                let name = ident.name.as_str();
                // Check SugarCube variable sigils
                if let Some(var_name) = name.strip_prefix('$') {
                    let n = self.fb.const_string(var_name);
                    return self.fb
                        .system_call("SugarCube.State", "get", &[n], Type::Dynamic);
                }
                if let Some(stripped) = name.strip_prefix('_') {
                    // Temp variable (only if followed by alphanumeric, which the
                    // identifier always is since _ alone isn't a SugarCube var)
                    if !stripped.is_empty() {
                        let alloc = self.get_or_create_temp(stripped);
                        return self.fb.load(alloc, Type::Dynamic);
                    }
                }
                // Check if identifier is a known local parameter
                if let Some(&param_val) = self.local_params.get(name) {
                    return param_val;
                }
                // Check for `undefined`
                if name == "undefined" {
                    return self.fb.const_null();
                }
                // Bare identifier — global function ref or runtime lookup
                let n = self.fb.const_string(name);
                self.fb
                    .system_call("SugarCube.Engine", "resolve", &[n], Type::Dynamic)
            }
            js::Expression::StaticMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                self.fb.get_field(obj, mem.property.name.as_str(), Type::Dynamic)
            }
            js::Expression::ComputedMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                let idx = self.lower_oxc_expr(&mem.expression, pp);
                self.fb.get_index(obj, idx, Type::Dynamic)
            }
            js::Expression::CallExpression(call) => {
                let args: Vec<ValueId> = call
                    .arguments
                    .iter()
                    .map(|a| self.lower_oxc_argument(a, pp))
                    .collect();
                let callee = self.lower_oxc_expr(&call.callee, pp);
                self.fb.call_indirect(callee, &args, Type::Dynamic)
            }
            js::Expression::NewExpression(new) => {
                let callee = self.lower_oxc_expr(&new.callee, pp);
                let mut all_args = vec![callee];
                for a in &new.arguments {
                    all_args.push(self.lower_oxc_argument(a, pp));
                }
                self.fb
                    .system_call("SugarCube.Engine", "new", &all_args, Type::Dynamic)
            }
            js::Expression::UnaryExpression(unary) => {
                self.lower_oxc_unary(unary, pp)
            }
            js::Expression::UpdateExpression(update) => {
                self.lower_oxc_update(update, pp)
            }
            js::Expression::BinaryExpression(bin) => {
                self.lower_oxc_binary(bin, pp)
            }
            js::Expression::LogicalExpression(log) => {
                self.lower_oxc_logical(log, pp)
            }
            js::Expression::ConditionalExpression(cond) => {
                self.lower_oxc_ternary(cond, pp)
            }
            js::Expression::AssignmentExpression(assign) => {
                self.lower_oxc_assign(assign, pp)
            }
            js::Expression::SequenceExpression(seq) => {
                let mut last = self.fb.const_null();
                for e in &seq.expressions {
                    last = self.lower_oxc_expr(e, pp);
                }
                last
            }
            js::Expression::ArrayExpression(arr) => {
                let vals: Vec<ValueId> = arr
                    .elements
                    .iter()
                    .map(|e| match e {
                        js::ArrayExpressionElement::SpreadElement(spread) => {
                            let val = self.lower_oxc_expr(&spread.argument, pp);
                            self.fb.spread(val)
                        }
                        js::ArrayExpressionElement::Elision(_) => self.fb.const_null(),
                        _ => self.lower_oxc_expr(e.to_expression(), pp),
                    })
                    .collect();
                self.fb.array_init(&vals, Type::Dynamic)
            }
            js::Expression::ObjectExpression(obj) => {
                let fields: Vec<(String, ValueId)> = obj
                    .properties
                    .iter()
                    .map(|prop| match prop {
                        js::ObjectPropertyKind::ObjectProperty(p) => {
                            let key = self.oxc_property_key_name(&p.key, pp);
                            let val = self.lower_oxc_expr(&p.value, pp);
                            (key, val)
                        }
                        js::ObjectPropertyKind::SpreadProperty(spread) => {
                            let val = self.lower_oxc_expr(&spread.argument, pp);
                            ("__spread__".to_string(), self.fb.spread(val))
                        }
                    })
                    .collect();
                self.fb.struct_init("Object", fields)
            }
            js::Expression::ArrowFunctionExpression(arrow) => {
                self.lower_oxc_arrow(arrow, pp)
            }
            js::Expression::ParenthesizedExpression(paren) => {
                self.lower_oxc_expr(&paren.expression, pp)
            }
            js::Expression::TaggedTemplateExpression(tagged) => {
                // Lower tag(template) as a call
                let tag = self.lower_oxc_expr(&tagged.tag, pp);
                let tl_val = self.lower_template_literal(&tagged.quasi, pp);
                self.fb.call_indirect(tag, &[tl_val], Type::Dynamic)
            }
            js::Expression::ChainExpression(chain) => {
                // Optional chaining: a?.b, a?.(), a?.[b]
                self.lower_oxc_chain(&chain.expression, pp)
            }
            js::Expression::YieldExpression(_)
            | js::Expression::AwaitExpression(_)
            | js::Expression::ClassExpression(_)
            | js::Expression::FunctionExpression(_)
            | js::Expression::ImportExpression(_)
            | js::Expression::MetaProperty(_)
            | js::Expression::Super(_)
            | js::Expression::ThisExpression(_)
            | js::Expression::TSAsExpression(_)
            | js::Expression::TSSatisfiesExpression(_)
            | js::Expression::TSTypeAssertion(_)
            | js::Expression::TSNonNullExpression(_)
            | js::Expression::TSInstantiationExpression(_)
            | js::Expression::PrivateFieldExpression(_)
            | js::Expression::PrivateInExpression(_)
            | js::Expression::JSXElement(_)
            | js::Expression::JSXFragment(_)
            | js::Expression::V8IntrinsicExpression(_) => {
                // Unsupported expression types — emit error
                let m = self.fb.const_string("unsupported expression");
                self.fb
                    .system_call("SugarCube.Engine", "error", &[m], Type::Dynamic)
            }
        }
    }

    fn lower_oxc_argument(&mut self, arg: &js::Argument<'_>, pp: &Preprocessed) -> ValueId {
        match arg {
            js::Argument::SpreadElement(spread) => {
                let val = self.lower_oxc_expr(&spread.argument, pp);
                self.fb.spread(val)
            }
            _ => self.lower_oxc_expr(arg.to_expression(), pp),
        }
    }

    fn lower_oxc_unary(
        &mut self,
        unary: &js::UnaryExpression<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        // Check if this `typeof` is actually a `def`, `ndef`, or `clone`
        if unary.operator == js::UnaryOperator::Typeof {
            let expr_start = unary.span.start as usize;
            if pp.def_positions.contains(&expr_start) {
                let val = self.lower_oxc_expr(&unary.argument, pp);
                return self.fb
                    .system_call("SugarCube.Engine", "def", &[val], Type::Bool);
            }
            if pp.ndef_positions.contains(&expr_start) {
                let val = self.lower_oxc_expr(&unary.argument, pp);
                return self.fb
                    .system_call("SugarCube.Engine", "ndef", &[val], Type::Bool);
            }
            if pp.clone_positions.contains(&expr_start) {
                let val = self.lower_oxc_expr(&unary.argument, pp);
                return self.fb
                    .system_call("SugarCube.Engine", "clone", &[val], Type::Dynamic);
            }
        }

        let val = self.lower_oxc_expr(&unary.argument, pp);
        match unary.operator {
            js::UnaryOperator::UnaryNegation => self.fb.neg(val),
            js::UnaryOperator::UnaryPlus => self.fb.coerce(val, Type::Float(64)),
            js::UnaryOperator::LogicalNot => self.fb.not(val),
            js::UnaryOperator::BitwiseNot => self.fb.bit_not(val),
            js::UnaryOperator::Typeof => {
                self.fb
                    .system_call("SugarCube.Engine", "typeof", &[val], Type::String)
            }
            js::UnaryOperator::Void => {
                // void expr → evaluate for side effects, return undefined
                self.fb.const_null()
            }
            js::UnaryOperator::Delete => {
                self.fb
                    .system_call("SugarCube.Engine", "delete", &[val], Type::Dynamic)
            }
        }
    }

    /// Read the current value from a SimpleAssignmentTarget (used by update expressions).
    fn lower_simple_assignment_target_read(
        &mut self,
        target: &js::SimpleAssignmentTarget<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        match target {
            js::SimpleAssignmentTarget::AssignmentTargetIdentifier(ident) => {
                let name = ident.name.as_str();
                if let Some(var_name) = name.strip_prefix('$') {
                    let n = self.fb.const_string(var_name);
                    self.fb.system_call("SugarCube.State", "get", &[n], Type::Dynamic)
                } else if let Some(stripped) = name.strip_prefix('_') {
                    if !stripped.is_empty() {
                        let alloc = self.get_or_create_temp(stripped);
                        self.fb.load(alloc, Type::Dynamic)
                    } else {
                        self.fb.const_null()
                    }
                } else if let Some(&param_val) = self.local_params.get(name) {
                    param_val
                } else {
                    let n = self.fb.const_string(name);
                    self.fb.system_call("SugarCube.Engine", "resolve", &[n], Type::Dynamic)
                }
            }
            _ => {
                if let Some(member) = target.as_member_expression() {
                    match member {
                        js::MemberExpression::StaticMemberExpression(mem) => {
                            let obj = self.lower_oxc_expr(&mem.object, pp);
                            self.fb.get_field(obj, mem.property.name.as_str(), Type::Dynamic)
                        }
                        js::MemberExpression::ComputedMemberExpression(mem) => {
                            let obj = self.lower_oxc_expr(&mem.object, pp);
                            let idx = self.lower_oxc_expr(&mem.expression, pp);
                            self.fb.get_index(obj, idx, Type::Dynamic)
                        }
                        _ => self.fb.const_null(),
                    }
                } else {
                    self.fb.const_null()
                }
            }
        }
    }

    fn lower_oxc_update(
        &mut self,
        update: &js::UpdateExpression<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        let val = self.lower_simple_assignment_target_read(&update.argument, pp);
        let one = self.fb.const_float(1.0);
        let new_val = match update.operator {
            js::UpdateOperator::Increment => self.fb.add(val, one),
            js::UpdateOperator::Decrement => self.fb.sub(val, one),
        };
        self.store_to_oxc_target(&update.argument, new_val, pp);
        if update.prefix {
            new_val
        } else {
            val
        }
    }

    fn lower_oxc_binary(
        &mut self,
        bin: &js::BinaryExpression<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        let lhs = self.lower_oxc_expr(&bin.left, pp);
        let rhs = self.lower_oxc_expr(&bin.right, pp);

        match bin.operator {
            js::BinaryOperator::Addition => self.fb.add(lhs, rhs),
            js::BinaryOperator::Subtraction => self.fb.sub(lhs, rhs),
            js::BinaryOperator::Multiplication => self.fb.mul(lhs, rhs),
            js::BinaryOperator::Division => self.fb.div(lhs, rhs),
            js::BinaryOperator::Remainder => self.fb.rem(lhs, rhs),
            js::BinaryOperator::Exponential => {
                self.fb
                    .system_call("SugarCube.Engine", "pow", &[lhs, rhs], Type::Dynamic)
            }
            js::BinaryOperator::Equality => self.fb.cmp(CmpKind::LooseEq, lhs, rhs),
            js::BinaryOperator::Inequality => self.fb.cmp(CmpKind::LooseNe, lhs, rhs),
            js::BinaryOperator::StrictEquality => self.fb.cmp(CmpKind::Eq, lhs, rhs),
            js::BinaryOperator::StrictInequality => self.fb.cmp(CmpKind::Ne, lhs, rhs),
            js::BinaryOperator::LessThan => self.fb.cmp(CmpKind::Lt, lhs, rhs),
            js::BinaryOperator::LessEqualThan => self.fb.cmp(CmpKind::Le, lhs, rhs),
            js::BinaryOperator::GreaterThan => self.fb.cmp(CmpKind::Gt, lhs, rhs),
            js::BinaryOperator::GreaterEqualThan => self.fb.cmp(CmpKind::Ge, lhs, rhs),
            js::BinaryOperator::BitwiseAnd => self.fb.bit_and(lhs, rhs),
            js::BinaryOperator::BitwiseOR => self.fb.bit_or(lhs, rhs),
            js::BinaryOperator::BitwiseXOR => self.fb.bit_xor(lhs, rhs),
            js::BinaryOperator::ShiftLeft => self.fb.shl(lhs, rhs),
            js::BinaryOperator::ShiftRight => self.fb.shr(lhs, rhs),
            js::BinaryOperator::ShiftRightZeroFill => {
                self.fb
                    .system_call("SugarCube.Engine", "ushr", &[lhs, rhs], Type::Dynamic)
            }
            js::BinaryOperator::In => {
                self.fb
                    .system_call("SugarCube.Engine", "in", &[lhs, rhs], Type::Bool)
            }
            js::BinaryOperator::Instanceof => {
                self.fb
                    .system_call("SugarCube.Engine", "instanceof", &[lhs, rhs], Type::Bool)
            }
        }
    }

    fn lower_oxc_logical(
        &mut self,
        log: &js::LogicalExpression<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        match log.operator {
            js::LogicalOperator::And => {
                let lhs = self.lower_oxc_expr(&log.left, pp);
                let rhs_block = self.fb.create_block();
                let merge_block = self.fb.create_block();
                let merge_params =
                    self.fb.add_block_params(merge_block, &[Type::Dynamic]);
                let merge_param = merge_params[0];
                self.fb
                    .br_if(lhs, rhs_block, &[], merge_block, &[lhs]);
                self.fb.switch_to_block(rhs_block);
                let rhs = self.lower_oxc_expr(&log.right, pp);
                self.fb.br(merge_block, &[rhs]);
                self.fb.switch_to_block(merge_block);
                merge_param
            }
            js::LogicalOperator::Or => {
                let lhs = self.lower_oxc_expr(&log.left, pp);
                let rhs_block = self.fb.create_block();
                let merge_block = self.fb.create_block();
                let merge_params =
                    self.fb.add_block_params(merge_block, &[Type::Dynamic]);
                let merge_param = merge_params[0];
                self.fb
                    .br_if(lhs, merge_block, &[lhs], rhs_block, &[]);
                self.fb.switch_to_block(rhs_block);
                let rhs = self.lower_oxc_expr(&log.right, pp);
                self.fb.br(merge_block, &[rhs]);
                self.fb.switch_to_block(merge_block);
                merge_param
            }
            js::LogicalOperator::Coalesce => {
                let lhs = self.lower_oxc_expr(&log.left, pp);
                let is_null = self.fb.system_call(
                    "SugarCube.Engine",
                    "is_nullish",
                    &[lhs],
                    Type::Bool,
                );
                let rhs_block = self.fb.create_block();
                let merge_block = self.fb.create_block();
                let merge_params =
                    self.fb.add_block_params(merge_block, &[Type::Dynamic]);
                let merge_param = merge_params[0];
                self.fb
                    .br_if(is_null, rhs_block, &[], merge_block, &[lhs]);
                self.fb.switch_to_block(rhs_block);
                let rhs = self.lower_oxc_expr(&log.right, pp);
                self.fb.br(merge_block, &[rhs]);
                self.fb.switch_to_block(merge_block);
                merge_param
            }
        }
    }

    fn lower_oxc_ternary(
        &mut self,
        cond: &js::ConditionalExpression<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        let cond_val = self.lower_oxc_expr(&cond.test, pp);
        let then_block = self.fb.create_block();
        let else_block = self.fb.create_block();
        let merge_block = self.fb.create_block();
        let merge_params = self.fb.add_block_params(merge_block, &[Type::Dynamic]);
        let merge_param = merge_params[0];
        self.fb
            .br_if(cond_val, then_block, &[], else_block, &[]);
        self.fb.switch_to_block(then_block);
        let then_val = self.lower_oxc_expr(&cond.consequent, pp);
        self.fb.br(merge_block, &[then_val]);
        self.fb.switch_to_block(else_block);
        let else_val = self.lower_oxc_expr(&cond.alternate, pp);
        self.fb.br(merge_block, &[else_val]);
        self.fb.switch_to_block(merge_block);
        merge_param
    }

    fn lower_oxc_assign(
        &mut self,
        assign: &js::AssignmentExpression<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        let rhs = self.lower_oxc_expr(&assign.right, pp);

        let final_val = if assign.operator != js::AssignmentOperator::Assign {
            // Compound assignment — load current, apply op
            let current = self.lower_oxc_assignment_target_val(&assign.left, pp);
            self.apply_oxc_compound_op(assign.operator, current, rhs)
        } else {
            rhs
        };

        self.store_to_oxc_assignment_target(&assign.left, final_val, pp);
        final_val
    }

    fn apply_oxc_compound_op(
        &mut self,
        op: js::AssignmentOperator,
        lhs: ValueId,
        rhs: ValueId,
    ) -> ValueId {
        match op {
            js::AssignmentOperator::Addition => self.fb.add(lhs, rhs),
            js::AssignmentOperator::Subtraction => self.fb.sub(lhs, rhs),
            js::AssignmentOperator::Multiplication => self.fb.mul(lhs, rhs),
            js::AssignmentOperator::Division => self.fb.div(lhs, rhs),
            js::AssignmentOperator::Remainder => self.fb.rem(lhs, rhs),
            js::AssignmentOperator::Exponential => {
                self.fb
                    .system_call("SugarCube.Engine", "pow", &[lhs, rhs], Type::Dynamic)
            }
            js::AssignmentOperator::BitwiseAnd => self.fb.bit_and(lhs, rhs),
            js::AssignmentOperator::BitwiseOR => self.fb.bit_or(lhs, rhs),
            js::AssignmentOperator::BitwiseXOR => self.fb.bit_xor(lhs, rhs),
            js::AssignmentOperator::ShiftLeft => self.fb.shl(lhs, rhs),
            js::AssignmentOperator::ShiftRight => self.fb.shr(lhs, rhs),
            js::AssignmentOperator::ShiftRightZeroFill => {
                self.fb
                    .system_call("SugarCube.Engine", "ushr", &[lhs, rhs], Type::Dynamic)
            }
            js::AssignmentOperator::LogicalAnd
            | js::AssignmentOperator::LogicalOr
            | js::AssignmentOperator::LogicalNullish => {
                // These are handled as logical short-circuit patterns,
                // but for compound assign just apply naively
                lhs // fallback
            }
            js::AssignmentOperator::Assign => rhs, // shouldn't reach here
        }
    }

    fn lower_oxc_assignment_target_val(
        &mut self,
        target: &js::AssignmentTarget<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        match target {
            js::AssignmentTarget::AssignmentTargetIdentifier(ident) => {
                let name = ident.name.as_str();
                if let Some(var_name) = name.strip_prefix('$') {
                    let n = self.fb.const_string(var_name);
                    self.fb
                        .system_call("SugarCube.State", "get", &[n], Type::Dynamic)
                } else if let Some(stripped) = name.strip_prefix('_') {
                    if !stripped.is_empty() {
                        let alloc = self.get_or_create_temp(stripped);
                        self.fb.load(alloc, Type::Dynamic)
                    } else {
                        self.fb.const_null()
                    }
                } else {
                    let n = self.fb.const_string(name);
                    self.fb
                        .system_call("SugarCube.Engine", "resolve", &[n], Type::Dynamic)
                }
            }
            js::AssignmentTarget::StaticMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                self.fb.get_field(obj, mem.property.name.as_str(), Type::Dynamic)
            }
            js::AssignmentTarget::ComputedMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                let idx = self.lower_oxc_expr(&mem.expression, pp);
                self.fb.get_index(obj, idx, Type::Dynamic)
            }
            _ => self.fb.const_null(),
        }
    }

    fn store_to_oxc_assignment_target(
        &mut self,
        target: &js::AssignmentTarget<'_>,
        value: ValueId,
        pp: &Preprocessed,
    ) {
        match target {
            js::AssignmentTarget::AssignmentTargetIdentifier(ident) => {
                let name = ident.name.as_str();
                if let Some(var_name) = name.strip_prefix('$') {
                    let n = self.fb.const_string(var_name);
                    self.fb
                        .system_call("SugarCube.State", "set", &[n, value], Type::Void);
                } else if let Some(stripped) = name.strip_prefix('_') {
                    if !stripped.is_empty() {
                        let alloc = self.get_or_create_temp(stripped);
                        self.fb.store(alloc, value);
                    }
                } else {
                    // Bare identifier assignment — unsupported, eval as side effect
                    let _ = value;
                }
            }
            js::AssignmentTarget::StaticMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                self.fb.set_field(obj, mem.property.name.as_str(), value);
            }
            js::AssignmentTarget::ComputedMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                let idx = self.lower_oxc_expr(&mem.expression, pp);
                self.fb.set_index(obj, idx, value);
            }
            _ => {
                // Destructuring etc — unsupported
            }
        }
    }

    /// Store to an update expression target (SimpleAssignmentTarget).
    fn store_to_oxc_target(
        &mut self,
        target: &js::SimpleAssignmentTarget<'_>,
        value: ValueId,
        pp: &Preprocessed,
    ) {
        match target {
            js::SimpleAssignmentTarget::AssignmentTargetIdentifier(ident) => {
                let name = ident.name.as_str();
                if let Some(var_name) = name.strip_prefix('$') {
                    let n = self.fb.const_string(var_name);
                    self.fb
                        .system_call("SugarCube.State", "set", &[n, value], Type::Void);
                } else if let Some(stripped) = name.strip_prefix('_') {
                    if !stripped.is_empty() {
                        let alloc = self.get_or_create_temp(stripped);
                        self.fb.store(alloc, value);
                    }
                }
            }
            js::SimpleAssignmentTarget::StaticMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                self.fb.set_field(obj, mem.property.name.as_str(), value);
            }
            js::SimpleAssignmentTarget::ComputedMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                let idx = self.lower_oxc_expr(&mem.expression, pp);
                self.fb.set_index(obj, idx, value);
            }
            _ => {}
        }
    }

    fn lower_template_literal(
        &mut self,
        tl: &js::TemplateLiteral<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        let mut result: Option<ValueId> = None;

        for (i, quasi) in tl.quasis.iter().enumerate() {
            // String part
            let text = quasi.value.raw.as_str();
            if !text.is_empty() {
                let s = self.fb.const_string(text);
                result = Some(match result {
                    Some(acc) => self.fb.add(acc, s),
                    None => s,
                });
            }
            // Expression part (if not the last quasi)
            if i < tl.expressions.len() {
                let val = self.lower_oxc_expr(&tl.expressions[i], pp);
                let str_val = self.fb.system_call(
                    "SugarCube.Engine",
                    "to_string",
                    &[val],
                    Type::String,
                );
                result = Some(match result {
                    Some(acc) => self.fb.add(acc, str_val),
                    None => str_val,
                });
            }
        }

        result.unwrap_or_else(|| self.fb.const_string(""))
    }

    fn lower_oxc_arrow(
        &mut self,
        arrow: &js::ArrowFunctionExpression<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        let arrow_name = format!("{}_arrow_{}", self.func_name, self.arrow_count);
        self.arrow_count += 1;

        let params: Vec<String> = arrow
            .params
            .items
            .iter()
            .filter_map(|p| {
                if let js::BindingPattern::BindingIdentifier(ident) = &p.pattern {
                    Some(ident.name.to_string())
                } else {
                    None
                }
            })
            .collect();

        let sig = FunctionSig {
            params: vec![Type::Dynamic; params.len()],
            return_ty: Type::Dynamic,
            defaults: vec![],
            has_rest_param: false,
        };
        let mut arrow_fb = FunctionBuilder::new(&arrow_name, sig, Visibility::Private);

        // Register parameter names
        let mut arrow_params = HashMap::new();
        for (idx, name) in params.iter().enumerate() {
            let v = arrow_fb.param(idx);
            arrow_fb.name_value(v, name.clone());
            arrow_params.insert(name.clone(), v);
        }

        let saved_fb = std::mem::replace(&mut self.fb, arrow_fb);
        let saved_temps = std::mem::take(&mut self.temp_vars);
        let saved_params = std::mem::replace(&mut self.local_params, arrow_params);

        // Lower arrow body
        if arrow.expression {
            // Expression body: `() => expr`
            if let Some(js::Statement::ExpressionStatement(es)) = arrow.body.statements.first() {
                let result = self.lower_oxc_expr(&es.expression, pp);
                self.fb.ret(Some(result));
            } else {
                self.fb.ret(None);
            }
        } else {
            // Block body: `() => { ... }` — lower each statement
            for stmt in &arrow.body.statements {
                self.lower_oxc_statement(stmt, pp);
            }
            self.fb.ret(None);
        }

        let built = std::mem::replace(&mut self.fb, saved_fb);
        self.temp_vars = saved_temps;
        self.local_params = saved_params;
        let mut func = built.build();
        func.method_kind = MethodKind::Closure;
        self.setter_callbacks.push(func);

        let name_val = self.fb.const_string(&arrow_name);
        self.fb
            .system_call("SugarCube.Engine", "closure", &[name_val], Type::Dynamic)
    }

    fn lower_oxc_statement(&mut self, stmt: &js::Statement<'_>, pp: &Preprocessed) {
        match stmt {
            js::Statement::ExpressionStatement(es) => {
                self.lower_oxc_expr(&es.expression, pp);
            }
            js::Statement::ReturnStatement(ret) => {
                let val = ret
                    .argument
                    .as_ref()
                    .map(|e| self.lower_oxc_expr(e, pp));
                self.fb.ret(val);
            }
            js::Statement::VariableDeclaration(decl) => {
                for d in &decl.declarations {
                    if let Some(init) = &d.init {
                        let val = self.lower_oxc_expr(init, pp);
                        if let js::BindingPattern::BindingIdentifier(ident) =
                            &d.id
                        {
                            let name = ident.name.as_str();
                            let alloc = self.get_or_create_temp(name);
                            self.fb.store(alloc, val);
                        }
                    }
                }
            }
            _ => {
                // Other statement types — ignore for now
            }
        }
    }

    fn lower_oxc_chain(
        &mut self,
        expr: &js::ChainElement<'_>,
        pp: &Preprocessed,
    ) -> ValueId {
        match expr {
            js::ChainElement::CallExpression(call) => {
                let callee = self.lower_oxc_expr(&call.callee, pp);
                let args: Vec<ValueId> = call
                    .arguments
                    .iter()
                    .map(|a| self.lower_oxc_argument(a, pp))
                    .collect();
                if call.optional {
                    // Optional call: callee?.()
                    let is_null = self.fb.system_call(
                        "SugarCube.Engine",
                        "is_nullish",
                        &[callee],
                        Type::Bool,
                    );
                    let call_block = self.fb.create_block();
                    let merge_block = self.fb.create_block();
                    let null = self.fb.const_null();
                    let merge_params =
                        self.fb.add_block_params(merge_block, &[Type::Dynamic]);
                    let merge_param = merge_params[0];
                    self.fb
                        .br_if(is_null, merge_block, &[null], call_block, &[]);
                    self.fb.switch_to_block(call_block);
                    let result = self.fb.call_indirect(callee, &args, Type::Dynamic);
                    self.fb.br(merge_block, &[result]);
                    self.fb.switch_to_block(merge_block);
                    merge_param
                } else {
                    self.fb.call_indirect(callee, &args, Type::Dynamic)
                }
            }
            js::ChainElement::StaticMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                if mem.optional {
                    let is_null = self.fb.system_call(
                        "SugarCube.Engine",
                        "is_nullish",
                        &[obj],
                        Type::Bool,
                    );
                    let access_block = self.fb.create_block();
                    let merge_block = self.fb.create_block();
                    let null = self.fb.const_null();
                    let merge_params =
                        self.fb.add_block_params(merge_block, &[Type::Dynamic]);
                    let merge_param = merge_params[0];
                    self.fb
                        .br_if(is_null, merge_block, &[null], access_block, &[]);
                    self.fb.switch_to_block(access_block);
                    let result =
                        self.fb.get_field(obj, mem.property.name.as_str(), Type::Dynamic);
                    self.fb.br(merge_block, &[result]);
                    self.fb.switch_to_block(merge_block);
                    merge_param
                } else {
                    self.fb.get_field(obj, mem.property.name.as_str(), Type::Dynamic)
                }
            }
            js::ChainElement::ComputedMemberExpression(mem) => {
                let obj = self.lower_oxc_expr(&mem.object, pp);
                let idx = self.lower_oxc_expr(&mem.expression, pp);
                if mem.optional {
                    let is_null = self.fb.system_call(
                        "SugarCube.Engine",
                        "is_nullish",
                        &[obj],
                        Type::Bool,
                    );
                    let access_block = self.fb.create_block();
                    let merge_block = self.fb.create_block();
                    let null = self.fb.const_null();
                    let merge_params =
                        self.fb.add_block_params(merge_block, &[Type::Dynamic]);
                    let merge_param = merge_params[0];
                    self.fb
                        .br_if(is_null, merge_block, &[null], access_block, &[]);
                    self.fb.switch_to_block(access_block);
                    let result = self.fb.get_index(obj, idx, Type::Dynamic);
                    self.fb.br(merge_block, &[result]);
                    self.fb.switch_to_block(merge_block);
                    merge_param
                } else {
                    self.fb.get_index(obj, idx, Type::Dynamic)
                }
            }
            js::ChainElement::PrivateFieldExpression(_) => {
                let m = self.fb.const_string("unsupported: private field");
                self.fb
                    .system_call("SugarCube.Engine", "error", &[m], Type::Dynamic)
            }
            js::ChainElement::TSNonNullExpression(e) => {
                // TypeScript non-null assertion (x!) — just lower the inner expression
                self.lower_oxc_expr(&e.expression, pp)
            }
        }
    }

    fn oxc_property_key_name(&mut self, key: &js::PropertyKey<'_>, _pp: &Preprocessed) -> String {
        match key {
            js::PropertyKey::StaticIdentifier(ident) => ident.name.to_string(),
            js::PropertyKey::StringLiteral(s) => s.value.to_string(),
            js::PropertyKey::NumericLiteral(n) => format!("{}", n.value),
            _ => {
                // Computed property — emit as __computed__
                "__computed__".to_string()
            }
        }
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
            NodeKind::Image { src, link } => {
                let src_val = self.fb.const_string(src.as_str());
                let mut args = vec![src_val];
                if let Some(link_target) = link {
                    let link_val = self.fb.const_string(link_target.as_str());
                    args.push(link_val);
                }
                self.fb
                    .system_call("SugarCube.Output", "image", &args, Type::Void);
            }
            NodeKind::HtmlOpen {
                tag,
                attrs,
                dynamic_attrs,
            } => self.emit_structured_open(tag, attrs, dynamic_attrs),
            NodeKind::HtmlClose(_) => self.emit_structured_close(),
            NodeKind::HtmlVoid {
                tag,
                attrs,
                dynamic_attrs,
            } => self.emit_structured_void(tag, attrs, dynamic_attrs),
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

        let mut args = vec![text_val, target_val];

        // Build setter callback if setters are present
        if !link.setters.is_empty() {
            let setter_name = format!("{}_setter_{}", self.func_name, self.setter_count);
            self.setter_count += 1;

            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void,
                defaults: vec![],
                has_rest_param: false,
            };
            let setter_fb = FunctionBuilder::new(&setter_name, sig, Visibility::Public);

            let saved_fb = std::mem::replace(&mut self.fb, setter_fb);
            let saved_temps = std::mem::take(&mut self.temp_vars);

            for setter_expr in &link.setters {
                self.lower_expr(setter_expr);
            }
            self.fb.ret(None);

            let built_fb = std::mem::replace(&mut self.fb, saved_fb);
            self.temp_vars = saved_temps;
            self.setter_callbacks.push(built_fb.build());

            let setter_ref = self.fb.global_ref(&setter_name, Type::Dynamic);
            args.push(setter_ref);
        }

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
        let src = self.source.clone();
        let text = expr.text(&src).trim();
        if let Some(var_name) = text.strip_prefix('$') {
            let n = self.fb.const_string(var_name);
            self.fb
                .system_call("SugarCube.State", "unset", &[n], Type::Void);
        } else if let Some(stripped) = text.strip_prefix('_') {
            if !stripped.is_empty() {
                let alloc = self.get_or_create_temp(stripped);
                let null = self.fb.const_null();
                self.fb.store(alloc, null);
            }
        } else {
            self.lower_expr(expr);
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
            self.fb.br(merge_block, &[]);
            return;
        }

        let clause = &clauses[index];

        if clause.kind == "else" {
            self.lower_nodes(&clause.body);
            self.fb.br(merge_block, &[]);
            return;
        }

        let cond = match &clause.args {
            MacroArgs::Expr(expr) => self.lower_expr(expr),
            _ => self.fb.const_bool(true),
        };

        let then_block = self.fb.create_block();
        let else_block = self.fb.create_block();

        self.fb
            .br_if(cond, then_block, &[], else_block, &[]);

        self.fb.switch_to_block(then_block);
        self.lower_nodes(&clause.body);
        self.fb.br(merge_block, &[]);

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
        if let Some(init_expr) = init {
            self.lower_expr(init_expr);
        }

        let header_block = self.fb.create_block();
        let body_block = self.fb.create_block();
        let latch_block = self.fb.create_block();
        let exit_block = self.fb.create_block();

        self.fb.br(header_block, &[]);

        self.fb.switch_to_block(header_block);
        let cond_val = if let Some(cond_expr) = cond {
            self.lower_expr(cond_expr)
        } else {
            self.fb.const_bool(true)
        };
        self.fb
            .br_if(cond_val, body_block, &[], exit_block, &[]);

        self.fb.switch_to_block(body_block);
        for clause in clauses {
            self.lower_nodes(&clause.body);
        }
        self.fb.br(latch_block, &[]);

        self.fb.switch_to_block(latch_block);
        if let Some(update_expr) = update {
            self.lower_expr(update_expr);
        }
        self.fb.br(header_block, &[]);

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

        self.fb.switch_to_block(header_block);
        let current = self.fb.load(alloc, Type::Dynamic);
        let cond = self.fb.cmp(CmpKind::Lt, current, end_val);
        self.fb
            .br_if(cond, body_block, &[], exit_block, &[]);

        self.fb.switch_to_block(body_block);
        for clause in clauses {
            self.lower_nodes(&clause.body);
        }
        self.fb.br(latch_block, &[]);

        self.fb.switch_to_block(latch_block);
        let cur = self.fb.load(alloc, Type::Dynamic);
        let one = self.fb.const_float(1.0);
        let next = self.fb.add(cur, one);
        self.fb.store(alloc, next);
        self.fb.br(header_block, &[]);

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

        self.fb.switch_to_block(header_block);
        let has_next = self.fb.system_call(
            "SugarCube.Engine",
            "iterator_has_next",
            &[iter],
            Type::Bool,
        );
        self.fb
            .br_if(has_next, body_block, &[], exit_block, &[]);

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

        self.fb.switch_to_block(exit_block);
    }

    fn lower_switch(&mut self, mac: &MacroNode) {
        let args = self.primary_args(mac).clone();
        let switch_val = match &args {
            MacroArgs::Switch(expr) => self.lower_expr(expr),
            _ => self.fb.const_null(),
        };

        let exit_block = self.fb.create_block();
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
                    if !clause.body.is_empty() {
                        case_blocks.push((vec![], block));
                    }
                }
            }
        }

        let default = default_block.unwrap_or(exit_block);
        self.lower_switch_chain(&case_blocks, switch_val, default, 0);

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
            self.lower_switch_chain(cases, switch_val, default_block, index + 1);
            return;
        }

        let mut combined: Option<ValueId> = None;
        for &v in vals {
            let eq = self.fb.cmp(CmpKind::Eq, switch_val, v);
            combined = Some(match combined {
                Some(prev) => self.fb.bit_or(prev, eq),
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
        match &mac.args {
            MacroArgs::Expr(expr) => {
                let target = self.lower_expr(expr);
                self.fb.system_call(
                    "SugarCube.Navigation",
                    "goto",
                    &[target],
                    Type::Void,
                );
            }
            MacroArgs::Raw(s) => {
                let target = self.fb.const_string(s.trim());
                self.fb.system_call(
                    "SugarCube.Navigation",
                    "goto",
                    &[target],
                    Type::Void,
                );
            }
            _ => {}
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

        self.fb.system_call(
            "SugarCube.Output",
            "link_block_start",
            &args,
            Type::Void,
        );

        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }

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
        let was = self.suppress_line_breaks;
        self.suppress_line_breaks = true;
        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }
        self.suppress_line_breaks = was;
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
        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }
    }

    fn lower_widget(&mut self, mac: &MacroNode) {
        let args = self.primary_args(mac).clone();
        if let MacroArgs::WidgetDef { name } = &args {
            let mut body_nodes = Vec::new();
            for clause in &mac.clauses {
                body_nodes.extend(clause.body.clone());
            }
            self.widgets.push((name.clone(), body_nodes, self.source.clone()));
        }
    }

    fn lower_script(&mut self, mac: &MacroNode) {
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

        self.fb
            .system_call("SugarCube.DOM", format!("{method}_start"), &args, Type::Void);

        for clause in &mac.clauses {
            self.lower_nodes(&clause.body);
        }

        self.fb
            .system_call("SugarCube.DOM", format!("{method}_end"), &[], Type::Void);
    }

    fn lower_input_macro(&mut self, mac: &MacroNode) {
        let method = mac.name.as_str();
        let args = self.collect_macro_args(mac);
        self.fb
            .system_call("SugarCube.Input", method, &args, Type::Void);

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

/// Result of translating a passage AST.
pub struct TranslateResult {
    /// The main passage function.
    pub func: Function,
    /// Widget definitions extracted from the passage (name, body, source).
    pub widgets: Vec<(String, Vec<Node>, String)>,
    /// Setter callback functions generated for link setters.
    pub setter_callbacks: Vec<Function>,
}

/// Translate a parsed passage AST into an IR Function.
pub fn translate_passage(name: &str, ast: &PassageAst) -> TranslateResult {
    let func_name = passage_func_name(name);
    let mut ctx = TranslateCtx::new(&func_name, &ast.source);

    ctx.lower_nodes(&ast.body);

    // Terminate the function if not already terminated
    ctx.fb.ret(None);

    let widgets = std::mem::take(&mut ctx.widgets);
    let setter_callbacks = std::mem::take(&mut ctx.setter_callbacks);
    TranslateResult {
        func: ctx.fb.build(),
        widgets,
        setter_callbacks,
    }
}

/// Translate a widget body into an IR Function plus any auxiliary functions.
pub fn translate_widget(name: &str, body: &[Node], source: &str) -> (Function, Vec<Function>) {
    let func_name = format!("widget_{name}");
    let mut ctx = TranslateCtx::new(&func_name, source);

    // Initialize _args from State
    let args_name = ctx.fb.const_string("_args");
    let args_val = ctx
        .fb
        .system_call("SugarCube.State", "get", &[args_name], Type::Dynamic);
    let alloc = ctx.get_or_create_temp("args");
    ctx.fb.store(alloc, args_val);

    ctx.lower_nodes(body);
    ctx.fb.ret(None);

    let callbacks = std::mem::take(&mut ctx.setter_callbacks);
    (ctx.fb.build(), callbacks)
}

/// Translate a user `<script>` block into an IR Function that evals the code.
pub fn translate_user_script(index: usize, code: &str) -> Function {
    let func_name = format!("__user_script_{index}");
    let mut ctx = TranslateCtx::new(&func_name, "");
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
        let result = translate_passage("test", &ast);
        result.func
    }

    #[test]
    fn plain_text_emits_output() {
        let func = translate("Hello, world!");
        assert!(!func.blocks.values().next().unwrap().insts.is_empty());
    }

    #[test]
    fn story_var_read() {
        let func = translate("<<print $name>>");
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
        assert!(func.blocks.len() >= 4, "expected at least 4 blocks, got {}", func.blocks.len());
    }

    #[test]
    fn widget_extraction() {
        let source = "<<widget \"myWidget\">>body<</widget>>";
        let ast = parser::parse(source);
        assert!(ast.errors.is_empty(), "parse errors: {:?}", ast.errors);
        assert_eq!(ast.body.len(), 1, "expected 1 node, got: {:#?}", ast.body);
        if let NodeKind::Macro(m) = &ast.body[0].kind {
            assert_eq!(m.name, "widget");
            assert!(!m.clauses.is_empty(), "expected clauses");
            assert!(
                matches!(&m.clauses[0].args, MacroArgs::WidgetDef { name } if name == "myWidget"),
                "unexpected clause args: {:?}", m.clauses[0].args
            );
        } else {
            panic!("expected Macro node, got: {:?}", ast.body[0].kind);
        }
        let result = translate_passage("test", &ast);
        assert_eq!(result.widgets.len(), 1);
        assert_eq!(result.widgets[0].0, "myWidget");
    }

    #[test]
    fn arrow_params_not_resolved() {
        let source = "<<run [1,2].forEach((x) => x + 1)>>";
        let ast = parser::parse(source);
        assert!(ast.errors.is_empty(), "parse errors: {:?}", ast.errors);
        let result = translate_passage("test", &ast);

        for cb in &result.setter_callbacks {
            for block in cb.blocks.values() {
                for &inst_id in &block.insts {
                    let inst = &cb.insts[inst_id];
                    if let reincarnate_core::ir::inst::Op::SystemCall { system, method, .. } =
                        &inst.op
                    {
                        assert!(
                            !(system == "SugarCube.Engine" && method == "resolve"),
                            "arrow param 'x' should not produce a resolve() call"
                        );
                    }
                }
            }
        }

        let arrow_cbs: Vec<_> = result
            .setter_callbacks
            .iter()
            .filter(|f| f.method_kind == MethodKind::Closure)
            .collect();
        assert!(
            !arrow_cbs.is_empty(),
            "expected arrow callback marked as Closure"
        );

        let has_closure_syscall = result.func.blocks.values().any(|block| {
            block.insts.iter().any(|&inst_id| {
                let inst = &result.func.insts[inst_id];
                matches!(
                    &inst.op,
                    reincarnate_core::ir::inst::Op::SystemCall {
                        system, method, ..
                    } if system == "SugarCube.Engine" && method == "closure"
                )
            })
        });
        assert!(
            has_closure_syscall,
            "parent function should contain a closure SystemCall"
        );
    }

    #[test]
    fn arrow_bare_param_not_resolved() {
        let source = "<<run [1,2].forEach(x => x + 1)>>";
        let ast = parser::parse(source);
        assert!(ast.errors.is_empty(), "parse errors: {:?}", ast.errors);
        let result = translate_passage("test", &ast);

        let arrow_cbs: Vec<_> = result
            .setter_callbacks
            .iter()
            .filter(|f| f.method_kind == MethodKind::Closure)
            .collect();
        assert!(
            !arrow_cbs.is_empty(),
            "bare-param arrow should produce a Closure callback"
        );

        for cb in &arrow_cbs {
            for block in cb.blocks.values() {
                for &inst_id in &block.insts {
                    let inst = &cb.insts[inst_id];
                    if let reincarnate_core::ir::inst::Op::SystemCall { system, method, .. } =
                        &inst.op
                    {
                        assert!(
                            !(system == "SugarCube.Engine" && method == "resolve"),
                            "bare-param arrow should not produce resolve() for param"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn passage_func_name_sanitization() {
        assert_eq!(passage_func_name("Start"), "passage_Start");
        assert_eq!(passage_func_name("My Passage"), "passage_My_Passage");
        assert_eq!(passage_func_name("a-b.c"), "passage_a_b_c");
    }
}
