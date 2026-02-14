use std::collections::HashMap;

use crate::error::CoreError;
use crate::ir::ty::{parse_type_notation, FunctionSig};
use crate::ir::{BlockId, Function, Module, Op, Type, ValueId};
use crate::pipeline::{Transform, TransformResult};

// ---------------------------------------------------------------------------
// Union-Find
// ---------------------------------------------------------------------------

/// A type variable index into the union-find.
type TVar = u32;

/// Error returned when two concrete types conflict during unification.
#[derive(Debug)]
struct TypeConflict {
    _a: Type,
    _b: Type,
}

/// Union-find with path compression, union-by-rank, and optional type binding.
struct UnionFind {
    parent: Vec<u32>,
    rank: Vec<u8>,
    /// Concrete type bound to a representative, if any.
    resolved: Vec<Option<Type>>,
}

impl UnionFind {
    fn new() -> Self {
        Self {
            parent: Vec::new(),
            rank: Vec::new(),
            resolved: Vec::new(),
        }
    }

    /// Allocate a fresh unbound type variable.
    fn fresh(&mut self) -> TVar {
        let id = self.parent.len() as u32;
        self.parent.push(id);
        self.rank.push(0);
        self.resolved.push(None);
        id
    }

    /// Allocate a fresh type variable pre-bound to a concrete type.
    fn fresh_with_type(&mut self, ty: Type) -> TVar {
        let id = self.parent.len() as u32;
        self.parent.push(id);
        self.rank.push(0);
        self.resolved.push(Some(ty));
        id
    }

    /// Find the representative of `x` with path compression.
    fn find(&mut self, x: TVar) -> TVar {
        let mut root = x;
        while self.parent[root as usize] != root {
            root = self.parent[root as usize];
        }
        // Path compression.
        let mut cur = x;
        while cur != root {
            let next = self.parent[cur as usize];
            self.parent[cur as usize] = root;
            cur = next;
        }
        root
    }

    /// Unify two type variables. If both are bound to concrete types that
    /// differ, returns `Err(TypeConflict)`.
    fn unify(&mut self, a: TVar, b: TVar) -> Result<(), TypeConflict> {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return Ok(());
        }

        // Merge resolved types.
        let merged = match (self.resolved[ra as usize].take(), self.resolved[rb as usize].take()) {
            (Some(ta), Some(tb)) => {
                if ta == tb {
                    Some(ta)
                } else {
                    // Conflict — restore both and return error.
                    self.resolved[ra as usize] = Some(ta.clone());
                    self.resolved[rb as usize] = Some(tb.clone());
                    return Err(TypeConflict { _a: ta, _b: tb });
                }
            }
            (Some(t), None) | (None, Some(t)) => Some(t),
            (None, None) => None,
        };

        // Union by rank.
        if self.rank[ra as usize] < self.rank[rb as usize] {
            self.parent[ra as usize] = rb;
            self.resolved[rb as usize] = merged;
        } else {
            self.parent[rb as usize] = ra;
            self.resolved[ra as usize] = merged;
            if self.rank[ra as usize] == self.rank[rb as usize] {
                self.rank[ra as usize] += 1;
            }
        }
        Ok(())
    }

    /// Resolve a type variable to its concrete type, if bound.
    fn resolve(&mut self, var: TVar) -> Option<Type> {
        let r = self.find(var);
        self.resolved[r as usize].clone()
    }
}

// ---------------------------------------------------------------------------
// FunctionSolver
// ---------------------------------------------------------------------------

/// Per-function solver state: maps IR values to type variables and collects
/// equality constraints between them.
struct FunctionSolver {
    uf: UnionFind,
    value_vars: HashMap<ValueId, TVar>,
    constraints: Vec<(TVar, TVar)>,
}

impl FunctionSolver {
    /// Build initial solver state from a function. Every value gets a type
    /// variable; concrete-typed values are pre-bound, `Dynamic` values are
    /// unbound.
    fn from_function(func: &Function) -> Self {
        let mut uf = UnionFind::new();
        let mut value_vars = HashMap::new();

        for (vid, ty) in func.value_types.iter() {
            let var = if *ty == Type::Dynamic {
                uf.fresh()
            } else {
                uf.fresh_with_type(ty.clone())
            };
            value_vars.insert(vid, var);
        }

        Self {
            uf,
            value_vars,
            constraints: Vec::new(),
        }
    }

    /// Get the type variable for a value (must exist).
    fn var_for(&self, v: ValueId) -> TVar {
        self.value_vars[&v]
    }

    /// Add an equality constraint between two values.
    fn constrain_equal_values(&mut self, a: ValueId, b: ValueId) {
        let va = self.var_for(a);
        let vb = self.var_for(b);
        self.constraints.push((va, vb));
    }

    /// Constrain a value to a concrete type (allocates a bound temp var).
    fn constrain_value_to_type(&mut self, v: ValueId, ty: &Type) {
        if *ty == Type::Dynamic {
            return;
        }
        let va = self.var_for(v);
        let tmp = self.uf.fresh_with_type(ty.clone());
        self.constraints.push((va, tmp));
    }
}

// ---------------------------------------------------------------------------
// ConstraintModuleContext
// ---------------------------------------------------------------------------

/// Module-level type context for the constraint solver.
/// Stores full `FunctionSig` (params + return) unlike the forward-only pass
/// which only stores return types.
struct ConstraintModuleContext {
    /// Function name → full signature.
    func_sigs: HashMap<String, FunctionSig>,
    /// Struct name → field name → field type.
    struct_fields: HashMap<String, HashMap<String, Type>>,
    /// (class_short_name, bare_method_name) → full signature.
    method_sigs: HashMap<(String, String), FunctionSig>,
    /// class_short_name → super_class_short_name.
    class_hierarchy: HashMap<String, Option<String>>,
    /// bare_method_name → full signature (only for unambiguous names).
    unique_method_sigs: HashMap<String, FunctionSig>,
}

impl ConstraintModuleContext {
    fn from_module(module: &Module) -> Self {
        let mut struct_fields = HashMap::new();
        for s in &module.structs {
            let fields: HashMap<String, Type> = s.fields.iter().map(|(n, t, _)| (n.clone(), t.clone())).collect();
            struct_fields.insert(s.name.clone(), fields);
        }

        let mut func_sigs: HashMap<String, FunctionSig> = module
            .functions
            .values()
            .map(|f| (f.name.clone(), f.sig.clone()))
            .collect();

        // Extend with external function signatures from runtime.
        for (name, ext_sig) in &module.external_function_sigs {
            func_sigs.entry(name.clone()).or_insert_with(|| FunctionSig {
                params: ext_sig
                    .params
                    .iter()
                    .map(|p| parse_type_notation(p))
                    .collect(),
                return_ty: parse_type_notation(&ext_sig.returns),
                ..Default::default()
            });
        }

        let mut method_sigs = HashMap::new();
        for f in module.functions.values() {
            if f.class.is_some() {
                if let Some(bare) = f.name.rsplit("::").next() {
                    if let Some(class) = &f.class {
                        method_sigs
                            .insert((class.clone(), bare.to_string()), f.sig.clone());
                    }
                }
            }
        }

        let mut class_hierarchy: HashMap<String, Option<String>> = HashMap::new();
        for class in &module.classes {
            let super_short = class
                .super_class
                .as_ref()
                .map(|sc| sc.rsplit("::").next().unwrap_or(sc).to_string());
            class_hierarchy.insert(class.name.clone(), super_short);
        }

        // Extend with external type definitions from runtime.
        for (name, ext) in &module.external_type_defs {
            // class_hierarchy
            class_hierarchy
                .entry(name.clone())
                .or_insert_with(|| ext.extends.clone());
            // struct_fields
            if !ext.fields.is_empty() {
                let fields: HashMap<String, Type> = ext
                    .fields
                    .iter()
                    .map(|(f, t)| (f.clone(), parse_type_notation(t)))
                    .collect();
                struct_fields.entry(name.clone()).or_default().extend(fields);
            }
            // method_sigs: build FunctionSig from external method signatures
            for (method, ext_sig) in &ext.methods {
                method_sigs
                    .entry((name.clone(), method.clone()))
                    .or_insert_with(|| FunctionSig {
                        params: ext_sig
                            .params
                            .iter()
                            .map(|p| parse_type_notation(p))
                            .collect(),
                        return_ty: parse_type_notation(&ext_sig.returns),
                        ..Default::default()
                    });
            }
        }

        // Build unique_method_sigs: bare names that resolve to a single signature.
        let mut bare_name_sigs: HashMap<String, Option<FunctionSig>> = HashMap::new();
        for ((_, bare), sig) in &method_sigs {
            match bare_name_sigs.get(bare) {
                None => {
                    bare_name_sigs.insert(bare.clone(), Some(sig.clone()));
                }
                Some(Some(existing)) if *existing == *sig => {}
                Some(Some(_)) => {
                    bare_name_sigs.insert(bare.clone(), None);
                }
                Some(None) => {}
            }
        }
        let unique_method_sigs = bare_name_sigs
            .into_iter()
            .filter_map(|(name, sig)| sig.map(|s| (name, s)))
            .collect();

        Self {
            func_sigs,
            struct_fields,
            method_sigs,
            class_hierarchy,
            unique_method_sigs,
        }
    }

    /// Resolve a method's signature by walking the class hierarchy, falling
    /// back to unique bare name. Same 3-strategy chain as `type_infer.rs`.
    fn resolve_func_sig(
        &self,
        name: &str,
        first_arg_ty: Option<&Type>,
        func_sig: &FunctionSig,
    ) -> Option<FunctionSig> {
        // Strategy 1: exact qualified name lookup.
        if let Some(sig) = self.func_sigs.get(name) {
            // Skip self-references (calling function would just return its own sig).
            if sig != func_sig || sig.params.iter().any(|p| *p != Type::Dynamic) {
                return Some(sig.clone());
            }
        }

        let bare = name.rsplit("::").next().unwrap_or(name);

        // Strategy 2: receiver-based — if first arg is Struct(class), walk hierarchy.
        if let Some(Type::Struct(class)) = first_arg_ty {
            if let Some(sig) = self.resolve_method_sig(class, bare) {
                return Some(sig);
            }
        }

        // Strategy 3: unique bare name fallback.
        self.unique_method_sigs.get(bare).cloned()
    }

    /// Walk class hierarchy to find method signature.
    fn resolve_method_sig(&self, class: &str, method: &str) -> Option<FunctionSig> {
        let mut current = Some(class.to_string());
        let max_depth = self.class_hierarchy.len();
        for _ in 0..=max_depth {
            let Some(cls) = current else { break };
            if let Some(sig) =
                self.method_sigs.get(&(cls.clone(), method.to_string()))
            {
                return Some(sig.clone());
            }
            current = self.class_hierarchy.get(&cls).and_then(|s| s.clone());
        }
        None
    }
}

// ---------------------------------------------------------------------------
// Constraint generation
// ---------------------------------------------------------------------------

/// Build a map from alloc ValueId → alloc type for Store constraints.
fn build_alloc_types_from_op(func: &Function) -> HashMap<ValueId, Type> {
    let mut alloc_types = HashMap::new();
    for block in func.blocks.values() {
        for &inst_id in &block.insts {
            let inst = &func.insts[inst_id];
            if let Op::Alloc(ty) = &inst.op {
                if let Some(result) = inst.result {
                    if *ty != Type::Dynamic {
                        alloc_types.insert(result, ty.clone());
                    }
                }
            }
        }
    }
    alloc_types
}

/// Constrain branch arguments to equal target block parameters.
fn constrain_branch_args(
    solver: &mut FunctionSolver,
    func: &Function,
    target: BlockId,
    args: &[ValueId],
) {
    let params = &func.blocks[target].params;
    for (arg, param) in args.iter().zip(params.iter()) {
        solver.constrain_equal_values(*arg, param.value);
    }
}

/// Walk all instructions in block order and generate equality constraints.
fn generate_constraints(
    solver: &mut FunctionSolver,
    func: &Function,
    ctx: &ConstraintModuleContext,
) {
    let alloc_types = build_alloc_types_from_op(func);

    for block in func.blocks.values() {
        for &inst_id in &block.insts {
            let inst = &func.insts[inst_id];
            let result = inst.result;

            match &inst.op {
                // Arithmetic: a = b, a = r
                Op::Add(a, b)
                | Op::Sub(a, b)
                | Op::Mul(a, b)
                | Op::Div(a, b)
                | Op::Rem(a, b) => {
                    solver.constrain_equal_values(*a, *b);
                    if let Some(r) = result {
                        solver.constrain_equal_values(*a, r);
                    }
                }
                Op::Neg(a) => {
                    if let Some(r) = result {
                        solver.constrain_equal_values(*a, r);
                    }
                }

                // Bitwise: a = b, a = r
                Op::BitAnd(a, b)
                | Op::BitOr(a, b)
                | Op::BitXor(a, b)
                | Op::Shl(a, b)
                | Op::Shr(a, b) => {
                    solver.constrain_equal_values(*a, *b);
                    if let Some(r) = result {
                        solver.constrain_equal_values(*a, r);
                    }
                }
                Op::BitNot(a) => {
                    if let Some(r) = result {
                        solver.constrain_equal_values(*a, r);
                    }
                }

                // Comparison: a = b, r = Bool
                Op::Cmp(_, a, b) => {
                    solver.constrain_equal_values(*a, *b);
                    if let Some(r) = result {
                        solver.constrain_value_to_type(r, &Type::Bool);
                    }
                }

                // Not: a = Bool, r = Bool
                Op::Not(a) => {
                    solver.constrain_value_to_type(*a, &Type::Bool);
                    if let Some(r) = result {
                        solver.constrain_value_to_type(r, &Type::Bool);
                    }
                }

                // Select: cond = Bool, on_true = on_false, on_true = r
                Op::Select {
                    cond,
                    on_true,
                    on_false,
                } => {
                    solver.constrain_value_to_type(*cond, &Type::Bool);
                    solver.constrain_equal_values(*on_true, *on_false);
                    if let Some(r) = result {
                        solver.constrain_equal_values(*on_true, r);
                    }
                }

                // Call: arg[i] = sig.params[i], r = sig.return_ty
                Op::Call { func: name, args } => {
                    let first_arg_ty = args
                        .first()
                        .map(|v| &func.value_types[*v]);
                    if let Some(sig) = ctx.resolve_func_sig(name, first_arg_ty, &func.sig) {
                        for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
                            solver.constrain_value_to_type(*arg, param_ty);
                        }
                        if let Some(r) = result {
                            solver.constrain_value_to_type(r, &sig.return_ty);
                        }
                    }
                }

                // Return: v = func.sig.return_ty
                Op::Return(Some(v)) => {
                    solver.constrain_value_to_type(*v, &func.sig.return_ty);
                }

                // BrIf: cond = Bool, branch args = target block params
                Op::BrIf {
                    cond,
                    then_target,
                    then_args,
                    else_target,
                    else_args,
                } => {
                    solver.constrain_value_to_type(*cond, &Type::Bool);
                    constrain_branch_args(solver, func, *then_target, then_args);
                    constrain_branch_args(solver, func, *else_target, else_args);
                }

                // Br: branch args = target block params
                Op::Br { target, args } => {
                    constrain_branch_args(solver, func, *target, args);
                }

                // Switch: branch args = target block params
                Op::Switch {
                    cases, default, ..
                } => {
                    for (_, target, args) in cases {
                        constrain_branch_args(solver, func, *target, args);
                    }
                    constrain_branch_args(solver, func, default.0, &default.1);
                }

                // Copy: v = r
                Op::Copy(v) => {
                    if let Some(r) = result {
                        solver.constrain_equal_values(*v, r);
                    }
                }

                // Cast: r = ty (no constraint on source)
                Op::Cast(_, ty, _) => {
                    if let Some(r) = result {
                        solver.constrain_value_to_type(r, ty);
                    }
                }

                // TypeCheck: r = Bool
                Op::TypeCheck(..) => {
                    if let Some(r) = result {
                        solver.constrain_value_to_type(r, &Type::Bool);
                    }
                }

                // SetField: value = field_ty (if struct type known)
                Op::SetField {
                    object,
                    field,
                    value,
                } => {
                    if let Type::Struct(name) = &func.value_types[*object] {
                        if let Some(field_ty) = ctx
                            .struct_fields
                            .get(name)
                            .and_then(|fields| fields.get(field))
                        {
                            solver.constrain_value_to_type(*value, field_ty);
                        }
                    }
                }

                // GetField: r = field_ty (if struct type known)
                Op::GetField { object, field } => {
                    if let Type::Struct(name) = &func.value_types[*object] {
                        if let Some(r) = result {
                            if let Some(field_ty) = ctx
                                .struct_fields
                                .get(name)
                                .and_then(|fields| fields.get(field))
                            {
                                solver.constrain_value_to_type(r, field_ty);
                            }
                        }
                    }
                }

                // StructInit: r = Struct(name), field values = field types
                Op::StructInit { name, fields } => {
                    if let Some(r) = result {
                        solver.constrain_value_to_type(r, &Type::Struct(name.clone()));
                    }
                    if let Some(field_defs) = ctx.struct_fields.get(name) {
                        for (fname, fval) in fields {
                            if let Some(fty) = field_defs.get(fname) {
                                solver.constrain_value_to_type(*fval, fty);
                            }
                        }
                    }
                }

                // ArrayInit: all elems[i] = elems[0]
                Op::ArrayInit(elems) => {
                    if elems.len() > 1 {
                        let first = elems[0];
                        for elem in &elems[1..] {
                            solver.constrain_equal_values(first, *elem);
                        }
                    }
                }

                // Store: if Alloc(ty) has ty != Dynamic, value = ty
                Op::Store { ptr, value } => {
                    if let Some(alloc_ty) = alloc_types.get(ptr) {
                        solver.constrain_value_to_type(*value, alloc_ty);
                    }
                }

                // MethodCall: constrain args to sig params if resolvable.
                Op::MethodCall {
                    receiver,
                    method: name,
                    args,
                } => {
                    let receiver_ty = Some(&func.value_types[*receiver]);
                    if let Some(sig) = ctx.resolve_func_sig(name, receiver_ty, &func.sig) {
                        // Skip the first param (receiver) in the signature.
                        for (arg, param_ty) in args.iter().zip(sig.params.iter().skip(1)) {
                            solver.constrain_value_to_type(*arg, param_ty);
                        }
                        if let Some(r) = result {
                            solver.constrain_value_to_type(r, &sig.return_ty);
                        }
                    }
                }

                // No additional constraints for these:
                Op::Const(_)
                | Op::Load(_)
                | Op::GlobalRef(_)
                | Op::SystemCall { .. }
                | Op::CallIndirect { .. }
                | Op::Alloc(_)
                | Op::Return(None)
                | Op::GetIndex { .. }
                | Op::SetIndex { .. }
                | Op::TupleInit(_)
                | Op::Yield(_)
                | Op::CoroutineCreate { .. }
                | Op::CoroutineResume(_) => {}
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Solve & apply
// ---------------------------------------------------------------------------

/// Run the constraint solver on a single function. Returns true if any types
/// were refined.
fn solve_function(func: &mut Function, ctx: &ConstraintModuleContext) -> bool {
    let mut solver = FunctionSolver::from_function(func);
    generate_constraints(&mut solver, func, ctx);

    // Solve: iterate constraints and unify, tracking conflicted representatives.
    let mut conflicted: std::collections::HashSet<TVar> = std::collections::HashSet::new();
    for (a, b) in solver.constraints.clone() {
        if solver.uf.unify(a, b).is_err() {
            // Mark both representatives as conflicted — any value in their
            // equivalence class should not be refined.
            conflicted.insert(solver.uf.find(a));
            conflicted.insert(solver.uf.find(b));
        }
    }

    // Collect updates: Dynamic values that now have concrete types and whose
    // representative is not conflicted.
    let mut changed = false;
    let updates: Vec<(ValueId, Type)> = solver
        .value_vars
        .iter()
        .filter_map(|(vid, &var)| {
            if func.value_types[*vid] != Type::Dynamic {
                return None;
            }
            let rep = solver.uf.find(var);
            if conflicted.contains(&rep) {
                return None;
            }
            let ty = solver.uf.resolve(var)?;
            if ty == Type::Dynamic {
                return None;
            }
            Some((*vid, ty))
        })
        .collect();

    for (vid, ty) in updates {
        func.value_types[vid] = ty;
        changed = true;
    }

    // Sync BlockParam.ty fields with value_types.
    for block in func.blocks.keys().collect::<Vec<_>>() {
        let param_vals: Vec<(usize, Type)> = func.blocks[block]
            .params
            .iter()
            .enumerate()
            .filter_map(|(i, p)| {
                let vty = &func.value_types[p.value];
                if p.ty != *vty {
                    Some((i, vty.clone()))
                } else {
                    None
                }
            })
            .collect();
        for (i, ty) in param_vals {
            func.blocks[block].params[i].ty = ty;
            changed = true;
        }
    }

    changed
}

// ---------------------------------------------------------------------------
// Transform impl
// ---------------------------------------------------------------------------

/// Constraint-based type inference pass. Runs after `TypeInference` to refine
/// remaining `Dynamic` types via unification of equality constraints.
pub struct ConstraintSolve;

impl Transform for ConstraintSolve {
    fn name(&self) -> &str {
        "constraint-solve"
    }

    fn apply(&self, mut module: Module) -> Result<TransformResult, CoreError> {
        let ctx = ConstraintModuleContext::from_module(&module);
        let mut changed = false;
        for func_id in module.functions.keys().collect::<Vec<_>>() {
            changed |= solve_function(&mut module.functions[func_id], &ctx);
        }
        Ok(TransformResult { module, changed })
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::EntityRef;
    use crate::ir::builder::{FunctionBuilder, ModuleBuilder};
    use crate::ir::ty::FunctionSig;
    use crate::ir::{ClassDef, FuncId, StructDef, Visibility};

    // ---- Identity & idempotency tests ----

    /// No type variables, all concrete → no changes.
    #[test]
    fn identity_no_change() {
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let p = fb.param(0);
        fb.ret(Some(p));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = ConstraintSolve.apply(module).unwrap();
        assert!(!result.changed);
    }

    /// Constraint solving is idempotent.
    #[test]
    fn idempotent_after_transform() {
        let callee_sig = FunctionSig {
            params: vec![Type::Int(32)],
            return_ty: Type::Void, ..Default::default() };
        let mut callee_fb = FunctionBuilder::new("foo", callee_sig, Visibility::Private);
        callee_fb.ret(None);
        let callee = callee_fb.build();

        let caller_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut caller_fb = FunctionBuilder::new("caller", caller_sig, Visibility::Private);
        let arg = caller_fb.param(0);
        caller_fb.call("foo", &[arg], Type::Void);
        caller_fb.ret(None);
        let caller = caller_fb.build();

        // Build module with both functions for idempotency test.
        let mut mb = ModuleBuilder::new("test");
        mb.add_function(callee);
        mb.add_function(caller);
        let module = mb.build();
        let r1 = ConstraintSolve.apply(module).unwrap();
        assert!(r1.changed);
        let r2 = ConstraintSolve.apply(r1.module).unwrap();
        assert!(!r2.changed);
    }

    // -- UnionFind unit tests --

    #[test]
    fn union_find_basic_unify() {
        let mut uf = UnionFind::new();
        let a = uf.fresh_with_type(Type::Int(32));
        let b = uf.fresh();
        assert!(uf.unify(a, b).is_ok());
        assert_eq!(uf.resolve(b), Some(Type::Int(32)));
    }

    #[test]
    fn union_find_path_compression() {
        let mut uf = UnionFind::new();
        let a = uf.fresh_with_type(Type::Bool);
        let b = uf.fresh();
        let c = uf.fresh();
        let d = uf.fresh();
        uf.unify(a, b).unwrap();
        uf.unify(b, c).unwrap();
        uf.unify(c, d).unwrap();
        // After find, d should resolve through compressed path.
        assert_eq!(uf.resolve(d), Some(Type::Bool));
    }

    #[test]
    fn union_find_conflict() {
        let mut uf = UnionFind::new();
        let a = uf.fresh_with_type(Type::Int(32));
        let b = uf.fresh_with_type(Type::String);
        assert!(uf.unify(a, b).is_err());
        // Both keep their original types.
        assert_eq!(uf.resolve(a), Some(Type::Int(32)));
        assert_eq!(uf.resolve(b), Some(Type::String));
    }

    #[test]
    fn union_find_same_type_ok() {
        let mut uf = UnionFind::new();
        let a = uf.fresh_with_type(Type::Int(64));
        let b = uf.fresh_with_type(Type::Int(64));
        assert!(uf.unify(a, b).is_ok());
        assert_eq!(uf.resolve(a), Some(Type::Int(64)));
    }

    // -- Integration tests --

    #[test]
    fn call_arg_backward_flow() {
        // foo(x: Int(32)) → Void. Caller passes Dynamic arg → should refine.
        let callee_sig = FunctionSig {
            params: vec![Type::Int(32)],
            return_ty: Type::Void, ..Default::default() };
        let mut callee_fb =
            FunctionBuilder::new("foo", callee_sig, Visibility::Private);
        callee_fb.ret(None);
        let callee = callee_fb.build();

        let caller_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut caller_fb =
            FunctionBuilder::new("caller", caller_sig, Visibility::Private);
        let arg = caller_fb.param(0);
        caller_fb.call("foo", &[arg], Type::Void);
        caller_fb.ret(None);
        let caller = caller_fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(callee);
        mb.add_function(caller);
        let module = mb.build();

        let transform = ConstraintSolve;
        let module = transform.apply(module).unwrap().module;

        let caller_func = &module.functions[FuncId::new(1)];
        assert_eq!(caller_func.value_types[arg], Type::Int(32));
    }

    #[test]
    fn return_backward_flow() {
        // fn returning Int(32), returns a Dynamic param → param should refine.
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Int(32), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let p = fb.param(0);
        fb.ret(Some(p));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = ConstraintSolve;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[p], Type::Int(32));
    }

    #[test]
    fn arithmetic_equalization() {
        // Add(v_dynamic, v_int64) → v_dynamic should become Int(64).
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let a = fb.param(0); // Dynamic
        let b = fb.const_int(42); // Int(64)
        let sum = fb.add(a, b);
        fb.ret(Some(sum));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = ConstraintSolve;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[a], Type::Int(64));
    }

    #[test]
    fn brif_cond_refined_to_bool() {
        // BrIf on a Dynamic cond → should become Bool.
        let sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0); // Dynamic
        let then_b = fb.create_block();
        let else_b = fb.create_block();
        fb.br_if(cond, then_b, &[], else_b, &[]);

        fb.switch_to_block(then_b);
        fb.ret(None);
        fb.switch_to_block(else_b);
        fb.ret(None);
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = ConstraintSolve;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[cond], Type::Bool);
    }

    #[test]
    fn transitive_constraint_flow() {
        // v1 = param(Dynamic), v2 = copy(v1), call("foo", [v2]) where foo
        // expects Int(32) → both v1 and v2 should become Int(32).
        let callee_sig = FunctionSig {
            params: vec![Type::Int(32)],
            return_ty: Type::Void, ..Default::default() };
        let mut callee_fb =
            FunctionBuilder::new("foo", callee_sig, Visibility::Private);
        callee_fb.ret(None);
        let callee = callee_fb.build();

        let caller_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut caller_fb =
            FunctionBuilder::new("caller", caller_sig, Visibility::Private);
        let v1 = caller_fb.param(0);
        let v2 = caller_fb.copy(v1);
        caller_fb.call("foo", &[v2], Type::Void);
        caller_fb.ret(None);
        let caller = caller_fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(callee);
        mb.add_function(caller);
        let module = mb.build();

        let transform = ConstraintSolve;
        let module = transform.apply(module).unwrap().module;

        let caller_func = &module.functions[FuncId::new(1)];
        assert_eq!(caller_func.value_types[v1], Type::Int(32));
        assert_eq!(caller_func.value_types[v2], Type::Int(32));
    }

    #[test]
    fn conflict_stays_dynamic() {
        // Value constrained to both Int(32) (by call) and String (by return) → stays Dynamic.
        let callee_sig = FunctionSig {
            params: vec![Type::Int(32)],
            return_ty: Type::Void, ..Default::default() };
        let mut callee_fb =
            FunctionBuilder::new("foo", callee_sig, Visibility::Private);
        callee_fb.ret(None);
        let callee = callee_fb.build();

        let caller_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::String, ..Default::default() };
        let mut caller_fb =
            FunctionBuilder::new("caller", caller_sig, Visibility::Private);
        let p = caller_fb.param(0);
        caller_fb.call("foo", &[p], Type::Void);
        caller_fb.ret(Some(p));
        let caller = caller_fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(callee);
        mb.add_function(caller);
        let module = mb.build();

        let transform = ConstraintSolve;
        let module = transform.apply(module).unwrap().module;

        let caller_func = &module.functions[FuncId::new(1)];
        // Conflict — stays Dynamic.
        assert_eq!(caller_func.value_types[p], Type::Dynamic);
    }

    #[test]
    fn concrete_values_preserved() {
        // Already-typed values should remain unchanged.
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let p = fb.param(0); // Int(64)
        let c = fb.const_int(42); // Int(64)
        let sum = fb.add(p, c);
        fb.ret(Some(sum));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = ConstraintSolve;
        let result = transform.apply(module).unwrap();

        let func = &result.module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[p], Type::Int(64));
        assert_eq!(func.value_types[c], Type::Int(64));
        assert_eq!(func.value_types[sum], Type::Int(64));
        assert!(!result.changed);
    }

    #[test]
    fn block_param_from_branch() {
        // Branch with concrete arg → target block param should be refined.
        let sig = FunctionSig {
            params: vec![Type::Bool],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let cond = fb.param(0);

        let (merge, merge_vals) = fb.create_block_with_params(&[Type::Dynamic]);
        let then_b = fb.create_block();
        let else_b = fb.create_block();
        fb.br_if(cond, then_b, &[], else_b, &[]);

        fb.switch_to_block(then_b);
        let a = fb.const_int(1);
        fb.br(merge, &[a]);

        fb.switch_to_block(else_b);
        let b = fb.const_int(2);
        fb.br(merge, &[b]);

        fb.switch_to_block(merge);
        fb.ret(Some(merge_vals[0]));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = ConstraintSolve;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[merge_vals[0]], Type::Int(64));
        assert_eq!(func.blocks[merge].params[0].ty, Type::Int(64));
    }

    #[test]
    fn noop_on_fully_typed() {
        // Module with no Dynamic values → changed = false.
        let sig = FunctionSig {
            params: vec![Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let p = fb.param(0);
        fb.ret(Some(p));
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(func);
        let module = mb.build();

        let transform = ConstraintSolve;
        let result = transform.apply(module).unwrap();
        assert!(!result.changed);
    }

    #[test]
    fn method_call_backward_flow() {
        // Method call: Creature::isAlive(self: Struct("Creature")) → Bool.
        // Caller calls "isAlive" with Dynamic receiver and uses result as Dynamic.
        // → receiver should stay Dynamic (no constraint on receiver type), but
        //   result should become Bool via unique method sig fallback.
        let method_sig = FunctionSig {
            params: vec![Type::Struct("Creature".to_string())],
            return_ty: Type::Bool, ..Default::default() };
        let mut method_fb =
            FunctionBuilder::new("Creature::isAlive", method_sig, Visibility::Public);
        let self_param = method_fb.param(0);
        method_fb.ret(Some(self_param));
        let mut method_func = method_fb.build();
        method_func.class = Some("Creature".to_string());

        let caller_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::Dynamic, ..Default::default() };
        let mut caller_fb =
            FunctionBuilder::new("caller", caller_sig, Visibility::Public);
        let recv = caller_fb.param(0);
        let result = caller_fb.call("isAlive", &[recv], Type::Dynamic);
        caller_fb.ret(Some(result));
        let caller_func = caller_fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_struct(StructDef {
            name: "Creature".into(),
            namespace: Vec::new(),
            fields: vec![],
            visibility: Visibility::Public,
        });
        let method_id = mb.add_function(method_func);
        mb.add_function(caller_func);
        mb.add_class(ClassDef {
            name: "Creature".into(),
            namespace: Vec::new(),
            struct_index: 0,
            methods: vec![method_id],
            super_class: None,
            visibility: Visibility::Public,
            static_fields: vec![],
            is_interface: false,
            interfaces: vec![],
        });
        let module = mb.build();

        let transform = ConstraintSolve;
        let module = transform.apply(module).unwrap().module;

        let caller = &module.functions[FuncId::new(1)];
        // Result gets Bool from method sig's return type.
        assert_eq!(caller.value_types[result], Type::Bool);
        // Receiver gets Struct("Creature") from method sig's param type.
        assert_eq!(
            caller.value_types[recv],
            Type::Struct("Creature".to_string())
        );
    }

    // ---- Edge case tests ----

    /// No type variables (all concrete) → unchanged.
    #[test]
    fn no_type_vars_noop() {
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Bool],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let p = fb.param(0);
        let c = fb.const_int(42);
        let sum = fb.add(p, c);
        fb.ret(Some(sum));

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(fb.build());
        let module = mb.build();
        let result = ConstraintSolve.apply(module).unwrap();
        assert!(!result.changed);
    }

    /// Conflicting constraints (Int vs String) → stays Dynamic.
    #[test]
    fn conflicting_constraints_fallback() {
        let callee_int = FunctionSig {
            params: vec![Type::Int(32)],
            return_ty: Type::Void, ..Default::default() };
        let mut fb1 = FunctionBuilder::new("want_int", callee_int, Visibility::Private);
        fb1.ret(None);
        let callee1 = fb1.build();

        let caller_sig = FunctionSig {
            params: vec![Type::Dynamic],
            return_ty: Type::String, ..Default::default() };
        let mut fb2 = FunctionBuilder::new("caller", caller_sig, Visibility::Private);
        let p = fb2.param(0);
        fb2.call("want_int", &[p], Type::Void);
        fb2.ret(Some(p)); // return type is String
        let caller = fb2.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_function(callee1);
        mb.add_function(caller);
        let module = mb.build();
        let result = ConstraintSolve.apply(module).unwrap();
        let func = &result.module.functions[FuncId::new(1)];
        assert_eq!(func.value_types[p], Type::Dynamic, "conflicting constraints → Dynamic");
    }

    #[test]
    fn set_field_constrains_value() {
        // SetField on a known struct constrains the value to the field type.
        let sig = FunctionSig {
            params: vec![Type::Struct("Point".to_string()), Type::Dynamic],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("test", sig, Visibility::Private);
        let obj = fb.param(0);
        let val = fb.param(1); // Dynamic
        fb.set_field(obj, "x", val);
        fb.ret(None);
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test");
        mb.add_struct(StructDef {
            name: "Point".into(),
            namespace: Vec::new(),
            fields: vec![
                ("x".into(), Type::Int(64), None),
                ("y".into(), Type::Int(64), None),
            ],
            visibility: Visibility::Public,
        });
        mb.add_function(func);
        let module = mb.build();

        let transform = ConstraintSolve;
        let module = transform.apply(module).unwrap().module;

        let func = &module.functions[FuncId::new(0)];
        assert_eq!(func.value_types[val], Type::Int(64));
    }
}
