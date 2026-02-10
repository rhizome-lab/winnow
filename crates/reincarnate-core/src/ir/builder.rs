use std::collections::HashMap;

use crate::entity::PrimaryMap;

use super::block::{Block, BlockId, BlockParam};
use super::func::{FuncId, Function, Visibility};
use super::inst::{CmpKind, Inst, Op};
use super::func::MethodKind;
use super::module::{ClassDef, EnumDef, EntryPoint, ExternalImport, Global, Import, Module, StructDef};
use super::ty::{FunctionSig, Type};
use super::value::{Constant, ValueId};

/// Builder for constructing a single [`Function`].
///
/// Manages value allocation, block creation, and instruction emission.
/// Tracks a "current block" cursor — instructions are appended to it.
pub struct FunctionBuilder {
    func: Function,
    current_block: BlockId,
}

impl FunctionBuilder {
    /// Create a new function builder.
    ///
    /// Creates the entry block and allocates `ValueId`s for each parameter.
    pub fn new(name: impl Into<String>, sig: FunctionSig, visibility: Visibility) -> Self {
        let mut blocks = PrimaryMap::new();
        let mut value_types = PrimaryMap::new();

        // Create entry block with params matching the function signature.
        let mut params = Vec::with_capacity(sig.params.len());
        for ty in &sig.params {
            let value = value_types.push(ty.clone());
            params.push(BlockParam {
                value,
                ty: ty.clone(),
            });
        }
        let entry = blocks.push(Block {
            params,
            insts: Vec::new(),
        });

        let func = Function {
            name: name.into(),
            sig,
            visibility,
            namespace: Vec::new(),
            class: None,
            method_kind: MethodKind::Free,
            blocks,
            insts: PrimaryMap::new(),
            value_types,
            entry,
            coroutine: None,
            value_names: HashMap::new(),
        };

        Self {
            func,
            current_block: entry,
        }
    }

    /// Create a new block with no parameters. Returns its `BlockId`.
    pub fn create_block(&mut self) -> BlockId {
        self.func.blocks.push(Block {
            params: Vec::new(),
            insts: Vec::new(),
        })
    }

    /// Create a new block with the given parameter types.
    /// Returns the `BlockId` and `ValueId`s for each parameter.
    pub fn create_block_with_params(&mut self, types: &[Type]) -> (BlockId, Vec<ValueId>) {
        let mut params = Vec::with_capacity(types.len());
        let mut values = Vec::with_capacity(types.len());
        for ty in types {
            let value = self.func.value_types.push(ty.clone());
            params.push(BlockParam {
                value,
                ty: ty.clone(),
            });
            values.push(value);
        }
        let block = self.func.blocks.push(Block {
            params,
            insts: Vec::new(),
        });
        (block, values)
    }

    /// Switch the current block cursor to the given block.
    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = block;
    }

    /// Get the current block.
    pub fn current_block(&self) -> BlockId {
        self.current_block
    }

    /// Get the entry block.
    pub fn entry_block(&self) -> BlockId {
        self.func.entry
    }

    /// Get the `ValueId` for a function parameter by index.
    ///
    /// # Panics
    /// Panics if `index` is out of range.
    pub fn param(&self, index: usize) -> ValueId {
        self.func.blocks[self.func.entry].params[index].value
    }

    /// Set class metadata on the function being built.
    pub fn set_class(&mut self, ns: Vec<String>, class: String, kind: MethodKind) {
        self.func.namespace = ns;
        self.func.class = Some(class);
        self.func.method_kind = kind;
    }

    /// Attach a debug name to a value (from source-level variable/parameter names).
    pub fn name_value(&mut self, v: ValueId, name: String) {
        self.func.value_names.insert(v, name);
    }

    /// Check whether a value already has a debug name.
    pub fn has_name(&self, v: ValueId) -> bool {
        self.func.value_names.contains_key(&v)
    }

    /// Consume the builder and return the constructed `Function`.
    pub fn build(self) -> Function {
        self.func
    }

    // -- internal helpers --

    /// Push an instruction with a result value into the current block.
    fn emit(&mut self, op: Op, ty: Type) -> ValueId {
        let value = self.func.value_types.push(ty);
        let inst_id = self.func.insts.push(Inst {
            op,
            result: Some(value),
            span: None,
        });
        self.func.blocks[self.current_block].insts.push(inst_id);
        value
    }

    /// Push a void instruction (no result value) into the current block.
    fn emit_void(&mut self, op: Op) {
        let inst_id = self.func.insts.push(Inst {
            op,
            result: None,
            span: None,
        });
        self.func.blocks[self.current_block].insts.push(inst_id);
    }

    /// Add parameters to an existing block. Returns `ValueId`s for each new parameter.
    ///
    /// Useful when translating stack-based bytecode where merge-point types
    /// are discovered during translation, not before block creation.
    pub fn add_block_params(&mut self, block: BlockId, types: &[Type]) -> Vec<ValueId> {
        let mut values = Vec::with_capacity(types.len());
        for ty in types {
            let value = self.func.value_types.push(ty.clone());
            self.func.blocks[block].params.push(BlockParam {
                value,
                ty: ty.clone(),
            });
            values.push(value);
        }
        values
    }

    /// Look up the type of a value.
    pub fn value_type(&self, value: ValueId) -> Type {
        self.func.value_types[value].clone()
    }

    // ========================================================================
    // Constants
    // ========================================================================

    pub fn const_null(&mut self) -> ValueId {
        let c = Constant::Null;
        let ty = c.ty();
        self.emit(Op::Const(c), ty)
    }

    pub fn const_bool(&mut self, value: bool) -> ValueId {
        let c = Constant::Bool(value);
        let ty = c.ty();
        self.emit(Op::Const(c), ty)
    }

    pub fn const_int(&mut self, value: i64) -> ValueId {
        let c = Constant::Int(value);
        let ty = c.ty();
        self.emit(Op::Const(c), ty)
    }

    pub fn const_uint(&mut self, value: u64) -> ValueId {
        let c = Constant::UInt(value);
        let ty = c.ty();
        self.emit(Op::Const(c), ty)
    }

    pub fn const_float(&mut self, value: f64) -> ValueId {
        let c = Constant::Float(value);
        let ty = c.ty();
        self.emit(Op::Const(c), ty)
    }

    pub fn const_string(&mut self, value: impl Into<String>) -> ValueId {
        let c = Constant::String(value.into());
        let ty = c.ty();
        self.emit(Op::Const(c), ty)
    }

    // ========================================================================
    // Arithmetic
    // ========================================================================

    pub fn add(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::Add(a, b), ty)
    }

    pub fn sub(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::Sub(a, b), ty)
    }

    pub fn mul(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::Mul(a, b), ty)
    }

    pub fn div(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::Div(a, b), ty)
    }

    pub fn rem(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::Rem(a, b), ty)
    }

    pub fn neg(&mut self, a: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::Neg(a), ty)
    }

    // ========================================================================
    // Bitwise
    // ========================================================================

    pub fn bit_and(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::BitAnd(a, b), ty)
    }

    pub fn bit_or(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::BitOr(a, b), ty)
    }

    pub fn bit_xor(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::BitXor(a, b), ty)
    }

    pub fn bit_not(&mut self, a: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::BitNot(a), ty)
    }

    pub fn shl(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::Shl(a, b), ty)
    }

    pub fn shr(&mut self, a: ValueId, b: ValueId) -> ValueId {
        let ty = self.value_type(a);
        self.emit(Op::Shr(a, b), ty)
    }

    // ========================================================================
    // Comparison & logic
    // ========================================================================

    pub fn cmp(&mut self, kind: CmpKind, a: ValueId, b: ValueId) -> ValueId {
        self.emit(Op::Cmp(kind, a, b), Type::Bool)
    }

    pub fn not(&mut self, a: ValueId) -> ValueId {
        self.emit(Op::Not(a), Type::Bool)
    }

    // ========================================================================
    // Control flow
    // ========================================================================

    pub fn br(&mut self, target: BlockId, args: &[ValueId]) {
        self.emit_void(Op::Br {
            target,
            args: args.to_vec(),
        });
    }

    pub fn br_if(
        &mut self,
        cond: ValueId,
        then_target: BlockId,
        then_args: &[ValueId],
        else_target: BlockId,
        else_args: &[ValueId],
    ) {
        self.emit_void(Op::BrIf {
            cond,
            then_target,
            then_args: then_args.to_vec(),
            else_target,
            else_args: else_args.to_vec(),
        });
    }

    pub fn switch(
        &mut self,
        value: ValueId,
        cases: Vec<(Constant, BlockId, Vec<ValueId>)>,
        default: (BlockId, Vec<ValueId>),
    ) {
        self.emit_void(Op::Switch {
            value,
            cases,
            default,
        });
    }

    pub fn ret(&mut self, value: Option<ValueId>) {
        self.emit_void(Op::Return(value));
    }

    // ========================================================================
    // Memory / fields
    // ========================================================================

    pub fn alloc(&mut self, ty: Type) -> ValueId {
        self.emit(Op::Alloc(ty), Type::Dynamic)
    }

    pub fn load(&mut self, ptr: ValueId, ty: Type) -> ValueId {
        self.emit(Op::Load(ptr), ty)
    }

    pub fn store(&mut self, ptr: ValueId, value: ValueId) {
        self.emit_void(Op::Store { ptr, value });
    }

    pub fn get_field(&mut self, object: ValueId, field: impl Into<String>, ty: Type) -> ValueId {
        self.emit(
            Op::GetField {
                object,
                field: field.into(),
            },
            ty,
        )
    }

    pub fn set_field(&mut self, object: ValueId, field: impl Into<String>, value: ValueId) {
        self.emit_void(Op::SetField {
            object,
            field: field.into(),
            value,
        });
    }

    pub fn get_index(&mut self, collection: ValueId, index: ValueId, ty: Type) -> ValueId {
        self.emit(Op::GetIndex { collection, index }, ty)
    }

    pub fn set_index(&mut self, collection: ValueId, index: ValueId, value: ValueId) {
        self.emit_void(Op::SetIndex {
            collection,
            index,
            value,
        });
    }

    // ========================================================================
    // Calls
    // ========================================================================

    pub fn call(&mut self, func: impl Into<String>, args: &[ValueId], ret_ty: Type) -> ValueId {
        self.emit(
            Op::Call {
                func: func.into(),
                args: args.to_vec(),
            },
            ret_ty,
        )
    }

    pub fn call_indirect(&mut self, callee: ValueId, args: &[ValueId], ret_ty: Type) -> ValueId {
        self.emit(
            Op::CallIndirect {
                callee,
                args: args.to_vec(),
            },
            ret_ty,
        )
    }

    pub fn system_call(
        &mut self,
        system: impl Into<String>,
        method: impl Into<String>,
        args: &[ValueId],
        ret_ty: Type,
    ) -> ValueId {
        self.emit(
            Op::SystemCall {
                system: system.into(),
                method: method.into(),
                args: args.to_vec(),
            },
            ret_ty,
        )
    }

    // ========================================================================
    // Type operations
    // ========================================================================

    pub fn cast(&mut self, value: ValueId, ty: Type) -> ValueId {
        self.emit(Op::Cast(value, ty.clone()), ty)
    }

    pub fn type_check(&mut self, value: ValueId, ty: Type) -> ValueId {
        self.emit(Op::TypeCheck(value, ty), Type::Bool)
    }

    // ========================================================================
    // Aggregate construction
    // ========================================================================

    pub fn struct_init(
        &mut self,
        name: impl Into<String>,
        fields: Vec<(String, ValueId)>,
    ) -> ValueId {
        let name = name.into();
        let ty = Type::Struct(name.clone());
        self.emit(Op::StructInit { name, fields }, ty)
    }

    pub fn array_init(&mut self, elements: &[ValueId], elem_ty: Type) -> ValueId {
        let ty = Type::Array(Box::new(elem_ty));
        self.emit(Op::ArrayInit(elements.to_vec()), ty)
    }

    pub fn tuple_init(&mut self, elements: &[ValueId], types: Vec<Type>) -> ValueId {
        let ty = Type::Tuple(types);
        self.emit(Op::TupleInit(elements.to_vec()), ty)
    }

    // ========================================================================
    // Coroutines
    // ========================================================================

    pub fn yield_(&mut self, value: Option<ValueId>, resume_ty: Type) -> ValueId {
        self.emit(Op::Yield(value), resume_ty)
    }

    pub fn coroutine_create(
        &mut self,
        func: impl Into<String>,
        args: &[ValueId],
        yield_ty: Type,
        return_ty: Type,
    ) -> ValueId {
        let ty = Type::Coroutine {
            yield_ty: Box::new(yield_ty),
            return_ty: Box::new(return_ty),
        };
        self.emit(
            Op::CoroutineCreate {
                func: func.into(),
                args: args.to_vec(),
            },
            ty,
        )
    }

    pub fn coroutine_resume(&mut self, coroutine: ValueId, yield_ty: Type) -> ValueId {
        self.emit(Op::CoroutineResume(coroutine), yield_ty)
    }

    // ========================================================================
    // Misc
    // ========================================================================

    pub fn global_ref(&mut self, name: impl Into<String>, ty: Type) -> ValueId {
        self.emit(Op::GlobalRef(name.into()), ty)
    }

    pub fn copy(&mut self, value: ValueId) -> ValueId {
        let ty = self.value_type(value);
        self.emit(Op::Copy(value), ty)
    }
}

/// Builder for constructing a [`Module`].
pub struct ModuleBuilder {
    module: Module,
}

impl ModuleBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            module: Module::new(name.into()),
        }
    }

    pub fn add_function(&mut self, func: Function) -> FuncId {
        self.module.functions.push(func)
    }

    pub fn add_struct(&mut self, def: StructDef) {
        self.module.structs.push(def);
    }

    pub fn struct_count(&self) -> usize {
        self.module.structs.len()
    }

    pub fn add_enum(&mut self, def: EnumDef) {
        self.module.enums.push(def);
    }

    pub fn add_global(&mut self, global: Global) {
        self.module.globals.push(global);
    }

    pub fn add_import(&mut self, import: Import) {
        self.module.imports.push(import);
    }

    pub fn add_class(&mut self, class: ClassDef) {
        self.module.classes.push(class);
    }

    pub fn set_entry_point(&mut self, entry: EntryPoint) {
        self.module.entry_point = Some(entry);
    }

    pub fn add_external_import(&mut self, qualified_name: String, import: ExternalImport) {
        self.module.external_imports.insert(qualified_name, import);
    }

    pub fn build(self) -> Module {
        self.module
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_simple_add_function() {
        // Build: fn add(a: Int(64), b: Int(64)) -> Int(64) { return a + b }
        let sig = FunctionSig {
            params: vec![Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("add", sig, Visibility::Public);

        let a = fb.param(0);
        let b = fb.param(1);
        let sum = fb.add(a, b);
        fb.ret(Some(sum));

        let func = fb.build();

        assert_eq!(func.name, "add");
        assert_eq!(func.sig.params.len(), 2);
        assert_eq!(func.sig.return_ty, Type::Int(64));

        // Entry block should have 2 params and 2 instructions (add + return).
        let entry = &func.blocks[func.entry];
        assert_eq!(entry.params.len(), 2);
        assert_eq!(entry.insts.len(), 2);

        // The add instruction should have a result.
        let add_inst = &func.insts[entry.insts[0]];
        assert!(add_inst.result.is_some());
        assert!(matches!(add_inst.op, Op::Add(_, _)));

        // The return instruction should have no result.
        let ret_inst = &func.insts[entry.insts[1]];
        assert!(ret_inst.result.is_none());
        assert!(matches!(ret_inst.op, Op::Return(Some(_))));

        // Value types: 2 params + 1 add result = 3.
        assert_eq!(func.value_types.len(), 3);
    }

    #[test]
    fn build_branching_function() {
        // Build: fn choose(cond: Bool, x: Int(64), y: Int(64)) -> Int(64)
        //   entry: br_if cond, then(x), else(y)
        //   then(v): return v
        //   else(v): return v
        let sig = FunctionSig {
            params: vec![Type::Bool, Type::Int(64), Type::Int(64)],
            return_ty: Type::Int(64), ..Default::default() };
        let mut fb = FunctionBuilder::new("choose", sig, Visibility::Public);

        let cond = fb.param(0);
        let x = fb.param(1);
        let y = fb.param(2);

        let (then_block, then_vals) =
            fb.create_block_with_params(&[Type::Int(64)]);
        let (else_block, else_vals) =
            fb.create_block_with_params(&[Type::Int(64)]);

        fb.br_if(cond, then_block, &[x], else_block, &[y]);

        fb.switch_to_block(then_block);
        fb.ret(Some(then_vals[0]));

        fb.switch_to_block(else_block);
        fb.ret(Some(else_vals[0]));

        let func = fb.build();

        assert_eq!(func.blocks.len(), 3);
        // Entry has 3 params, then/else each have 1 param.
        assert_eq!(func.blocks[func.entry].params.len(), 3);
        assert_eq!(func.blocks[then_block].params.len(), 1);
        assert_eq!(func.blocks[else_block].params.len(), 1);
    }

    #[test]
    fn build_module() {
        let sig = FunctionSig {
            params: vec![],
            return_ty: Type::Void, ..Default::default() };
        let mut fb = FunctionBuilder::new("main", sig, Visibility::Public);
        fb.ret(None);
        let func = fb.build();

        let mut mb = ModuleBuilder::new("test_module");
        let fid = mb.add_function(func);
        mb.add_global(Global {
            name: "counter".into(),
            ty: Type::Int(64),
            visibility: Visibility::Private,
            mutable: true,
        });
        let module = mb.build();

        assert_eq!(module.name, "test_module");
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.functions[fid].name, "main");
        assert_eq!(module.globals.len(), 1);
        assert_eq!(module.globals[0].name, "counter");
    }
}
