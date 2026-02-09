pub mod ast;
pub mod ast_passes;
pub mod block;
pub mod builder;
pub mod coroutine;
pub mod func;
pub mod inst;
pub mod linear;
pub mod module;
pub mod printer;
pub mod structurize;
pub mod ty;
pub mod value;

pub use ast::{AstFunction, BinOp, Expr, Stmt, UnaryOp};
pub use block::{Block, BlockId, BlockParam};
pub use builder::{FunctionBuilder, ModuleBuilder};
pub use coroutine::CoroutineInfo;
pub use func::{FuncId, Function, MethodKind, Visibility};
pub use inst::{CmpKind, Inst, InstId, Op, Span};
pub use linear::lower_function_linear;
pub use module::{ClassDef, EnumDef, EnumVariant, Global, Import, Module, StructDef};
pub use structurize::{
    build_cfg, compute_dominators_lt, dominates, structurize, BlockArgAssign, Cfg, Shape,
};
pub use ty::{FunctionSig, Type, TypeConstraint, TypeVarId};
pub use value::{Constant, ValueId};
