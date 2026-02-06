pub mod block;
pub mod coroutine;
pub mod func;
pub mod inst;
pub mod module;
pub mod ty;
pub mod value;

pub use block::{Block, BlockId, BlockParam};
pub use coroutine::CoroutineInfo;
pub use func::{FuncId, Function, Visibility};
pub use inst::{CmpKind, Inst, InstId, Op, Span};
pub use module::{EnumDef, EnumVariant, Global, Import, Module, StructDef};
pub use ty::{FunctionSig, Type, TypeConstraint, TypeVarId};
pub use value::{Constant, ValueId};
