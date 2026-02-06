use serde::{Deserialize, Serialize};

use crate::define_entity;

define_entity!(TypeVarId);

/// A resolved type in the IR.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Type {
    /// Void / unit.
    Void,
    /// Boolean.
    Bool,
    /// Signed integer with bit width.
    Int(u8),
    /// Unsigned integer with bit width.
    UInt(u8),
    /// Floating point with bit width (32 or 64).
    Float(u8),
    /// UTF-8 string.
    String,
    /// Array of a uniform element type.
    Array(Box<Type>),
    /// Associative map.
    Map(Box<Type>, Box<Type>),
    /// Optional / nullable.
    Option(Box<Type>),
    /// Tuple of types.
    Tuple(Vec<Type>),
    /// Named struct reference.
    Struct(String),
    /// Named enum reference.
    Enum(String),
    /// Function type.
    Function(Box<FunctionSig>),
    /// Coroutine that yields a type and returns a type.
    Coroutine {
        yield_ty: Box<Type>,
        return_ty: Box<Type>,
    },
    /// Unresolved type variable (pre-inference).
    Var(TypeVarId),
    /// Dynamic / any — fallback when inference fails.
    /// Backends emit a tagged union for this.
    Dynamic,
}

/// Function signature.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionSig {
    pub params: Vec<Type>,
    pub return_ty: Type,
}

/// Constraint generated during type inference.
#[derive(Debug, Clone)]
pub enum TypeConstraint {
    /// Two types must be equal.
    Equal(Type, Type),
    /// A type variable must be a subtype of a concrete type.
    Subtype { sub: Type, sup: Type },
    /// A type must have a specific field.
    HasField {
        ty: Type,
        field: String,
        field_ty: Type,
    },
    /// A type must be callable with given args and return type.
    Callable {
        ty: Type,
        args: Vec<Type>,
        ret: Type,
    },
}
