use serde::{Deserialize, Serialize};

use super::ty::Type;

/// Metadata for a coroutine function.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoroutineInfo {
    /// Type yielded by each `Yield` operation.
    pub yield_ty: Type,
    /// Final return type of the coroutine.
    pub return_ty: Type,
}
