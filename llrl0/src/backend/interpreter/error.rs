use super::Value;
use crate::emitter::ir::RtId;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Panic: {0}")]
    Panic(String),

    #[error("Internal error: {0}")]
    Internal(String),

    #[error("Undefined behavior detected: {0}")]
    UndefinedBehavior(String),

    #[error("Unhandled continuation request: {0}")]
    ContRequest(RtId, Vec<Value>),

    #[error("Unhandled return request: {0}")]
    ReturnRequest(Value),
}
