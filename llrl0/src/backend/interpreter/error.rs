use super::Value;
use crate::lowering::ir::RtId;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Panic: {0}")]
    Panic(String),

    #[error("Internal error: {0}")]
    InternalError(String),

    #[error("Unhandled continuation request: {0}")]
    ContRequest(RtId, Vec<Value>),

    #[error("Unhandled return request: {0}")]
    ReturnRequest(Value),
}

macro_rules! internal_error {
    ($( $t:tt )*) => {
        Err(Error::InternalError(format!($( $t )*)))
    };
}
