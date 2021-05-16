use crate::token::Token;

#[derive(PartialEq, PartialOrd, thiserror::Error, Debug, Clone)]
#[error("{}: Expected {} but got {}", .unexpected.range, .expected, .unexpected.rep)]
pub struct Error {
    /// An unexpected token when the parse fails.
    pub unexpected: Token,
    /// A symbol that was expected when the parse fails.
    pub expected: String,
}

impl Error {
    pub fn new(unexpected: Token, expected: impl Into<String>) -> Self {
        Self {
            unexpected,
            expected: expected.into(),
        }
    }
}
