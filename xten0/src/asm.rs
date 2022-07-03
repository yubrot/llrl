//! x64 assembler.

#[cfg(test)]
#[macro_use]
mod gas;

mod encoding;
mod operand;

pub use operand::*;
