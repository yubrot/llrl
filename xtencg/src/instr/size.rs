use std::fmt;
use Size::*;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Size {
    B,  // 1-byte
    W,  // 2-byte
    D,  // 4-byte
    O,  // 8-byte
    O2, // 16-byte
}

impl Size {
    pub fn bits(self) -> u8 {
        match self {
            B => 8,
            W => 16,
            D => 32,
            O => 64,
            O2 => 128,
        }
    }
}

impl fmt::Display for Size {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            B => write!(f, "b"),
            W => write!(f, "w"),
            D => write!(f, "d"),
            O => write!(f, "o"),
            O2 => write!(f, "<128>"),
        }
    }
}
