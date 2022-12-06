use super::mmap::Error as MmapError;
use crate::asm::RelocType;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Mmap(#[from] MmapError),
    #[error("Undefined symbol: {0}")]
    UndefinedSymbol(String),
    #[error("Duplicate symbol: {0}")]
    DuplicateSymbol(String),
    #[error("Offset {1} is too large or too small for relocation {0:?}")]
    OffsetOutOfRange(RelocType, isize),
    #[error("Relocation {0:?} against RelocTarget::Section is unsupported")]
    UnsupportedSectionRelocation(RelocType),
}
