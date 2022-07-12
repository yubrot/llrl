use derive_new::new;
use std::collections::BTreeMap;
use std::io;

/// An abstract representation of an object containing machine code and data.
#[derive(Debug, Clone)]
pub struct Object {
    pub text: Vec<u8>,
    pub data: Vec<u8>,
    pub rodata: Vec<u8>,
    pub bss: u64,
    pub symbols: BTreeMap<String, Symbol>,
    pub relocs: Vec<Reloc>,
}

/// A symbol on an object.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, new)]
pub struct Symbol {
    pub name: String,
    pub binding: Binding,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum Binding {
    /// Local symbols. The definition corresponding to a symbol must always exist.
    Local(Location),
    /// Global symbols. The definition corresponding to a symbol may be external.
    Global(Option<Location>),
}

/// Location of the definition.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash, new)]
pub struct Location {
    pub section: LocationSection,
    pub pos: u64,
}

impl Location {
    /// Adjust `self.pos` with offset.
    pub fn offset(self, offset: i64) -> Self {
        assert!(0 <= self.pos as i64 + offset);
        Self::new(self.section, (self.pos as i64 + offset) as u64)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum LocationSection {
    Text,   // .text
    Data,   // .data
    Rodata, // .rodata
    Bss,    // .bss
}

/// A relocation. Set `target + addend` to `location` by `ty`.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, new)]
pub struct Reloc {
    pub location: Location,
    pub target: RelocTarget,
    pub addend: i64,
    pub ty: RelocType,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum RelocTarget {
    Symbol(String),
    Section(LocationSection),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum RelocType {
    /// Program counter relative offset as 8-bit signed.
    ///
    /// This corresponds to the relocation type `R_X86_64_PC8`.
    PcRel8,
    /// Program counter relative offset as 32-bit signed.
    ///
    /// This corresponds to the relocation type `R_X86_64_PC32`.
    PcRel32,
    /// Program counter relative offset to the address table as 32-bit signed.
    ///
    /// This corresponds to the relocation type `R_X86_64_GOTPCREL`.
    PcRelToAddressTable32,
    /// Absolute address as 64-bit unsigned.
    ///
    /// This corresponds to the relocation type `R_X86_64_64`.
    Abs64,
}

impl RelocType {
    /// If a relocation with this `RelocType` can be resolved statically, resolves it.
    ///
    /// Returns `true` if the relocation is resolved.
    pub fn resolve_statically(
        self,
        buf: &mut (impl io::Write + io::Seek),
        def_pos: u64,
        use_pos: u64,
        addend: i64,
    ) -> io::Result<bool> {
        match self {
            RelocType::PcRel8 => {
                // S + A - P
                let value = def_pos as i64 + addend - use_pos as i64;
                assert!(
                    i8::MIN as i64 <= value && value <= i8::MAX as i64,
                    "Cannot encode offset={} as PcRel8",
                    value
                );
                buf.seek(io::SeekFrom::Start(use_pos))?;
                buf.write_all(&(value as i8).to_le_bytes())?;
                Ok(true)
            }
            RelocType::PcRel32 => {
                // S + A - P
                let value = def_pos as i64 + addend - use_pos as i64;
                assert!(
                    i32::MIN as i64 <= value && value <= i32::MAX as i64,
                    "Cannot encode offset={} as PcRel32",
                    value
                );
                buf.seek(io::SeekFrom::Start(use_pos))?;
                buf.write_all(&(value as i32).to_le_bytes())?;
                Ok(true)
            }
            RelocType::PcRelToAddressTable32 => Ok(false),
            RelocType::Abs64 => Ok(false),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn reloc_type_resolve_static() {
        let mut buf = Cursor::new(vec![0u8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]);
        assert!(matches!(
            RelocType::PcRel8.resolve_statically(&mut buf, 4, 9, 0),
            Ok(true)
        ));
        assert_eq!(
            i8::from_le_bytes(buf.get_ref()[9..10].try_into().unwrap()),
            -5
        );
        assert!(matches!(
            RelocType::PcRel8.resolve_statically(&mut buf, 4, 10, 0),
            Ok(true)
        ));
        assert_eq!(
            i8::from_le_bytes(buf.get_ref()[10..11].try_into().unwrap()),
            -6
        );
        assert!(matches!(
            RelocType::PcRel32.resolve_statically(&mut buf, 10, 4, -1),
            Ok(true)
        ));
        assert_eq!(
            i32::from_le_bytes(buf.get_ref()[4..8].try_into().unwrap()),
            5
        );
        assert!(matches!(
            RelocType::Abs64.resolve_statically(&mut buf, 6, 2, 0),
            Ok(false)
        ));
        assert!(matches!(
            RelocType::PcRelToAddressTable32.resolve_statically(&mut buf, 6, 2, 0),
            Ok(false)
        ));
        assert!(matches!(
            buf.into_inner().as_slice(),
            &[0, 1, 2, 3, _, _, _, _, 8, _, _, 11],
        ));
    }
}
