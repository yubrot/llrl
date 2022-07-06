use super::{Endian, SectionHeader, SectionHeaderIndex, SectionWriter, StrIndex, SymIndex};
use byteorder::WriteBytesExt;
use std::io;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Entry {
    offset: u64, // offset from the head of the section on relocatable object
    sym: SymIndex,
    ty: RelocationType,
    addend: i64,
}

impl Entry {
    pub const SIZE: u64 = 24;

    pub fn new(offset: u64, sym: SymIndex, ty: RelocationType) -> Self {
        Self {
            offset,
            sym,
            ty,
            addend: 0,
        }
    }

    pub fn addend(self, addend: i64) -> Self {
        Self { addend, ..self }
    }

    pub fn write(&self, w: &mut impl io::Write) -> io::Result<()> {
        let info = ((Into::<u32>::into(self.sym) as u64) << 32) + (self.ty.value() as u64);
        w.write_u64::<Endian>(self.offset)?;
        w.write_u64::<Endian>(info)?;
        w.write_i64::<Endian>(self.addend)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Writer<W> {
    w: SectionWriter<W>,
}

impl<W: io::Write + io::Seek> Writer<W> {
    pub fn new(w: W) -> io::Result<Self> {
        let w = SectionWriter::new(w)?;
        Ok(Self { w })
    }

    pub fn write(&mut self, e: Entry) -> io::Result<()> {
        e.write(&mut self.w)
    }

    pub fn complete(
        &mut self,
        name: StrIndex,
        symtab: SectionHeaderIndex,
        section_to_modify: SectionHeaderIndex,
    ) -> io::Result<SectionHeader> {
        self.w
            .complete(SectionHeader::rela(name, symtab, section_to_modify))
    }

    pub fn into_inner(self) -> W {
        self.w.into_inner()
    }
}

macro_rules! relocation_type {
    ($($id:ident: $value:tt,)*) => {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
        pub enum RelocationType {
            $($id,)*
        }

        impl RelocationType {
            pub fn value(self) -> u32 {
                match self {
                    $(Self::$id => $value,)*
                }
            }
        }
    };
}

// https://wiki.osdev.org/System_V_ABI
relocation_type! {
   None: 0,
   _64: 1, // S+A (absolute)
   _32: 10,
   _16: 12,
   _8: 14,
   Pc64: 24, // S+A-P (PC-relative)
   Pc32: 2,
   Pc16: 13,
   Pc8: 15,
   Got32: 3, // G+A (GOT entry index)
   GotPcRel: 9, // G+GOT+A-P (PC-relative to GOT entry: GotPc32 + Got32)
   GotPcRelX: 41,
   RexGotPcRelX: 42,
   GotOff64: 25, // S+A-GOT
   GotPc32: 26, // GOT+A-P (PC-relative to GOT)
   Plt32: 4,
   Copy: 5,
   GlobDat: 6,
   JumpSlot: 7,
   Relative: 8,
   Size32: 32, // Z+A
   Size64: 33,
}
