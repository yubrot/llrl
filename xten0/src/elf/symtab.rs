use super::{Endian, SectionHeader, SectionHeaderIndex, SectionWriter, StrIndex};
use byteorder::WriteBytesExt;
use std::io::{self, Write};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Index(u32);

impl Index {
    pub const UNDEF: Self = Self(0);
}

impl From<Index> for u32 {
    fn from(i: Index) -> Self {
        i.0
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Entry {
    pub name: StrIndex,
    pub ty: Type,
    pub vis: Vis,
    pub shndx: SectionHeaderIndex,
    pub value: u64, // On relocation object, value is the offset from the head of the section
    pub size: u64,
}

impl Entry {
    pub const SIZE: u64 = 24;

    pub fn new(name: StrIndex, shndx: SectionHeaderIndex, value: u64) -> Self {
        Self {
            name,
            ty: Type::NoType,
            vis: Vis::Default,
            shndx,
            value,
            size: 0,
        }
    }

    pub fn undef(name: StrIndex) -> Self {
        Self::new(name, SectionHeaderIndex::UNDEF, 0)
    }

    pub fn file(name: StrIndex) -> Self {
        Self::new(name, SectionHeaderIndex::ABS, 0).ty(Type::File)
    }

    pub fn section(name: StrIndex, shndx: SectionHeaderIndex) -> Self {
        Self::new(name, shndx, 0).ty(Type::Section)
    }

    pub fn ty(self, ty: Type) -> Self {
        Self { ty, ..self }
    }

    pub fn size(self, size: u64) -> Self {
        Self { size, ..self }
    }

    pub fn write(&self, bind: Bind, w: &mut impl io::Write) -> io::Result<()> {
        w.write_u32::<Endian>(self.name.into())?;
        w.write_u8((bind.value() << 4) | self.ty.value())?;
        w.write_u8(self.vis.value())?;
        w.write_u16::<Endian>(self.shndx.into())?;
        w.write_u64::<Endian>(self.value)?;
        w.write_u64::<Endian>(self.size)?;
        Ok(())
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Bind {
    Local,
    Global,
}

impl Bind {
    pub fn value(self) -> u8 {
        match self {
            Self::Local => 0,
            Self::Global => 1,
        }
    }
}

#[allow(clippy::enum_variant_names)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Type {
    NoType,  // unspecified
    Object,  // data object
    Func,    // code object
    Section, // associated with a section
    File,    // file name
    Common,  // common data object
    Tls,     // thread-local data object
}

impl Type {
    pub fn value(self) -> u8 {
        match self {
            Self::NoType => 0,
            Self::Object => 1,
            Self::Func => 2,
            Self::Section => 3,
            Self::File => 4,
            Self::Common => 5,
            Self::Tls => 6,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Vis {
    Default,
    Internal,  // processor specific hidden class
    Hidden,    // sym unavailable in other modules
    Protected, // not preemptible, not exported
}

impl Vis {
    pub fn value(self) -> u8 {
        match self {
            Self::Default => 0,
            Self::Internal => 1,
            Self::Hidden => 2,
            Self::Protected => 3,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Writer<W> {
    w: SectionWriter<W>,
    next_index: Index,
    first_nonlocal_index: Option<Index>,
}

impl<W: io::Write + io::Seek> Writer<W> {
    pub fn new(w: W) -> io::Result<Self> {
        let mut w = SectionWriter::new(w)?;
        w.write_all(&[0; 24])?; // reserved entry (Index::UNDEF)
        Ok(Self {
            w,
            next_index: Index(1),
            first_nonlocal_index: None,
        })
    }

    /// Begin writing global symbols.
    pub fn begin_global(&mut self) {
        assert!(self.first_nonlocal_index.is_none());
        self.first_nonlocal_index = Some(self.next_index);
    }

    pub fn write(&mut self, e: Entry) -> io::Result<Index> {
        let index = self.next_index;
        self.next_index.0 += 1;
        let bind = match self.first_nonlocal_index {
            Some(_) => Bind::Global,
            None => Bind::Local,
        };
        e.write(bind, &mut self.w)?;
        Ok(index)
    }

    pub fn complete(
        &mut self,
        name: StrIndex,
        strtab: SectionHeaderIndex,
    ) -> io::Result<SectionHeader> {
        self.w.complete(SectionHeader::symtab(
            name,
            strtab,
            self.first_nonlocal_index.unwrap_or(self.next_index).0,
        ))
    }

    pub fn into_inner(self) -> W {
        self.w.into_inner()
    }
}
