use super::{Endian, RelaEntry, StrIndex, SymtabEntry};
use byteorder::WriteBytesExt;
use std::io;

/// Section header index.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct HeaderIndex(u16);

impl HeaderIndex {
    pub const UNDEF: Self = Self(0); // SHN_UNDEF
    pub const ABS: Self = Self(0xfff1); // SHN_ABS
    pub const COMMON: Self = Self(0xfff2); // SHN_COMMON
}

impl From<HeaderIndex> for u16 {
    fn from(h: HeaderIndex) -> Self {
        h.0
    }
}

/// Section header.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Header {
    pub name: StrIndex, // Section name
    pub ty: Type,
    pub flags: u64,     // attribute flags
    pub addr: u64,      // virtual addr at execution
    pub offset: u64,    // section file offset
    pub size: u64,      // section size in bytes
    pub link: u32,      // link to another section
    pub info: u32,      // additional section information
    pub addralign: u64, // section alignment
    pub entsize: u64,   // entry size if section holds table
}

impl Header {
    pub const SIZE: u16 = 0x40;

    #[allow(clippy::too_many_arguments)]
    fn new(
        name: StrIndex,
        ty: Type,
        flags: u64,
        addr: u64,
        link: u32,
        info: u32,
        addralign: u64,
        entsize: u64,
    ) -> Self {
        Self {
            name,
            ty,
            flags,
            addr,
            link,
            info,
            addralign,
            entsize,
            offset: 0,
            size: 0,
        }
    }

    pub fn undef() -> Self {
        Self::new(StrIndex::NULL, Type::Null, 0, 0, 0, 0, 0, 0)
    }

    pub fn progbits(name: StrIndex, flags: u64, addr: u64, addralign: u64) -> Self {
        // NOTE: Are link and info used?
        Self::new(name, Type::Progbits, flags, addr, 0, 0, addralign, 0)
    }

    pub fn symtab(name: StrIndex, strtab: HeaderIndex, first_nonlocal_index: u32) -> Self {
        let link = strtab.0 as u32;
        let info = first_nonlocal_index;
        Self::new(name, Type::Symtab, 0, 0, link, info, 8, SymtabEntry::SIZE)
    }

    pub fn strtab(name: StrIndex) -> Self {
        Self::new(name, Type::Strtab, 0, 0, 0, 0, 1, 0)
    }

    pub fn nobits(name: StrIndex, flags: u64, size: u64, addr: u64, addralign: u64) -> Self {
        // NOTE: Are link and info used?
        Self {
            // Since nobits sections do not have actual data, its size is specified directly.
            // Use `Section::offset_at` to set offset instead of using `Writer`.
            size,
            ..Self::new(name, Type::Nobits, flags, addr, 0, 0, addralign, 0)
        }
    }

    pub fn offset_at(self, w: &mut impl io::Seek) -> io::Result<Self> {
        let offset = w.stream_position()?;
        Ok(Self { offset, ..self })
    }

    pub fn rela(name: StrIndex, symtab: HeaderIndex, section_to_modify: HeaderIndex) -> Self {
        let flags = Header::FLAGS_INFO_LINK;
        let link = symtab.0 as u32;
        let info = section_to_modify.0 as u32;
        Self::new(name, Type::Rela, flags, 0, link, info, 8, RelaEntry::SIZE)
    }

    pub fn text(name: StrIndex, addr: u64) -> Self {
        let flags = Header::FLAGS_ALLOC | Header::FLAGS_EXECINSTR;
        Self::progbits(name, flags, addr, 16)
    }

    pub fn data(name: StrIndex, addr: u64) -> Self {
        let flags = Header::FLAGS_ALLOC | Header::FLAGS_WRITE;
        Self::progbits(name, flags, addr, 16)
    }

    pub fn rodata(name: StrIndex, addr: u64) -> Self {
        let flags = Header::FLAGS_ALLOC;
        Self::progbits(name, flags, addr, 16)
    }

    pub fn bss(name: StrIndex, size: u64, addr: u64) -> Self {
        let flags = Header::FLAGS_ALLOC | Header::FLAGS_WRITE;
        Self::nobits(name, flags, size, addr, 16)
    }

    /// Write the section header.
    pub fn write(&self, w: &mut impl io::Write) -> io::Result<()> {
        w.write_u32::<Endian>(self.name.into())?; // 0x0
        w.write_u32::<Endian>(self.ty.into())?; // 0x4
        w.write_u64::<Endian>(self.flags)?; // 0x8
        w.write_u64::<Endian>(self.addr)?; // 0x10
        w.write_u64::<Endian>(self.offset)?; // 0x18
        w.write_u64::<Endian>(self.size)?; // 0x20
        w.write_u32::<Endian>(self.link)?; // 0x28
        w.write_u32::<Endian>(self.info)?; // 0x2c
        w.write_u64::<Endian>(self.addralign)?; // 0x30
        w.write_u64::<Endian>(self.entsize)?; // 0x38
        Ok(())
    }

    pub const FLAGS_WRITE: u64 = 0x1; // SHF_WRITE
    pub const FLAGS_ALLOC: u64 = 0x2; // SHF_ALLOC
    pub const FLAGS_EXECINSTR: u64 = 0x4; // SHF_EXECINSTR
    pub const FLAGS_MERGE: u64 = 0x10; // SHF_MERGE
    pub const FLAGS_STRINGS: u64 = 0x20; // SHF_STRINGS
    pub const FLAGS_INFO_LINK: u64 = 0x40; // SHF_INTO_LINK
}

/// Arranging section headers.
#[derive(Debug, Clone)]
pub struct Headers {
    buf: Vec<Header>,
}

impl Headers {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            buf: vec![Header::undef()],
        }
    }

    pub fn add(&mut self, header: Header) -> HeaderIndex {
        let index = HeaderIndex(self.buf.len() as u16);
        self.buf.push(header);
        index
    }

    /// Write all added section headers.
    pub fn write(self, w: &mut (impl io::Write + io::Seek)) -> io::Result<(u64, u16)> {
        let shnum = self.buf.len() as u16;
        let mut shoff = w.stream_position()?;

        // 8 bytes aligned
        while (shoff % 8u64) != 0 {
            w.write_u8(0)?;
            shoff += 1;
        }

        for h in self.buf {
            h.write(w)?;
        }
        Ok((shoff, shnum))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Type {
    Null,
    Progbits, // Information defined by the program
    Symtab,   // Symbol table
    Strtab,   // String table
    Rela,     // Relocation entries with explicit addends
    Hash,
    Dynamic,
    Note,
    Nobits,
    Rel,
    Shlib,
    Dynsym,
}

impl From<Type> for u32 {
    fn from(t: Type) -> Self {
        match t {
            Type::Null => 0,
            Type::Progbits => 1,
            Type::Symtab => 2,
            Type::Strtab => 3,
            Type::Rela => 4,
            Type::Hash => 5,
            Type::Dynamic => 6,
            Type::Note => 7,
            Type::Nobits => 8,
            Type::Rel => 9,
            Type::Shlib => 10,
            Type::Dynsym => 11,
        }
    }
}

/// Writer for sections.
#[derive(Debug, Clone)]
pub struct Writer<W> {
    w: W,
    start: u64,
}

impl<W: io::Write + io::Seek> Writer<W> {
    pub fn new(mut w: W) -> io::Result<Self> {
        let start = w.stream_position()?;
        Ok(Self { w, start })
    }

    /// Get the offset in the section.
    pub fn offset(&mut self) -> io::Result<u64> {
        Ok(self.w.stream_position()? - self.start)
    }

    /// Adjusting alignment within the section.
    pub fn align(&mut self, alignment: u64) -> io::Result<()> {
        let p = self.offset()? % alignment;
        if p != 0 {
            for _ in 0..alignment - p {
                self.w.write_u8(0)?;
            }
        }
        Ok(())
    }

    /// Updates the offset and size of the section in the section header.
    pub fn complete(&mut self, header: Header) -> io::Result<Header> {
        let end = self.w.stream_position()?;
        Ok(Header {
            offset: self.start,
            size: end - self.start,
            ..header
        })
    }

    pub fn into_inner(self) -> W {
        self.w
    }
}

impl<W: io::Write> io::Write for Writer<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.w.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.w.flush()
    }
}

impl<W: io::Seek> io::Seek for Writer<W> {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        match pos {
            io::SeekFrom::Start(pos) => {
                // Relative from self.start
                let pos = self.w.seek(io::SeekFrom::Start(pos + self.start))?;
                Ok(pos - self.start)
            }
            io::SeekFrom::End(_) => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "section::Writer: SeekFrom::End is unsupported",
            )),
            io::SeekFrom::Current(ofs) => {
                let pos = self.w.seek(io::SeekFrom::Current(ofs))?;
                if self.start <= pos {
                    Ok(pos - self.start)
                } else {
                    self.w.seek(io::SeekFrom::Current(-ofs))?; // rollback
                    Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "section::Writer: invalid seek",
                    ))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{Cursor, Seek, SeekFrom, Write};

    #[test]
    fn writer() {
        let mut w = Cursor::new(Vec::new());
        w.write_all(&[0u8; 4]).unwrap();

        let mut sw = Writer::new(&mut w).unwrap();
        assert!(matches!(sw.offset(), Ok(0)));

        assert!(sw.write_all(&[1u8; 4]).is_ok());
        assert!(matches!(sw.offset(), Ok(4)));

        assert!(sw.align(8).is_ok());
        assert!(matches!(sw.offset(), Ok(8)));

        assert!(matches!(sw.seek(SeekFrom::Start(0)), Ok(0)));
        assert!(matches!(sw.seek(SeekFrom::Start(4)), Ok(4)));
        assert!(matches!(sw.seek(SeekFrom::Current(4)), Ok(8)));
        assert!(matches!(sw.seek(SeekFrom::Current(-10)), Err(_)));

        let h = sw.complete(Header::data(StrIndex::NULL, 0));
        assert!(h.is_ok());

        let h = h.unwrap();
        assert_eq!(h.offset, 4);
        assert_eq!(h.size, 8);
    }
}
