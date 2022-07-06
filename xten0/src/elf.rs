//! Incomplete ELF implementation.

use byteorder::WriteBytesExt;
use std::io;

mod ident;
mod program;
mod rela;
mod section;
mod strtab;
mod symtab;

pub use byteorder::LittleEndian as Endian;
pub use program::Header as ProgramHeader;
pub use rela::{Entry as RelaEntry, RelocationType, Writer as RelaWriter};
pub use section::{
    Header as SectionHeader, HeaderIndex as SectionHeaderIndex, Headers as SectionHeaders,
    Type as SectionType, Writer as SectionWriter,
};
pub use strtab::{Index as StrIndex, Writer as StrtabWriter};
pub use symtab::{Entry as SymtabEntry, Index as SymIndex, Writer as SymtabWriter};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Header {
    pub ty: Type,
    pub entry: u64,                   // entry point virtual address
    pub phoff: u64,                   // Program header table file offset
    pub shoff: u64,                   // Section header table file offset
    pub flags: u32,                   // processor-specific flags
    pub phnum: u16,                   // Program header entry count
    pub shnum: u16,                   // Section header table entry count
    pub shstrndx: SectionHeaderIndex, // Section header string table index
}

impl Header {
    pub const SIZE: u16 = 0x40;

    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            entry: 0,
            phoff: 0,
            shoff: 0,
            flags: 0,
            phnum: 0,
            shnum: 0,
            shstrndx: SectionHeaderIndex::UNDEF,
        }
    }

    pub fn program_headers(self, (phoff, phnum): (u64, u16)) -> Self {
        Self {
            phoff,
            phnum,
            ..self
        }
    }

    pub fn section_headers(self, (shoff, shnum): (u64, u16)) -> Self {
        Self {
            shoff,
            shnum,
            ..self
        }
    }

    pub fn shstrndx(self, shstrndx: SectionHeaderIndex) -> Self {
        Self { shstrndx, ..self }
    }

    pub fn phsize(&self) -> usize {
        ProgramHeader::SIZE as usize * self.phnum as usize
    }

    pub fn shsize(&self) -> usize {
        SectionHeader::SIZE as usize * self.shnum as usize
    }

    pub fn write(&self, w: &mut impl io::Write) -> io::Result<()> {
        ident::write(w)?;
        w.write_u16::<Endian>(self.ty.into())?; // 0x10
        w.write_u16::<Endian>(MACHINE)?; // 0x12
        w.write_u32::<Endian>(VERSION)?; // 0x14
        w.write_u64::<Endian>(self.entry)?; // 0x18
        w.write_u64::<Endian>(self.phoff)?; // 0x20
        w.write_u64::<Endian>(self.shoff)?; // 0x28
        w.write_u32::<Endian>(self.flags)?; // 0x30
        w.write_u16::<Endian>(Self::SIZE)?; // 0x34
        w.write_u16::<Endian>(match self.phnum {
            0 => 0,
            _ => ProgramHeader::SIZE,
        })?; // 0x36
        w.write_u16::<Endian>(self.phnum)?; // 0x38
        w.write_u16::<Endian>(match self.shnum {
            0 => 0,
            _ => SectionHeader::SIZE,
        })?; // 0x3a
        w.write_u16::<Endian>(self.shnum)?; // 0x3c
        w.write_u16::<Endian>(self.shstrndx.into())?; // 0x3e
        Ok(())
    }
}

/// Object file type.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Type {
    None,
    Rel,
    Exec,
    Dyn,
    Core,
}

impl From<Type> for u16 {
    fn from(ty: Type) -> Self {
        match ty {
            Type::None => 0,
            Type::Rel => 1,
            Type::Exec => 2,
            Type::Dyn => 3,
            Type::Core => 4,
        }
    }
}

pub const MACHINE: u16 = 62; // AMD64

pub const VERSION: u32 = ident::VERSION as u32;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binutils::readelf;
    use once_cell::sync::Lazy;
    use regex::Regex;
    use std::collections::HashMap;
    use std::fs::File;
    use std::io::{self, Seek, Write};
    use std::path::Path;
    use tempfile::tempdir;

    fn create_test_elf(path: impl AsRef<Path>) -> io::Result<()> {
        let mut f = File::create(path)?;
        let mut headers = SectionHeaders::new();

        // prepare for ELF header
        f.write_all(&[0; Header::SIZE as usize])?;

        // [1] write .strtab
        let mut strtab = StrtabWriter::new(&mut f)?;
        let file_name = strtab.write("elf.o")?;
        let strtab_name = strtab.write(".strtab")?;
        let symtab_name = strtab.write(".symtab")?;
        let text_name = strtab.write(".text")?;
        let rodata_name = strtab.write(".rodata")?;
        let bss_name = strtab.write(".bss")?;
        let rela_text_name = strtab.write(".rela.text")?;
        let foo_name = strtab.write("foo")?;
        let bar_name = strtab.write("bar")?;
        let strtab = headers.add(strtab.complete(strtab_name)?);

        // [2] write .text
        let mut text = SectionWriter::new(&mut f)?;
        text.write_all(&[
            0x55, 0x48, 0x89, 0xe5, 0x48, 0x8b, 0x05, 0, 0, 0, 0, 0x5d, 0xc3,
        ])?;
        let text = headers.add(text.complete(SectionHeader::text(text_name, 0))?);

        // [3] write .rodata
        let mut rodata = SectionWriter::new(&mut f)?;
        let rodata = headers.add(rodata.complete(SectionHeader::rodata(rodata_name, 0))?);

        // [4] write .bss
        let _bss = headers.add(SectionHeader::bss(bss_name, 16, 0).offset_at(&mut f)?);

        // [5] write .symtab
        let mut symtab = SymtabWriter::new(&mut f)?;
        symtab.write(SymtabEntry::file(file_name))?;
        symtab.write(SymtabEntry::section(text_name, text))?;
        symtab.write(SymtabEntry::section(rodata_name, rodata))?;
        symtab.begin_global();
        let foo = symtab.write(SymtabEntry::undef(foo_name))?;
        let _bar = symtab.write(SymtabEntry::new(bar_name, text, 0))?;
        let symtab = headers.add(symtab.complete(symtab_name, strtab)?);

        // [6] write .rela.text
        let mut rela_text = RelaWriter::new(&mut f)?;
        rela_text.write(RelaEntry::new(7, foo, RelocationType::GotPcRel).addend(-4))?;
        let _rela_text = headers.add(rela_text.complete(rela_text_name, symtab, text)?);

        // write section headers
        let headers = headers.write(&mut f)?;

        // write ELF header
        f.rewind()?;
        Header::new(Type::Rel)
            .section_headers(headers)
            .shstrndx(strtab)
            .write(&mut f)?;

        Ok(())
    }

    static HEADER_FIELD: Lazy<Regex> = Lazy::new(|| Regex::new(r"^(.+):(.+)$").unwrap());
    static SECTION_HEADER: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"\[\s*(\d+)\]\s*([^\s]+)\s*([^\s]+)").unwrap());
    static SYMTAB_ENTRY: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"(\d+):\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+([^\s]+)").unwrap()
    });
    static RELOC_ENTRY: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"(\w+)\s+(\w+)\s+(\w+)\s+(\w+)\s+(\w+(?: [-+] \w+)?)").unwrap());

    #[test]
    fn elf_format() {
        let dir = tempdir().unwrap();
        assert!(create_test_elf(dir.path().join("test.o")).is_ok());

        // header
        let header = readelf(dir.path(), &["-h", "test.o"]);
        let header = header
            .lines()
            .filter_map(|line| {
                let c = HEADER_FIELD.captures(line)?;
                Some((c.get(1)?.as_str().trim(), c.get(2)?.as_str().trim()))
            })
            .collect::<HashMap<_, _>>();
        assert_eq!(header.get("Class"), Some(&"ELF64"));
        assert_eq!(header.get("Type"), Some(&"REL (Relocatable file)"));
        assert_eq!(header.get("Number of section headers"), Some(&"7"));
        assert_eq!(header.get("Section header string table index"), Some(&"1"));

        // sections
        let sections = readelf(dir.path(), &["-S", "test.o"]);
        let sections = sections
            .lines()
            .filter_map(|line| {
                let c = SECTION_HEADER.captures(line)?;
                Some((c.get(1)?.as_str(), (c.get(2)?.as_str(), c.get(3)?.as_str())))
            })
            .collect::<HashMap<_, _>>();
        assert_eq!(sections.get("1"), Some(&(".strtab", "STRTAB")));
        assert_eq!(sections.get("2"), Some(&(".text", "PROGBITS")));
        assert_eq!(sections.get("3"), Some(&(".rodata", "PROGBITS")));
        assert_eq!(sections.get("4"), Some(&(".bss", "NOBITS")));
        assert_eq!(sections.get("5"), Some(&(".symtab", "SYMTAB")));
        assert_eq!(sections.get("6"), Some(&(".rela.text", "RELA")));

        // symtab entries
        let syms = readelf(dir.path(), &["-s", "test.o"]);
        let syms = syms
            .lines()
            .filter_map(|line| {
                let c = SYMTAB_ENTRY.captures(line)?;
                let ty = c.get(4)?.as_str();
                let bind = c.get(5)?.as_str();
                let ndx = c.get(7)?.as_str();
                let name = c.get(8)?.as_str();
                Some((name, (ty, bind, ndx)))
            })
            .collect::<HashMap<_, _>>();
        assert_eq!(syms.get("elf.o"), Some(&("FILE", "LOCAL", "ABS")));
        assert_eq!(syms.get(".text"), Some(&("SECTION", "LOCAL", "2")));
        assert_eq!(syms.get("foo"), Some(&("NOTYPE", "GLOBAL", "UND")));
        assert_eq!(syms.get("bar"), Some(&("NOTYPE", "GLOBAL", "2")));

        // rela entries
        let relocs = readelf(dir.path(), &["-r", "test.o"]);
        let relocs = relocs
            .lines()
            .filter_map(|line| {
                let c = RELOC_ENTRY.captures(line)?;
                let offset = c.get(1)?.as_str().trim_start_matches('0');
                let ty = c.get(3)?.as_str();
                let value = c.get(5)?.as_str();
                Some((offset, (ty, value)))
            })
            .collect::<HashMap<_, _>>();
        assert_eq!(relocs.get("7"), Some(&("R_X86_64_GOTPCREL", "foo - 4")));
    }
}
