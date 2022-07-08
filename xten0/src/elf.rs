//! Incomplete ELF implementation.

use byteorder::WriteBytesExt;
use std::io;

mod ident;
mod obj;
mod program;
mod rela;
mod section;
mod strtab;
mod symtab;

pub use byteorder::LittleEndian as Endian;
pub use obj::write_relocatable_object;
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
    use crate::asm::*;
    use crate::binutils::readelf;
    use once_cell::sync::Lazy;
    use regex::Regex;
    use std::collections::HashMap;
    use std::fs::File;
    use std::io::{self, Write};
    use std::path::Path;
    use tempfile::tempdir;

    fn create_test_object() -> io::Result<Object> {
        let mut w = Writer::new();

        let hello_text = w.get_label("hello_text");
        let puts_hello = w.get_label("puts_hello");
        let puts = w.get_label("puts");

        w.rodata().define(hello_text, false);
        w.rodata().write_all(b"HELLO\0")?;

        w.define(puts_hello, true);
        w.subq(Rsp, 8i8)?;
        w.leaq(Rdi, hello_text)?;
        w.callq(AddressTable(puts))?;
        w.addq(Rsp, 8i8)?;
        w.retq()?;

        w.produce()
    }

    fn create_test_elf_object(path: impl AsRef<Path>) -> io::Result<()> {
        let mut f = File::create(path)?;
        let obj = create_test_object()?;
        write_relocatable_object(&mut f, "elf.o", &obj)?;
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
        assert!(create_test_elf_object(dir.path().join("elf.o")).is_ok());

        // header
        let header = readelf(dir.path(), &["-h", "elf.o"]);
        let header = header
            .lines()
            .filter_map(|line| {
                let c = HEADER_FIELD.captures(line)?;
                Some((c.get(1)?.as_str().trim(), c.get(2)?.as_str().trim()))
            })
            .collect::<HashMap<_, _>>();
        assert_eq!(header.get("Class"), Some(&"ELF64"));
        assert_eq!(header.get("Type"), Some(&"REL (Relocatable file)"));
        assert_eq!(header.get("Number of section headers"), Some(&"11"));
        assert_eq!(header.get("Section header string table index"), Some(&"1"));

        // sections
        let sections = readelf(dir.path(), &["-S", "elf.o"]);
        let sections = sections
            .lines()
            .filter_map(|line| {
                let c = SECTION_HEADER.captures(line)?;
                Some((c.get(1)?.as_str(), (c.get(2)?.as_str(), c.get(3)?.as_str())))
            })
            .collect::<HashMap<_, _>>();
        assert_eq!(sections.get("2"), Some(&(".strtab", "STRTAB")));
        assert_eq!(sections.get("3"), Some(&(".text", "PROGBITS")));
        assert_eq!(sections.get("5"), Some(&(".rodata", "PROGBITS")));
        assert_eq!(sections.get("6"), Some(&(".bss", "NOBITS")));
        assert_eq!(sections.get("7"), Some(&(".symtab", "SYMTAB")));
        assert_eq!(sections.get("8"), Some(&(".rela.text", "RELA")));

        // symtab entries
        let syms = readelf(dir.path(), &["-s", "elf.o"]);
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
        assert_eq!(syms.get(".text"), Some(&("SECTION", "LOCAL", "3")));
        assert_eq!(syms.get("puts"), Some(&("NOTYPE", "GLOBAL", "UND")));
        assert_eq!(syms.get("hello_text"), Some(&("NOTYPE", "LOCAL", "5")));
        assert_eq!(syms.get("puts_hello"), Some(&("NOTYPE", "GLOBAL", "3")));

        // rela entries
        let relocs = readelf(dir.path(), &["-r", "elf.o"]);
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
        assert_eq!(relocs.get("7"), Some(&("R_X86_64_PC32", "hello_text - 4")));
        assert_eq!(relocs.get("d"), Some(&("R_X86_64_GOTPCREL", "puts - 4")));
    }
}
