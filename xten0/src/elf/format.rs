use crate::asm::RelocType;
use byteorder::WriteBytesExt;
use std::collections::{hash_map::Entry as HashMapEntry, HashMap};
use std::ffi::CString;
use std::io;

pub use byteorder::LittleEndian as Endian;

/// String table index.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct StrtabIndex(u32);

impl StrtabIndex {
    pub const NULL: Self = Self(0);
}

/// String table section.
#[derive(Debug, Clone)]
pub struct Strtab {
    name: String,
    payload: Vec<u8>,
    map: HashMap<String, StrtabIndex>,
}

impl Strtab {
    pub fn new(name: String) -> Self {
        Self {
            name,
            payload: vec![0], // reserved entry (StrtabIndex::NULL)
            map: vec![(String::new(), StrtabIndex::NULL)]
                .into_iter()
                .collect(),
        }
    }

    pub fn get(&mut self, s: &str) -> StrtabIndex {
        match self.map.entry(s.to_owned()) {
            HashMapEntry::Occupied(e) => *e.get(),
            HashMapEntry::Vacant(e) => {
                let index = StrtabIndex(self.payload.len() as u32);
                let s = CString::new(s).unwrap();
                self.payload.extend_from_slice(s.as_bytes_with_nul());
                *e.insert(index)
            }
        }
    }
}

/// Symbol table index.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct SymtabIndex(u32);

impl SymtabIndex {
    pub const UNDEF: Self = Self(0);
}

/// Symbol table section.
#[derive(Debug, Clone)]
pub struct Symtab {
    name: String,
    strtab: Strtab,
    first_nonlocal_index: Option<SymtabIndex>,
    payload: Vec<u8>,
}

impl Symtab {
    pub fn new(name: String, strtab: Strtab) -> Self {
        Self {
            name,
            strtab,
            first_nonlocal_index: None,
            payload: vec![0; Symbol::SIZE], // reserved entry (SymtabIndex::UNDEF)
        }
    }

    fn next_index(&self) -> SymtabIndex {
        SymtabIndex((self.payload.len() / Symbol::SIZE) as u32)
    }

    pub fn add(&mut self, sym: Symbol<'_>) -> SymtabIndex {
        let index = self.next_index();
        let name = self.strtab.get(sym.name);
        let bind = self.first_nonlocal_index.map_or(0, |_| 1) << 4;
        self.payload.write_u32::<Endian>(name.0).unwrap();
        self.payload.write_u8(bind | u8::from(sym.ty)).unwrap();
        self.payload.write_u8(sym.vis.into()).unwrap();
        self.payload.write_u16::<Endian>(sym.shndx.0).unwrap();
        self.payload.write_u64::<Endian>(sym.value).unwrap();
        self.payload.write_u64::<Endian>(sym.size).unwrap();
        index
    }

    /// Begin writing global symbols.
    pub fn begin_global(&mut self) {
        assert!(self.first_nonlocal_index.is_none());
        self.first_nonlocal_index = Some(self.next_index());
    }
}

/// A Symbol in the symbol table.
#[derive(Debug, Clone, Copy)]
pub struct Symbol<'s> {
    pub name: &'s str,
    pub ty: SymbolType,
    pub vis: SymbolVisibility,
    pub shndx: Shndx,
    pub value: u64, // On relocation object, value is the offset from the head of the section
    pub size: u64,
}

impl<'s> Symbol<'s> {
    pub const SIZE: usize = 24;

    pub fn new(name: &'s str, shndx: Shndx, value: u64) -> Self {
        Self {
            name,
            ty: SymbolType::NoType,
            vis: SymbolVisibility::Default,
            shndx,
            value,
            size: 0,
        }
    }

    pub fn undef(name: &'s str) -> Self {
        Self::new(name, Shndx::UNDEF, 0)
    }

    pub fn file(name: &'s str) -> Self {
        Self::new(name, Shndx::ABS, 0).ty(SymbolType::File)
    }

    pub fn section(shndx: Shndx) -> Self {
        Self::new("", shndx, 0).ty(SymbolType::Section)
    }

    pub fn ty(self, ty: SymbolType) -> Self {
        Self { ty, ..self }
    }

    pub fn size(self, size: u64) -> Self {
        Self { size, ..self }
    }

    pub fn vis(self, vis: SymbolVisibility) -> Self {
        Self { vis, ..self }
    }
}

/// Symbol type.
#[allow(clippy::enum_variant_names)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum SymbolType {
    NoType,  // unspecified
    Object,  // data object
    Func,    // code object
    Section, // associated with a section
    File,    // file name
    Common,  // common data object
    Tls,     // thread-local data object
}

impl From<SymbolType> for u8 {
    fn from(ty: SymbolType) -> Self {
        match ty {
            SymbolType::NoType => 0,
            SymbolType::Object => 1,
            SymbolType::Func => 2,
            SymbolType::Section => 3,
            SymbolType::File => 4,
            SymbolType::Common => 5,
            SymbolType::Tls => 6,
        }
    }
}

/// Symbol visibility.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum SymbolVisibility {
    Default,
    Internal,  // processor specific hidden class
    Hidden,    // sym unavailable in other modules
    Protected, // not preemptible, not exported
}

impl From<SymbolVisibility> for u8 {
    fn from(vis: SymbolVisibility) -> Self {
        match vis {
            SymbolVisibility::Default => 0,
            SymbolVisibility::Internal => 1,
            SymbolVisibility::Hidden => 2,
            SymbolVisibility::Protected => 3,
        }
    }
}

/// Relocation section.
#[derive(Debug, Clone)]
pub struct Rela {
    name: String,
    symtab: Shndx,
    payload: Vec<u8>,
    target: Shndx,
}

impl Rela {
    pub fn new(name: String, symtab: Shndx, target: Shndx) -> Self {
        Self {
            name,
            symtab,
            payload: Vec::new(),
            target,
        }
    }

    pub fn add(&mut self, entry: RelaEntry) {
        let info = ((entry.sym.0 as u64) << 32) | (u32::from(entry.ty) as u64);
        self.payload.write_u64::<Endian>(entry.offset).unwrap();
        self.payload.write_u64::<Endian>(info).unwrap();
        self.payload.write_i64::<Endian>(entry.addend).unwrap();
    }
}

/// A relocation entry.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct RelaEntry {
    pub offset: u64, // offset from the head of the target section
    pub sym: SymtabIndex,
    pub ty: RelocationType,
    pub addend: i64,
}

impl RelaEntry {
    pub const SIZE: usize = 24;

    pub fn new(offset: u64, sym: SymtabIndex, ty: RelocationType, addend: i64) -> Self {
        Self {
            offset,
            sym,
            ty,
            addend,
        }
    }
}

macro_rules! relocation_type {
    ($($id:ident: $value:tt,)*) => {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
        pub enum RelocationType {
            $($id,)*
        }

        impl From<RelocationType> for u32 {
            fn from(r: RelocationType) -> Self {
                match r {
                    $(RelocationType::$id => $value,)*
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

impl From<RelocType> for RelocationType {
    fn from(r: RelocType) -> Self {
        match r {
            RelocType::PcRel8 => RelocationType::Pc8,
            RelocType::PcRel32 => RelocationType::Pc32,
            RelocType::PcRelToAddressTable32 => RelocationType::GotPcRel,
            RelocType::Abs64 => RelocationType::_64,
        }
    }
}

/// Section header table index.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct Shndx(u16);

impl Shndx {
    pub const UNDEF: Self = Self(0); // SHN_UNDEF
    pub const ABS: Self = Self(0xfff1); // SHN_ABS
    pub const COMMON: Self = Self(0xfff2); // SHN_COMMON
}

#[derive(Debug, Clone)]
pub struct Section {
    pub name: String,
    pub ty: SectionType,
    pub flags: u64,   // attribute flags
    pub addr: u64,    // virtual addr at execution
    pub link: u32,    // link to another section
    pub info: u32,    // additional section information
    pub align: u64,   // section alignment
    pub entsize: u64, // entry size if section holds table
    pub body: SectionBody,
}

impl Section {
    pub const HEADER_SIZE: usize = 0x40;

    pub fn new(name: String, ty: SectionType, align: u64, body: impl Into<SectionBody>) -> Self {
        Self {
            name,
            ty,
            flags: 0,
            addr: 0,
            link: 0,
            info: 0,
            align,
            entsize: 0,
            body: body.into(),
        }
    }

    pub fn flags(self, flags: u64) -> Self {
        Self { flags, ..self }
    }

    pub fn link(self, link: u32) -> Self {
        Self { link, ..self }
    }

    pub fn info(self, info: u32) -> Self {
        Self { info, ..self }
    }

    pub fn entsize(self, entsize: u64) -> Self {
        Self { entsize, ..self }
    }

    pub fn undef() -> Self {
        Self::new(String::new(), SectionType::Null, 0, 0u64)
    }

    pub fn text(name: String, body: Vec<u8>) -> Self {
        Self::new(name, SectionType::Progbits, 16, body)
            .flags(Self::SHF_ALLOC | Self::SHF_EXECINSTR)
    }

    pub fn data(name: String, body: Vec<u8>) -> Self {
        Self::new(name, SectionType::Progbits, 16, body).flags(Self::SHF_ALLOC | Self::SHF_WRITE)
    }

    pub fn rodata(name: String, body: Vec<u8>) -> Self {
        Self::new(name, SectionType::Progbits, 16, body).flags(Self::SHF_ALLOC)
    }

    pub fn bss(name: String, size: u64) -> Self {
        Self::new(name, SectionType::Nobits, 16, size).flags(Self::SHF_ALLOC | Self::SHF_WRITE)
    }

    const SHF_WRITE: u64 = 0x1; // SHF_WRITE
    const SHF_ALLOC: u64 = 0x2; // SHF_ALLOC
    const SHF_EXECINSTR: u64 = 0x4; // SHF_EXECINSTR
    const SHF_INFO_LINK: u64 = 0x40; // SHF_INTO_LINK

    // const SHF_MERGE: u64 = 0x10; // SHF_MERGE
    // const SHF_STRINGS: u64 = 0x20; // SHF_STRINGS
}

#[derive(Debug, Clone)]
pub enum SectionBody {
    Bytes(Vec<u8>),
    Size(u64),
}
impl SectionBody {
    fn size(&self, in_elf: bool) -> u64 {
        match self {
            SectionBody::Bytes(bytes) => bytes.len() as u64,
            SectionBody::Size(_) if in_elf => 0,
            SectionBody::Size(s) => *s,
        }
    }
}

impl From<Vec<u8>> for SectionBody {
    fn from(bytes: Vec<u8>) -> Self {
        Self::Bytes(bytes)
    }
}

impl From<u64> for SectionBody {
    fn from(size: u64) -> Self {
        Self::Size(size)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub enum SectionType {
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

impl From<SectionType> for u32 {
    fn from(t: SectionType) -> Self {
        match t {
            SectionType::Null => 0,
            SectionType::Progbits => 1,
            SectionType::Symtab => 2,
            SectionType::Strtab => 3,
            SectionType::Rela => 4,
            SectionType::Hash => 5,
            SectionType::Dynamic => 6,
            SectionType::Note => 7,
            SectionType::Nobits => 8,
            SectionType::Rel => 9,
            SectionType::Shlib => 10,
            SectionType::Dynsym => 11,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Elf {
    pub ty: ObjectType,
    pub entry: u64, // entry point virtual address
    pub flags: u32, // processor-specific flags
    sections: Vec<Section>,
}

impl Elf {
    pub const HEADER_SIZE: usize = 0x40;

    pub fn new(ty: ObjectType) -> Self {
        Self {
            ty,
            entry: 0,
            flags: 0,
            sections: vec![Section::undef()],
        }
    }

    fn add_shstrtab(&mut self) -> (Shndx, Vec<StrtabIndex>) {
        let mut section_names = Vec::new();
        let shndx = {
            let mut shstrtab = Strtab::new(".shstrtab".to_string());
            for s in self.sections.iter().map(|s| s.name.as_str()) {
                section_names.push(shstrtab.get(s));
            }
            section_names.push(shstrtab.get(".shstrtab"));
            self.add_section(shstrtab)
        };
        (shndx, section_names)
    }

    pub fn write(mut self, w: &mut impl io::Write) -> io::Result<()> {
        let (shstrtab_shndx, section_names) = self.add_shstrtab();
        let Self {
            ty,
            entry,
            flags,
            sections,
        } = self;

        // Section headers are placed after ELF header + Program header (0) + Sections, with alignment
        let mut shoff =
            Self::HEADER_SIZE as u64 + sections.iter().map(|s| s.body.size(true)).sum::<u64>();
        let shalign = (8 - shoff % 8) % 8;
        shoff += shalign;
        let shnum = sections.len() as u16;

        // ELF header
        w.write_all(&Self::MAG)?; // 0x0-0x3
        w.write_u8(Self::CLASS)?; // 0x4
        w.write_u8(Self::DATA)?; // 0x5
        w.write_u8(Self::VERSION)?; // 0x6
        w.write_u8(Self::OSABI)?; // 0x7
        w.write_u8(Self::ABIVERSION)?; // 0x8
        w.write_all(&Self::PAD)?; // 0x9-0xf
        w.write_u16::<Endian>(ty.into())?; // 0x10
        w.write_u16::<Endian>(Self::MACHINE)?; // 0x12
        w.write_u32::<Endian>(Self::VERSION as u32)?; // 0x14
        w.write_u64::<Endian>(entry)?; // 0x18
        w.write_u64::<Endian>(0)?; // 0x20: phoff: Program header offset (unsupported)
        w.write_u64::<Endian>(shoff)?; // 0x28
        w.write_u32::<Endian>(flags)?; // 0x30
        w.write_u16::<Endian>(Self::HEADER_SIZE as u16)?; // 0x34
        w.write_u16::<Endian>(0)?; // 0x36: phentsize: Program header entry size (unsupported)
        w.write_u16::<Endian>(0)?; // 0x38: phnum: Program header number (unsupported)
        w.write_u16::<Endian>(Section::HEADER_SIZE as u16)?; // 0x3a
        w.write_u16::<Endian>(shnum)?; // 0x3c
        w.write_u16::<Endian>(shstrtab_shndx.0)?; // 0x3e

        // Program header (unsupported)

        // Sections
        let mut offset = Self::HEADER_SIZE;
        let mut section_offsets = Vec::new();
        for s in sections.iter() {
            section_offsets.push(offset as u64);
            if let SectionBody::Bytes(ref bytes) = s.body {
                w.write_all(bytes)?;
                offset += bytes.len();
            }
        }
        for _ in 0..shalign {
            w.write_u8(0)?;
        }

        // Section header
        for ((s, offset), name) in sections.iter().zip(section_offsets).zip(section_names) {
            w.write_u32::<Endian>(name.0)?; // 0x0
            w.write_u32::<Endian>(s.ty.into())?; // 0x4
            w.write_u64::<Endian>(s.flags)?; // 0x8
            w.write_u64::<Endian>(s.addr)?; // 0x10
            w.write_u64::<Endian>(offset)?; // 0x18
            w.write_u64::<Endian>(s.body.size(false))?; // 0x20
            w.write_u32::<Endian>(s.link)?; // 0x28
            w.write_u32::<Endian>(s.info)?; // 0x2c
            w.write_u64::<Endian>(s.align)?; // 0x30
            w.write_u64::<Endian>(s.entsize)?; // 0x38
        }

        Ok(())
    }

    pub const MAG: [u8; 4] = [0x7f, b'E', b'L', b'F'];
    pub const CLASS: u8 = 2; // always CLASS64 on x64
    pub const DATA: u8 = 1; // always LSB on x64
    pub const VERSION: u8 = 1; // CURRENT
    pub const OSABI: u8 = 0; // No extensions or unspecified
    pub const ABIVERSION: u8 = 0; // Unused for OSABI=0
    pub const PAD: [u8; 7] = [0; 7];
    pub const MACHINE: u16 = 62; // AMD64
}

pub trait AddSection<S> {
    fn add_section(&mut self, section: S) -> Shndx;
}

impl AddSection<Section> for Elf {
    fn add_section(&mut self, section: Section) -> Shndx {
        let shndx = Shndx(self.sections.len() as u16);
        self.sections.push(section);
        shndx
    }
}

impl<T: AddSection<Section>> AddSection<Strtab> for T {
    fn add_section(&mut self, s: Strtab) -> Shndx {
        let section = Section::new(s.name, SectionType::Strtab, 1, s.payload);
        self.add_section(section)
    }
}

impl<T: AddSection<Section>> AddSection<Symtab> for T {
    fn add_section(&mut self, s: Symtab) -> Shndx {
        let next_index = s.next_index();
        let strtab = AddSection::<Strtab>::add_section(self, s.strtab);
        let section = Section::new(s.name, SectionType::Symtab, 8, s.payload)
            .link(strtab.0 as u32)
            .info(s.first_nonlocal_index.unwrap_or(next_index).0)
            .entsize(Symbol::SIZE as u64);
        self.add_section(section)
    }
}

impl<T: AddSection<Section>> AddSection<Rela> for T {
    fn add_section(&mut self, r: Rela) -> Shndx {
        let section = Section::new(r.name, SectionType::Rela, 8, r.payload)
            .flags(Section::SHF_INFO_LINK)
            .link(r.symtab.0 as u32)
            .info(r.target.0 as u32)
            .entsize(RelaEntry::SIZE as u64);
        self.add_section(section)
    }
}

/// Object file type.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum ObjectType {
    None,
    Rel,
    Exec,
    Dyn,
    Core,
}

impl From<ObjectType> for u16 {
    fn from(ty: ObjectType) -> Self {
        match ty {
            ObjectType::None => 0,
            ObjectType::Rel => 1,
            ObjectType::Exec => 2,
            ObjectType::Dyn => 3,
            ObjectType::Core => 4,
        }
    }
}
