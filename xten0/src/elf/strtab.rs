use super::{SectionHeader, SectionWriter};
use std::ffi::CString;
use std::io::{self, Write};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Index(u32);

impl Index {
    pub const NULL: Self = Self(0);
}

impl From<Index> for u32 {
    fn from(i: Index) -> Self {
        i.0
    }
}

#[derive(Debug, Clone)]
pub struct Writer<W> {
    w: SectionWriter<W>,
}

impl<W: io::Write + io::Seek> Writer<W> {
    pub fn new(w: W) -> io::Result<Self> {
        let mut w = SectionWriter::new(w)?;
        w.write_all(&[0])?; // NULL
        Ok(Self { w })
    }

    pub fn write(&mut self, s: &str) -> io::Result<Index> {
        let index = Index(self.w.offset()? as u32);
        let s = CString::new(s).unwrap();
        let bytes = s.to_bytes_with_nul();
        self.w.write_all(bytes)?;
        Ok(index)
    }

    pub fn complete(&mut self, name: Index) -> io::Result<SectionHeader> {
        self.w.complete(SectionHeader::strtab(name))
    }

    pub fn into_inner(self) -> W {
        self.w.into_inner()
    }
}
