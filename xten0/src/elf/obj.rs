use super::{
    Header, RelaEntry, RelaWriter, SectionHeader, SectionHeaders, SectionWriter, StrIndex,
    StrtabWriter, SymtabEntry, SymtabWriter, Type,
};
use crate::asm::{Binding, LocationSection, Object, Reloc, RelocTarget};
use std::collections::HashMap;
use std::io::{self, Write};

pub fn write_relocatable_object<W>(mut w: W, file: &str, o: &Object) -> io::Result<W>
where
    W: io::Write + io::Seek,
{
    // prepare for ELF header
    w.seek(io::SeekFrom::Start(0))?;
    w.write_all(&[0; Header::SIZE as usize])?;

    let mut headers = SectionHeaders::new();

    // [1] write .shstrtab
    let mut shstrtab = StrtabWriter::new(&mut w)?;
    let shstrtab_str = shstrtab.write(".shstrtab")?;
    let strtab_str = shstrtab.write(".strtab")?;
    let text_str = shstrtab.write(".text")?;
    let data_str = shstrtab.write(".data")?;
    let rodata_str = shstrtab.write(".rodata")?;
    let bss_str = shstrtab.write(".bss")?;
    let symtab_str = shstrtab.write(".symtab")?;
    let rela_text_str = shstrtab.write(".rela.text")?;
    let rela_data_str = shstrtab.write(".rela.data")?;
    let rela_rodata_str = shstrtab.write(".rela.rodata")?;
    let shstrtab = headers.add(shstrtab.complete(shstrtab_str)?);

    // [2] write .strtab
    let mut strtab = StrtabWriter::new(&mut w)?;
    let file_str = strtab.write(file)?;
    let mut sym_strs = HashMap::new();
    for sym in o.symbols.values() {
        let sym_str = strtab.write(&sym.name)?;
        sym_strs.insert(sym.name.as_str(), sym_str);
    }
    let strtab = headers.add(strtab.complete(strtab_str)?);

    // [3] write .text
    let mut text = SectionWriter::new(&mut w)?;
    text.write_all(&o.text)?;
    let text = headers.add(text.complete(SectionHeader::text(text_str, 0))?);

    // [4] write .data
    let mut data = SectionWriter::new(&mut w)?;
    data.write_all(&o.data)?;
    let data = headers.add(data.complete(SectionHeader::data(data_str, 0))?);

    // [5] write .rodata
    let mut rodata = SectionWriter::new(&mut w)?;
    rodata.write_all(&o.rodata)?;
    let rodata = headers.add(rodata.complete(SectionHeader::rodata(rodata_str, 0))?);

    // [6] write .bss
    let bss = headers.add(SectionHeader::bss(bss_str, o.bss, 0).offset_at(&mut w)?);

    // LocationSection -> SectionHeaderIndex
    let shndx = move |s: LocationSection| match s {
        LocationSection::Text => text,
        LocationSection::Data => data,
        LocationSection::Rodata => rodata,
        LocationSection::Bss => bss,
    };

    // [7] write .symtab
    let mut symtab = SymtabWriter::new(&mut w)?;
    // Well-known symbols
    let _file_sym = symtab.write(SymtabEntry::file(file_str))?;
    let text_sym = symtab.write(SymtabEntry::section(StrIndex::NULL, text))?;
    let data_sym = symtab.write(SymtabEntry::section(StrIndex::NULL, data))?;
    let rodata_sym = symtab.write(SymtabEntry::section(StrIndex::NULL, rodata))?;
    let bss_sym = symtab.write(SymtabEntry::section(StrIndex::NULL, bss))?;
    // Symbols in the object, from locals to globals
    let mut sym_indices = HashMap::new();
    for sym in o.symbols.values() {
        if let Binding::Local(loc) = sym.binding {
            let sym_str = sym_strs[&sym.name.as_str()];
            sym_indices.insert(
                sym.name.as_str(),
                symtab.write(SymtabEntry::new(sym_str, shndx(loc.section), loc.pos))?,
            );
        }
    }
    symtab.begin_global();
    for sym in o.symbols.values() {
        if let Binding::Global(loc) = sym.binding {
            let sym_str = sym_strs[&sym.name.as_str()];
            sym_indices.insert(
                sym.name.as_str(),
                symtab.write(match loc {
                    Some(loc) => SymtabEntry::new(sym_str, shndx(loc.section), loc.pos),
                    None => SymtabEntry::undef(sym_str),
                })?,
            );
        }
    }
    let symtab = headers.add(symtab.complete(symtab_str, strtab)?);

    // LocationSection -> SymIndex
    let shsym = move |s: LocationSection| match s {
        LocationSection::Text => text_sym,
        LocationSection::Data => data_sym,
        LocationSection::Rodata => rodata_sym,
        LocationSection::Bss => bss_sym,
    };

    // Reloc -> RalaEntry
    let rela = move |r: &Reloc| {
        let index = match r.target {
            RelocTarget::Symbol(ref s) => sym_indices[&s.as_str()],
            RelocTarget::Section(section) => shsym(section),
        };
        RelaEntry::new(r.location.pos, index, r.ty.into()).addend(r.addend)
    };

    // [8] write .rela.text
    let mut rela_text = RelaWriter::new(&mut w)?;
    for r in o.relocs.iter() {
        if r.location.section == LocationSection::Text {
            rela_text.write(rela(r))?;
        }
    }
    let _rela_text = headers.add(rela_text.complete(rela_text_str, symtab, text)?);

    // [9] write .rela.data
    let mut rela_data = RelaWriter::new(&mut w)?;
    for r in o.relocs.iter() {
        if r.location.section == LocationSection::Data {
            rela_data.write(rela(r))?;
        }
    }
    let _rela_data = headers.add(rela_data.complete(rela_data_str, symtab, data)?);

    // [10] write .rela.rorodata
    let mut rela_rodata = RelaWriter::new(&mut w)?;
    for r in o.relocs.iter() {
        if r.location.section == LocationSection::Rodata {
            rela_rodata.write(rela(r))?;
        }
    }
    let _rela_rodata = headers.add(rela_rodata.complete(rela_rodata_str, symtab, rodata)?);

    // write section headers
    let headers = headers.write(&mut w)?;

    // write ELF header
    w.rewind()?;
    Header::new(Type::Rel)
        .section_headers(headers)
        .shstrndx(shstrtab)
        .write(&mut w)?;

    Ok(w)
}
