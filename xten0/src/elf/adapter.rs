use super::format::*;
use crate::asm::{Binding, LocationSection, Object, RelocTarget};
use std::collections::HashMap;

/// Convert `asm::Object` into a relocatable ELF object.
pub fn into_relocatable_object(file: &str, o: Object) -> Elf {
    let mut elf = Elf::new(ObjectType::Rel);

    let text_shndx = elf.add_section(Section::text(".text".to_string(), o.text));
    let data_shndx = elf.add_section(Section::data(".data".to_string(), o.data));
    let rodata_shndx = elf.add_section(Section::rodata(".rodata".to_string(), o.rodata));
    let bss_shndx = elf.add_section(Section::bss(".bss".to_string(), o.bss));

    // LocationSection -> Shndx
    let location_section_shndx = move |s: LocationSection| match s {
        LocationSection::Text => text_shndx,
        LocationSection::Data => data_shndx,
        LocationSection::Rodata => rodata_shndx,
        LocationSection::Bss => bss_shndx,
    };

    let mut symtab = Symtab::new(".symtab".to_string(), Strtab::new(".strtab".to_string()));

    // Well-known symbols
    symtab.add(Symbol::file(file));
    let text_symtab_index = symtab.add(Symbol::section(text_shndx));
    let data_symtab_index = symtab.add(Symbol::section(data_shndx));
    let rodata_symtab_index = symtab.add(Symbol::section(rodata_shndx));
    let bss_symtab_index = symtab.add(Symbol::section(bss_shndx));

    // Symbols in the object, from locals to globals
    let mut symbol_symtab_index = HashMap::new();
    for sym in o.symbols.iter() {
        if let Binding::Local(loc) = sym.binding {
            let symbol = Symbol::new(&sym.name, location_section_shndx(loc.section), loc.pos);
            symbol_symtab_index.insert(sym.name.clone(), symtab.add(symbol));
        }
    }
    symtab.begin_global();
    for sym in o.symbols.into_iter() {
        if let Binding::Global(loc) = sym.binding {
            let symbol = match loc {
                Some(loc) => Symbol::new(&sym.name, location_section_shndx(loc.section), loc.pos),
                None => Symbol::undef(&sym.name),
            };
            symbol_symtab_index.insert(sym.name.clone(), symtab.add(symbol));
        }
    }

    let symtab = elf.add_section(symtab);

    let mut rela_text = Rela::new(".rela.text".to_string(), symtab, text_shndx);
    let mut rela_data = Rela::new(".rela.data".to_string(), symtab, data_shndx);
    let mut rela_rodata = Rela::new(".rela.rodata".to_string(), symtab, rodata_shndx);
    for r in o.relocs.into_iter() {
        let sym = match r.target {
            RelocTarget::Symbol(ref s) => symbol_symtab_index[s.as_str()],
            RelocTarget::Section(LocationSection::Text) => text_symtab_index,
            RelocTarget::Section(LocationSection::Data) => data_symtab_index,
            RelocTarget::Section(LocationSection::Rodata) => rodata_symtab_index,
            RelocTarget::Section(LocationSection::Bss) => bss_symtab_index,
        };
        let rela = match r.location.section {
            LocationSection::Text => &mut rela_text,
            LocationSection::Data => &mut rela_data,
            LocationSection::Rodata => &mut rela_rodata,
            LocationSection::Bss => panic!("Attempt to relocate to bss section"),
        };
        rela.add(RelaEntry::new(r.location.pos, sym, r.ty.into(), r.addend));
    }
    elf.add_section(rela_text);
    elf.add_section(rela_data);
    elf.add_section(rela_rodata);

    elf
}
