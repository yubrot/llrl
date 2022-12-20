mod address_table;
mod error;
pub mod mmap;
pub mod segment;
mod symbol_table;
mod table;

pub use error::Error;
pub use symbol_table::{
    resolver as symbol_resolver, Resolver as SymbolResolver, ResolverFn as SymbolResolverFn,
};

use crate::asm::{Binding, Location, LocationSection, Object, Reloc, RelocTarget, RelocType};
use address_table::AddressTable;
use mmap::Protect;
use segment::Segment;
use std::collections::{hash_map::Entry as HashMapEntry, HashMap};
use std::ptr;
use symbol_table::SymbolTable;

/// A JIT execution engine.
#[derive(Debug)]
pub struct Engine<R: SymbolResolver = SymbolResolverFn> {
    symbol_table: SymbolTable<R>,
    address_table: AddressTable,
    ro: Segment,
    rw: Segment,
    rx: Segment,
}

impl<R: SymbolResolver> Engine<R> {
    /// Initializes the JIT engine.
    pub fn new(symbol_resolver: R) -> Self {
        Self {
            symbol_table: SymbolTable::new(symbol_resolver),
            address_table: AddressTable::new(true),
            ro: Segment::new(Protect::ReadOnly, true),
            rw: Segment::new(Protect::ReadWrite, true),
            rx: Segment::new(Protect::ReadExec, true),
        }
    }

    /// Register the pointer corresponding to the symbol.
    pub fn register(&mut self, symbol: &str, ptr: *const u8) -> Result<(), Error> {
        self.symbol_table.bind(symbol, Some(ptr))
    }

    /// Get the pointer corresponding to the symbol.
    pub fn get(&self, symbol: &str) -> Option<*const u8> {
        self.symbol_table.resolve(symbol)
    }

    /// Add an object. All symbols and relocations are immediately resolved.
    pub fn add_object(&mut self, obj: &Object) -> Result<(), Error> {
        fn align(size: usize) -> usize {
            ((size + 15) / 16) * 16
        }

        let text_size = align(obj.text.len());
        let data_size = align(obj.data.len());
        let bss_size = align(obj.bss as usize);
        let rodata_size = align(obj.rodata.len());

        // allocate spaces on segments
        let mut rx = self.rx.allocate(text_size)?;
        let rw = self.rw.allocate(data_size + bss_size)?;
        let mut ro = self.ro.allocate(rodata_size)?;

        // relax memory protection
        rx.set_protect_temporarily(Protect::ReadWrite)?;
        ro.set_protect_temporarily(Protect::ReadWrite)?;

        // load
        let text = rx.as_ptr();
        let data = rw.as_ptr();
        let bss = data.wrapping_add(data_size);
        let rodata = ro.as_ptr();
        if !obj.text.is_empty() {
            unsafe { ptr::copy(obj.text.as_ptr(), text, obj.text.len()) };
        }
        if !obj.data.is_empty() {
            unsafe { ptr::copy(obj.data.as_ptr(), data, obj.data.len()) };
        }
        if obj.bss != 0 {
            unsafe { ptr::write_bytes(bss, 0, obj.bss as usize) };
        }
        if !obj.rodata.is_empty() {
            unsafe { ptr::copy(obj.rodata.as_ptr(), rodata, obj.rodata.len()) };
        }

        // link
        let mut linker = Linker::new(
            &mut self.symbol_table,
            &mut self.address_table,
            Locator::new(text, data, rodata, bss),
        );

        for sym in obj.symbols.iter() {
            linker.bind(&sym.name, sym.binding)?;
        }

        for reloc in obj.relocs.iter() {
            linker.relocate(reloc)?;
        }

        // restore memory protection
        drop(rx);
        drop(rw);
        drop(ro);

        Ok(())
    }
}

#[derive(Debug)]
struct Locator {
    text: *const u8,
    data: *const u8,
    bss: *const u8,
    rodata: *const u8,
}

impl Locator {
    fn new(text: *mut u8, data: *mut u8, rodata: *mut u8, bss: *mut u8) -> Self {
        Self {
            text,
            data,
            rodata,
            bss,
        }
    }

    fn locate(&self, loc: Location) -> *const u8 {
        match loc.section {
            LocationSection::Text => self.text.wrapping_add(loc.pos as usize),
            LocationSection::Data => self.data.wrapping_add(loc.pos as usize),
            LocationSection::Rodata => self.rodata.wrapping_add(loc.pos as usize),
            LocationSection::Bss => self.bss.wrapping_add(loc.pos as usize),
        }
    }
}

#[derive(Debug)]
struct Linker<'a, R: SymbolResolver> {
    symbol_table: &'a mut SymbolTable<R>,
    address_table: &'a mut AddressTable,
    locator: Locator,
    local_symbols: HashMap<&'a str, *const u8>,
}

impl<'a, R: SymbolResolver> Linker<'a, R> {
    fn new(
        symbol_table: &'a mut SymbolTable<R>,
        address_table: &'a mut AddressTable,
        locator: Locator,
    ) -> Self {
        Self {
            symbol_table,
            address_table,
            locator,
            local_symbols: HashMap::new(),
        }
    }

    fn bind(&mut self, symbol: &'a str, binding: Binding) -> Result<(), Error> {
        match binding {
            Binding::Local(loc) => match self.local_symbols.entry(symbol) {
                HashMapEntry::Vacant(e) => {
                    e.insert(self.locator.locate(loc));
                    Ok(())
                }
                _ => Err(Error::DuplicateSymbol(symbol.to_owned())),
            },
            Binding::Global(loc) => self
                .symbol_table
                .bind(symbol, loc.map(|loc| self.locator.locate(loc))),
        }
    }

    fn relocate(&mut self, reloc: &'a Reloc) -> Result<(), Error> {
        let dest = self.locator.locate(reloc.location) as *mut u8;
        let addend = reloc.addend as isize;

        match reloc.ty {
            RelocType::PcRel8 => {
                // S + A - P
                let src = self.reloc_target_direct(reloc)?;
                let value = unsafe { src.wrapping_offset(addend).offset_from(dest) };
                if value < (i8::MIN as isize) && (i8::MAX as isize) < value {
                    Err(Error::OffsetOutOfRange(reloc.ty, value))?;
                }
                unsafe { ptr::write(dest as *mut i8, value as i8) };
            }
            RelocType::PcRel32 => {
                // S + A - P
                let src = self.reloc_target_direct(reloc)?;
                let value = unsafe { src.wrapping_offset(addend).offset_from(dest) };
                if value < (i32::MIN as isize) && (i32::MAX as isize) < value {
                    Err(Error::OffsetOutOfRange(reloc.ty, value))?;
                }
                unsafe { ptr::write(dest as *mut i32, value as i32) };
            }
            RelocType::PcRelToAddressTable32 => {
                // <AddressTableEntry> + A - P
                let src = self.reloc_target_address_table_entry(reloc)? as *const u8;
                let value = unsafe { src.wrapping_offset(addend).offset_from(dest) };
                if value < (i32::MIN as isize) && (i32::MAX as isize) < value {
                    Err(Error::OffsetOutOfRange(reloc.ty, value))?;
                }
                unsafe { ptr::write(dest as *mut i32, value as i32) };
            }
            RelocType::Abs64 => {
                // S + A
                let src = self.reloc_target_direct(reloc)?;
                let value = src.wrapping_offset(addend) as usize as u64;
                unsafe { ptr::write(dest as *mut u64, value) };
            }
        }

        Ok(())
    }

    fn reloc_target_direct(&self, reloc: &'a Reloc) -> Result<*const u8, Error> {
        match reloc.target {
            RelocTarget::Symbol(ref symbol) => self
                .local_symbols
                .get(symbol.as_str())
                .copied()
                .or_else(|| self.symbol_table.resolve(symbol))
                .ok_or_else(|| Error::UndefinedSymbol(symbol.to_owned())),
            RelocTarget::Section(section) => Ok(self.locator.locate(Location::new(section, 0))),
        }
    }

    fn reloc_target_address_table_entry(
        &mut self,
        reloc: &'a Reloc,
    ) -> Result<*const *const u8, Error> {
        match reloc.target {
            RelocTarget::Symbol(ref symbol) => self
                .address_table
                .prepare(symbol, || self.symbol_table.resolve(symbol))?
                .ok_or_else(|| Error::UndefinedSymbol(symbol.to_owned())),
            RelocTarget::Section(_) => Err(Error::UnsupportedSectionRelocation(reloc.ty)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asm::*;
    use std::io::{self, Write};

    // int add(int a, int b) {
    //   return a + b;
    // }
    fn test_object_add() -> io::Result<Object> {
        let mut w = Writer::new();

        let add = w.get_label("add");

        w.define(add, true);
        w.leal(Eax, memory(Rdi + Rsi))?;
        w.retq()?;

        w.produce()
    }

    // extern int add(int a, int b);
    //
    // int fib(int n) {
    //   int a = 0;
    //   int b = 1;
    //   for (int i = 1; i < n; ++i) {
    //     int c = add(a, b);
    //     a = b;
    //     b = c;
    //   }
    //   return b;
    // }
    fn test_object_fib() -> io::Result<Object> {
        let mut w = Writer::new();

        let add = w.get_label("add");
        let fib = w.get_label("fib");
        let l1 = w.issue_label();
        let l2 = w.issue_label();

        w.define(fib, true);
        {
            w.pushq(Rbp)?;
            w.pushq(Rbx)?;
            w.pushq(Rax)?;
            w.movl(Eax, 0x1)?;
            w.cmpl(Edi, 0x2i8)?;
            w.jl(Short(l2))?;
            w.movl(Ebx, Edi)?;
            w.subl(Ebx, 0x1i8)?;
            w.movl(Ebp, 0x1)?;
            w.xorl(Edi, Edi)?;
        }
        w.define(l1, false);
        {
            w.movl(Esi, Ebp)?;
            w.callq(AddressTable(add))?;
            w.movl(Edi, Ebp)?;
            w.movl(Ebp, Eax)?;
            w.subl(Ebx, 0x1i8)?;
            w.jne(Short(l1))?;
        }
        w.define(l2, false);
        {
            w.addq(Rsp, 0x8i8)?;
            w.popq(Rbx)?;
            w.popq(Rbp)?;
            w.retq()?;
        }

        w.produce()
    }

    // const int foo = 123;
    fn test_object_1() -> io::Result<Object> {
        let mut w = Writer::new();
        let foo = w.get_label("foo");
        w.rodata().define(foo, true);
        w.rodata().write_all(&123i32.to_le_bytes())?;
        w.produce()
    }

    // extern int foo;
    // int *bar = &foo;
    fn test_object_2() -> io::Result<Object> {
        let mut w = Writer::new();
        let foo = w.get_label("foo");
        let bar = w.get_label("bar");
        w.data().define(bar, true);
        w.data().write_all(&0u64.to_le_bytes())?;
        w.data().use_relative(-8, foo, 0, RelocType::Abs64);
        w.produce()
    }

    // extern int *bar;
    // int baz() { return *bar; }
    fn test_object_3() -> io::Result<Object> {
        let mut w = Writer::new();
        let bar = w.get_label("bar");
        let baz = w.get_label("baz");
        w.define(baz, true);
        w.movq(Rax, bar)?;
        w.movl(Eax, memory(Rax))?;
        w.retq()?;
        w.produce()
    }

    // #include <math.h>
    // double foo(double n) { return log10(n); }
    fn test_object_dl() -> io::Result<Object> {
        let mut w = Writer::new();
        let log10 = w.get_label("log10");
        let foo = w.get_label("foo");
        w.define(foo, true);
        w.jmpq(AddressTable(log10))?;
        w.produce()
    }

    #[test]
    fn jit() {
        let mut engine = Engine::new(symbol_resolver::none);

        let obj = test_object_add().unwrap();
        assert_eq!(engine.add_object(&obj), Ok(()));

        let add = engine.get("add").expect("add");

        {
            let add = unsafe { std::mem::transmute::<_, extern "C" fn(i32, i32) -> i32>(add) };
            assert_eq!(add(11, 7), 18);
        }

        let mut engine2 = Engine::new(symbol_resolver::none);
        assert_eq!(engine2.register("add", add), Ok(()));

        let obj = test_object_fib().unwrap();
        assert_eq!(engine2.add_object(&obj), Ok(()));

        let fib = engine2.get("fib").expect("fib");
        let fib = unsafe { std::mem::transmute::<_, extern "C" fn(i32) -> i32>(fib) };
        assert_eq!(fib(10), 55);
    }

    #[test]
    fn jit_multiple_objects() {
        let mut engine = Engine::new(symbol_resolver::none);

        let obj1 = test_object_1().unwrap();
        assert_eq!(engine.add_object(&obj1), Ok(()));

        let obj2 = test_object_2().unwrap();
        assert_eq!(engine.add_object(&obj2), Ok(()));

        let obj3 = test_object_3().unwrap();
        assert_eq!(engine.add_object(&obj3), Ok(()));

        let baz = engine.get("baz");
        assert!(baz.is_some());
        let baz = unsafe { std::mem::transmute::<_, extern "C" fn() -> i32>(baz.unwrap()) };
        assert_eq!(baz(), 123);

        static EXAMPLE_INT: i32 = 456;

        let bar = engine.get("bar");
        assert!(bar.is_some());
        let bar = bar.unwrap() as *mut *const i32;
        unsafe { std::ptr::write(bar, &EXAMPLE_INT) };
        assert_eq!(baz(), 456);
    }

    #[test]
    fn jit_dl() {
        let mut engine = Engine::new(symbol_resolver::dl::default);

        let obj = test_object_dl().unwrap();
        assert_eq!(engine.add_object(&obj), Ok(()));

        let foo = engine.get("foo");
        assert!(foo.is_some());

        let foo = unsafe { std::mem::transmute::<_, extern "C" fn(f64) -> f64>(foo.unwrap()) };
        assert_eq!(foo(1000.0), 3.0);
    }
}
