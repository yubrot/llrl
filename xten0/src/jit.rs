mod mmap;
mod segment;
mod table;

pub use mmap::{Error as MmapError, Mmap, Protect};
pub use segment::Segment;
pub use table::Table;

use crate::asm::{Binding, Location, LocationSection, Object, Reloc, RelocTarget, RelocType};
use std::collections::{hash_map::Entry as HashMapEntry, HashMap};
use std::ptr;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Mmap(#[from] MmapError),
    #[error(transparent)]
    Link(#[from] LinkError),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, thiserror::Error)]
pub enum LinkError {
    #[error("Undefined symbol: {0}")]
    UndefinedSymbol(String),
    #[error("Duplicate symbol: {0}")]
    DuplicateSymbol(String),
    #[error("Offset {1} is too large or too small for relocation {0:?}")]
    OffsetOutOfRange(RelocType, isize),
    #[error("Relocation {0:?} against RelocTarget::Section is unsupported")]
    UnsupportedSectionRelocation(RelocType),
}

/// Alias of `FnMut(&str) -> Option<*const u8>`. This is used for resolving undefined symbols.
pub trait SymbolResolver: FnMut(&str) -> Option<*const u8> {}

impl<T: FnMut(&str) -> Option<*const u8>> SymbolResolver for T {}

pub mod symbol_resolver {
    pub fn none(_: &str) -> Option<*const u8> {
        None
    }
}

/// A JIT execution engine.
#[derive(Debug)]
pub struct Engine<R: SymbolResolver> {
    global: Global<R>,
    ro: Segment,
    rw: Segment,
    rx: Segment,
}

impl<R: SymbolResolver> Engine<R> {
    /// Initializes the JIT engine. `hint_addr` is used as a hint for mmap areas to be allocated.
    pub fn new(hint_addr: *const u8, symbol_resolver: R) -> Self {
        Self {
            global: Global::new(hint_addr, symbol_resolver),
            ro: Segment::new(hint_addr, Protect::ReadOnly),
            rw: Segment::new(hint_addr, Protect::ReadWrite),
            rx: Segment::new(hint_addr, Protect::ReadExec),
        }
    }

    /// Register the address corresponding to the symbol.
    pub fn register(&mut self, symbol: &str, address: *const u8) -> Result<(), Error> {
        self.global.bind(symbol, Some(address))?;
        Ok(())
    }

    /// Get the address corresponding to the symbol.
    pub fn get(&self, symbol: &str) -> Option<*const u8> {
        self.global.resolve(symbol)
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

        // bind
        let mut local = Local::new(text, data, rodata, bss, &mut self.global);
        for sym in obj.symbols.values() {
            local.bind(&sym.name, sym.binding)?;
        }

        // relocate
        for reloc in obj.relocs.iter() {
            local.relocate(reloc)?;
        }

        // restore memory protection
        drop(rx);
        drop(rw);
        drop(ro);

        Ok(())
    }
}

#[derive(Debug)]
struct Global<R> {
    symbols: HashMap<String, Symbol>,
    symbol_resolver: R,
    address_table: Table<*const u8>,
}

impl<R: SymbolResolver> Global<R> {
    fn new(hint_addr: *const u8, symbol_resolver: R) -> Self {
        Self {
            symbols: HashMap::new(),
            symbol_resolver,
            address_table: Table::new(hint_addr),
        }
    }

    fn bind(&mut self, symbol: &str, address: Option<*const u8>) -> Result<(), LinkError> {
        match (self.symbols.entry(symbol.to_owned()), address) {
            (HashMapEntry::Vacant(e), Some(address)) => {
                e.insert(Symbol::new(address));
                Ok(())
            }
            (HashMapEntry::Vacant(e), None) => match (self.symbol_resolver)(symbol) {
                Some(address) => {
                    e.insert(Symbol::new(address));
                    Ok(())
                }
                None => Err(LinkError::UndefinedSymbol(symbol.to_owned())),
            },
            (_, Some(_)) => Err(LinkError::DuplicateSymbol(symbol.to_owned())),
            (_, None) => Ok(()),
        }
    }

    fn resolve(&self, symbol: &str) -> Option<*const u8> {
        self.symbols.get(symbol).map(|sym| sym.address)
    }

    fn resolve_address_table_entry(
        &mut self,
        symbol: &str,
    ) -> Result<Option<*const *const u8>, Error> {
        self.symbols
            .get_mut(symbol)
            .map(|sym| sym.address_table_entry(&mut self.address_table))
            .transpose()
    }
}

#[derive(Debug)]
struct Local<'a, R> {
    text: *const u8,
    data: *const u8,
    bss: *const u8,
    rodata: *const u8,
    symbols: HashMap<&'a str, Symbol>,
    global: &'a mut Global<R>,
}

impl<'a, R: SymbolResolver> Local<'a, R> {
    fn new(
        text: *mut u8,
        data: *mut u8,
        rodata: *mut u8,
        bss: *mut u8,
        global: &'a mut Global<R>,
    ) -> Self {
        Self {
            text,
            data,
            rodata,
            bss,
            symbols: HashMap::new(),
            global,
        }
    }

    fn location_address(&self, loc: Location) -> *const u8 {
        match loc.section {
            LocationSection::Text => self.text.wrapping_add(loc.pos as usize),
            LocationSection::Data => self.data.wrapping_add(loc.pos as usize),
            LocationSection::Rodata => self.rodata.wrapping_add(loc.pos as usize),
            LocationSection::Bss => self.bss.wrapping_add(loc.pos as usize),
        }
    }

    fn bind(&mut self, symbol: &'a str, binding: Binding) -> Result<(), LinkError> {
        match binding {
            Binding::Local(loc) => {
                let address = self.location_address(loc);
                match self.symbols.entry(symbol) {
                    HashMapEntry::Vacant(e) => {
                        e.insert(Symbol::new(address));
                        Ok(())
                    }
                    _ => Err(LinkError::DuplicateSymbol(symbol.to_owned())),
                }
            }
            Binding::Global(loc) => {
                let address = loc.map(|loc| self.location_address(loc));
                self.global.bind(symbol, address)
            }
        }
    }

    fn resolve(&self, symbol: &'a str) -> Option<*const u8> {
        self.symbols
            .get(&symbol)
            .map(|sym| sym.address)
            .or_else(|| self.global.resolve(symbol))
    }

    fn resolve_address_table_entry(
        &mut self,
        symbol: &str,
    ) -> Result<Option<*const *const u8>, Error> {
        Ok(match self.symbols.get_mut(symbol) {
            Some(sym) => Some(sym.address_table_entry(&mut self.global.address_table)?),
            None => self.global.resolve_address_table_entry(symbol)?,
        })
    }

    fn relocate(&mut self, reloc: &'a Reloc) -> Result<(), Error> {
        let dest = self.location_address(reloc.location) as *mut u8;
        let addend = reloc.addend as isize;

        match reloc.ty {
            RelocType::PcRel8 => {
                // S + A - P
                let src = self.reloc_target_symbol(reloc)?;
                let value = unsafe { src.wrapping_offset(addend).offset_from(dest) };
                if value < (i8::MIN as isize) && (i8::MAX as isize) < value {
                    Err(LinkError::OffsetOutOfRange(reloc.ty, value))?;
                }
                unsafe { ptr::write(dest as *mut i8, value as i8) };
            }
            RelocType::PcRel32 => {
                // S + A - P
                let src = self.reloc_target_symbol(reloc)?;
                let value = unsafe { src.wrapping_offset(addend).offset_from(dest) };
                if value < (i32::MIN as isize) && (i32::MAX as isize) < value {
                    Err(LinkError::OffsetOutOfRange(reloc.ty, value))?;
                }
                unsafe { ptr::write(dest as *mut i32, value as i32) };
            }
            RelocType::PcRelToAddressTable32 => {
                // <AddressTableEntry> + A - P
                let src = self.reloc_target_address_table_entry(reloc)? as *const u8;
                let value = unsafe { src.wrapping_offset(addend).offset_from(dest) };
                if value < (i32::MIN as isize) && (i32::MAX as isize) < value {
                    Err(LinkError::OffsetOutOfRange(reloc.ty, value))?;
                }
                unsafe { ptr::write(dest as *mut i32, value as i32) };
            }
            RelocType::Abs64 => {
                // S + A
                let src = self.reloc_target_symbol(reloc)?;
                let value = src.wrapping_offset(addend) as usize as u64;
                unsafe { ptr::write(dest as *mut u64, value) };
            }
        }

        Ok(())
    }

    fn reloc_target_symbol(&self, reloc: &'a Reloc) -> Result<*const u8, Error> {
        Ok(match reloc.target {
            RelocTarget::Symbol(ref symbol) => self
                .resolve(symbol)
                .ok_or_else(|| LinkError::UndefinedSymbol(symbol.to_owned()))?,
            RelocTarget::Section(section) => self.location_address(Location::new(section, 0)),
        })
    }

    fn reloc_target_address_table_entry(
        &mut self,
        reloc: &'a Reloc,
    ) -> Result<*const *const u8, Error> {
        Ok(match reloc.target {
            RelocTarget::Symbol(ref symbol) => self
                .resolve_address_table_entry(symbol)?
                .ok_or_else(|| LinkError::UndefinedSymbol(symbol.to_owned()))?,
            RelocTarget::Section(_) => Err(LinkError::UnsupportedSectionRelocation(reloc.ty))?,
        })
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
struct Symbol {
    address: *const u8,
    address_table_entry: *const *const u8,
}

impl Symbol {
    fn new(address: *const u8) -> Self {
        Self {
            address,
            address_table_entry: ptr::null(),
        }
    }

    fn address_table_entry(
        &mut self,
        address_table: &mut Table<*const u8>,
    ) -> Result<*const *const u8, Error> {
        if self.address_table_entry.is_null() {
            self.address_table_entry = address_table.put(self.address)?;
        }
        Ok(self.address_table_entry)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asm::*;
    use once_cell::sync::Lazy;
    use std::io::{self, Write};
    use std::sync::Mutex;

    // extern void log_fib(int);
    //
    // int fib(int n) {
    //   int a = 0;
    //   int b = 1;
    //   for (int i = 1; i < n; ++i) {
    //     int c = a + b;
    //     a = b;
    //     b = c;
    //     log_fib(b);
    //   }
    //   return b;
    // }
    fn test_object_fib() -> io::Result<Object> {
        let mut w = Writer::new();

        let fib_log = w.get_label("log_fib");
        let fib = w.get_label("fib");
        let l1 = w.issue_label();
        let l2 = w.issue_label();

        w.define(fib, true);
        {
            w.pushq(Rbp)?;
            w.pushq(R15)?;
            w.pushq(R14)?;
            w.pushq(Rbx)?;
            w.pushq(Rax)?;
            w.movl(Ebp, 0x1)?;
            w.cmpl(Edi, 0x2i8)?;
            w.jl(Short(l2))?;
            w.movl(Ebx, Edi)?;
            w.subl(Ebx, 0x1i8)?;
            w.movl(R14D, 0x1)?;
            w.xorl(Eax, Eax)?;
            w.movq(R15, AddressTable(fib_log))?;
        }
        w.define(l1, false);
        {
            w.movl(Ebp, Eax)?;
            w.addl(Ebp, R14D)?;
            w.movl(Edi, Ebp)?;
            w.callq(R15)?;
            w.movl(Eax, R14D)?;
            w.movl(R14D, Ebp)?;
            w.subl(Ebx, 0x1i8)?;
            w.jne(Short(l1))?;
        }
        w.define(l2, false);
        {
            w.movl(Eax, Ebp)?;
            w.addq(Rsp, 0x8i8)?;
            w.popq(Rbx)?;
            w.popq(R14)?;
            w.popq(R15)?;
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

    // extern const int foo;
    // const int *bar = &foo;
    fn test_object_2() -> io::Result<Object> {
        let mut w = Writer::new();
        let foo = w.get_label("foo");
        let bar = w.get_label("bar");
        w.data().define(bar, true);
        w.data().write_all(&0u64.to_le_bytes())?;
        w.data().r#use(-8, foo, 0, RelocType::Abs64);
        w.produce()
    }

    // extern const int *bar;
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

    #[test]
    fn jit() {
        let some_heap_space = Box::new(0u8);
        let some_heap_ptr = &*some_heap_space as *const u8;
        let mut engine = Engine::new(some_heap_ptr, symbol_resolver::none);

        static FIB_LOG: Lazy<Mutex<Vec<i32>>> = Lazy::new(|| Mutex::new(Vec::new()));
        extern "C" fn log_fib(value: i32) {
            FIB_LOG.lock().unwrap().push(value);
        }
        assert_eq!(engine.register("log_fib", log_fib as *const u8), Ok(()));

        let obj = test_object_fib().unwrap();
        assert_eq!(engine.add_object(&obj), Ok(()));

        let fib = engine.get("fib");
        assert!(fib.is_some());

        let fib = unsafe { std::mem::transmute::<_, extern "C" fn(i32) -> i32>(fib.unwrap()) };
        assert_eq!(fib(10), 55);
        assert_eq!(
            FIB_LOG.lock().unwrap().as_slice(),
            &[1, 2, 3, 5, 8, 13, 21, 34, 55]
        );
    }

    #[test]
    fn jit_multiple_objects() {
        let some_heap_space = Box::new(0u8);
        let some_heap_ptr = &*some_heap_space as *const u8;
        let mut engine = Engine::new(some_heap_ptr, symbol_resolver::none);

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
}
