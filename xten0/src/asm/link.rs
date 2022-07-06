use super::inst::{WriteInst, WriteInstExt};
use super::operand::{memory, Rip};
use derive_new::new;
use std::collections::BTreeMap;
use std::io;

/// A label indicates the location of a machine code.
///
/// Labels may or may not be named. Named labels are added as entries in the output symtab.
/// Labels may be global or local. Global labels are always named. Local labels are valid only
/// within an object. Any use of local labels is resolved at output time, while any use of
/// global labels is not resolved at output time and is output as a rela entry.
///
/// ## Encoding
/// * Local label uses are always resolved as relative displacement. This corresponds to
///   `rel8` or `rel32` in the opcode and to the relocation types `R_X86_64_PC8` or `R_X86_64_PC32`.
/// * Global label uses are always resolved as absolute indirect (RIP-relative) address references.
///   This corresponds to `disp32` in the opcode and to relocation type `R_X86_64_GOTPCREL`.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, new)]
pub struct Label(usize);

/// Writer stores information for linking while writing machine code.
#[derive(Debug, Clone)]
pub struct Writer<W> {
    w: W,
    named_labels: BTreeMap<String, Label>,
    defs: Vec<Def>,
    uses: Vec<Use>,
}

impl<W> Writer<W> {
    pub fn new(w: W) -> Self {
        Self {
            w,
            named_labels: BTreeMap::new(),
            defs: Vec::new(),
            uses: Vec::new(),
        }
    }

    /// Issue an unnamed local label.
    pub fn issue_label(&mut self) -> Label {
        let label = Label(self.defs.len());
        self.defs.push(Def::new(label, None, false, None));
        label
    }

    /// Get a named label.
    pub fn get_label(&mut self, name: &str, is_global: bool) -> Label {
        match self.named_labels.get(name) {
            Some(&label) => {
                assert_eq!(
                    self.defs[label.0].is_global, is_global,
                    "Label binding mismatch"
                );
                label
            }
            None => {
                // issue a new label for name
                let label = Label(self.defs.len());
                self.named_labels.insert(name.to_owned(), label);
                self.defs
                    .push(Def::new(label, Some(name.to_owned()), is_global, None));
                label
            }
        }
    }

    fn set_label_start(&mut self, label: Label, location: u64) {
        let d = self.defs.get_mut(label.0).unwrap();
        assert!(
            d.location.is_none(),
            "Writer::set_label_start is called multiple times for the same label"
        );
        d.location = Some(location);
    }
}

// WriteInstExt methods can be used directly
impl<W: io::Write> io::Write for Writer<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.w.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.w.flush()
    }
}

impl<W: io::Write + io::Seek> Writer<W> {
    /// Get the current location. `io::Seek::stream_position` is used as the location.
    pub fn location(&mut self) -> u64 {
        self.w.stream_position().unwrap()
    }

    /// Set the label start to the current position.
    ///
    /// It is usually used in conjunction with `Writer::issue_label`.
    pub fn start_label(&mut self, label: Label) {
        let location = self.location();
        self.set_label_start(label, location);
    }

    /// Get a named label and set the label start to the current position.
    ///
    /// It is usually used in conjunction with `Writer::get_label`.
    pub fn define_label(&mut self, name: &str, global: bool) -> Label {
        let label = self.get_label(name, global);
        self.start_label(label);
        label
    }

    fn use_label(&mut self, label: Label, offset: i64, addend: i64, size: UseSize) {
        let location = (self.location() as i64 + offset) as u64;
        self.uses.push(Use::new(location, label, addend, size));
    }

    /// Output symtab entries and rela entries by resolving definitions and uses.
    pub fn output(mut self) -> io::Result<(W, Vec<SymtabEntry>, Vec<RelaEntry>)> {
        let mut symtab = Vec::new();
        let mut symtab_indices = BTreeMap::new(); // label -> symtab index
        let mut rela = Vec::new();

        // Collect named local labels (symtab entries are ordered from locals to globals)
        for d in self.defs.iter_mut().filter(|d| !d.is_global) {
            if let Some(name) = d.name.take() {
                // Named local labels are recorded as LOCAL symbols in symtab
                symtab_indices.insert(d.label, symtab.len());
                symtab.push(SymtabEntry::new(name, false, d.location));
            }
        }

        // Collect global labels
        for d in self.defs.iter_mut().filter(|d| d.is_global) {
            let name = d.name.take().expect("unnamed global labels");
            // Global labels are recorded as GLOBAL symbols in symtab
            symtab_indices.insert(d.label, symtab.len());
            symtab.push(SymtabEntry::new(name, true, d.location));
        }

        // Collect and link (if possible) uses
        for u in self.uses {
            let d = &self.defs[u.label.0];
            if d.is_global {
                // Global label uses are linked later by the JIT engine or by the loader
                let symtab_index = *symtab_indices.get(&u.label).unwrap();
                rela.push(RelaEntry::for_global(symtab_index, &u));
            } else {
                // Local label uses are linked here
                u.link_to(d, &mut self.w)?;
            }
        }

        Ok((self.w, symtab, rela))
    }
}

#[derive(PartialEq, Eq, Debug, Clone, new)]
struct Def {
    label: Label,
    name: Option<String>,
    is_global: bool,
    location: Option<u64>, // where the definition starts
}

#[derive(PartialEq, Eq, Debug, Clone, new)]
struct Use {
    location: u64, // where the use starts
    label: Label,  // a target definition
    addend: i64,   // adjustment
    size: UseSize,
}

impl Use {
    fn link_to(&self, d: &Def, w: &mut (impl io::Write + io::Seek)) -> io::Result<()> {
        assert!(!d.is_global, "Linking global labels are unsupported");
        if d.location.is_none() {
            if let Some(name) = d.name.as_ref() {
                panic!(
                    "Cannot resolve the location of the definition for label={}",
                    name
                );
            } else {
                panic!(
                    "Cannot resolve the location of the definition for label={}",
                    d.label.0
                );
            }
        }

        // S + A - P
        let value = d.location.unwrap() as i64 + self.addend - self.location as i64;
        w.seek(io::SeekFrom::Start(self.location))?;
        match self.size {
            UseSize::Near => {
                assert!(
                    i32::MIN as i64 <= value && value <= i32::MAX as i64,
                    "Cannot encode offset={} as rel32",
                    value
                );
                w.write_all(&(value as i32).to_le_bytes()) // rel8
            }
            UseSize::Short => {
                assert!(
                    i8::MIN as i64 <= value && value <= i8::MAX as i64,
                    "Cannot encode offset={} as rel8",
                    value
                );
                w.write_all(&(value as i8).to_le_bytes()) // rel32
            }
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
enum UseSize {
    Near,  // 4 bytes
    Short, // 1 byte
}

// callq Label
impl<W: io::Write + io::Seek> WriteInst<Writer<W>> for super::inst::Callq<Label> {
    fn write_inst(&self, w: &mut Writer<W>) -> io::Result<()> {
        match w.defs[self.0 .0].is_global {
            false => w.callq(0i32)?,              // rel32
            true => w.callq(memory(Rip + 0i32))?, // [RIP + disp32]
        }
        w.use_label(self.0, -4, -4, UseSize::Near);
        Ok(())
    }
}

// jmpq Label
impl<W: io::Write + io::Seek> WriteInst<Writer<W>> for super::inst::Jmpq<Label> {
    fn write_inst(&self, w: &mut Writer<W>) -> io::Result<()> {
        match w.defs[self.0 .0].is_global {
            false => w.jmpq(0i32)?,              // rel32
            true => w.jmpq(memory(Rip + 0i32))?, // [RIP + disp32]
        }
        w.use_label(self.0, -4, -4, UseSize::Near);
        Ok(())
    }
}

/// Wrapper type for short jmp, used to force rel8 encoding for jmp instructions.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct Short<T>(pub T);

// jmpq Short<Label>
impl<W: io::Write + io::Seek> WriteInst<Writer<W>> for super::inst::Jmpq<Short<Label>> {
    fn write_inst(&self, w: &mut Writer<W>) -> io::Result<()> {
        assert!(
            !w.defs[self.0 .0 .0].is_global,
            "Cannot use short jmp with global labels"
        );
        w.jmpq(0i8)?; // rel8
        w.use_label(self.0 .0, -1, -1, UseSize::Short);
        Ok(())
    }
}

macro_rules! impl_conditional_jmp {
    ($op:tt::$method:tt) => {
        // jCC Label
        impl<W: io::Write + io::Seek> WriteInst<Writer<W>> for super::inst::$op<Label> {
            fn write_inst(&self, w: &mut Writer<W>) -> io::Result<()> {
                assert!(
                    !w.defs[self.0 .0].is_global,
                    "Cannot use conditional jmp with global labels"
                );
                w.$method(0i32)?; // rel32
                w.use_label(self.0, -4, -4, UseSize::Near);
                Ok(())
            }
        }

        // jCC Short<Label>
        impl<W: io::Write + io::Seek> WriteInst<Writer<W>> for super::inst::$op<Short<Label>> {
            fn write_inst(&self, w: &mut Writer<W>) -> io::Result<()> {
                assert!(
                    !w.defs[self.0 .0 .0].is_global,
                    "Cannot use conditional short jmp with global labels"
                );
                w.$method(0i8)?; // rel8
                w.use_label(self.0 .0, -1, -1, UseSize::Short);
                Ok(())
            }
        }
    };
}

impl_conditional_jmp!(Ja::ja);
impl_conditional_jmp!(Jae::jae);
impl_conditional_jmp!(Jb::jb);
impl_conditional_jmp!(Jbe::jbe);
impl_conditional_jmp!(Jc::jc);
impl_conditional_jmp!(Je::je);
impl_conditional_jmp!(Jg::jg);
impl_conditional_jmp!(Jge::jge);
impl_conditional_jmp!(Jl::jl);
impl_conditional_jmp!(Jle::jle);
impl_conditional_jmp!(Jna::jna);
impl_conditional_jmp!(Jnae::jnae);
impl_conditional_jmp!(Jnb::jnb);
impl_conditional_jmp!(Jnbe::jnbe);
impl_conditional_jmp!(Jnc::jnc);
impl_conditional_jmp!(Jne::jne);
impl_conditional_jmp!(Jng::jng);
impl_conditional_jmp!(Jnge::jnge);
impl_conditional_jmp!(Jnl::jnl);
impl_conditional_jmp!(Jnle::jnle);
impl_conditional_jmp!(Jno::jno);
impl_conditional_jmp!(Jnp::jnp);
impl_conditional_jmp!(Jns::jns);
impl_conditional_jmp!(Jnz::jnz);
impl_conditional_jmp!(Jo::jo);
impl_conditional_jmp!(Jp::jp);
impl_conditional_jmp!(Jpe::jpe);
impl_conditional_jmp!(Jpo::jpo);
impl_conditional_jmp!(Js::js);
impl_conditional_jmp!(Jz::jz);

/// Symbol table entry.
#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct SymtabEntry {
    pub name: String,
    pub global: bool,
    pub location: Option<u64>,
}

/// Relocation entry.
///
/// Currently only global labels with UseSize::Near can be a RelaEntry. This implies that
/// the relocation type of the entry is always `R_X86_64_GOTPCREL` compatible.
#[derive(PartialEq, Eq, Debug, Clone, new)]
pub struct RelaEntry {
    pub location: u64,
    pub symtab_index: usize,
    pub addend: i64,
}

impl RelaEntry {
    fn for_global(symtab_index: usize, u: &Use) -> Self {
        assert!(
            matches!(u.size, UseSize::Near),
            "Relocations other than LabelUseSize::Near is unsupported"
        );
        Self::new(u.location, symtab_index, u.addend)
    }
}

#[cfg(test)]
mod tests {
    use crate::asm::*;
    use std::io;
    use std::io::Cursor;

    fn asm(
        f: impl FnOnce(&mut Writer<Cursor<Vec<u8>>>) -> io::Result<()>,
    ) -> (Vec<u8>, Vec<SymtabEntry>, Vec<RelaEntry>) {
        let mut buf = Writer::new(Cursor::new(Vec::new()));
        f(&mut buf).unwrap();
        let (w, symtab, rela) = buf.output().unwrap();
        (w.into_inner(), symtab, rela)
    }

    #[test]
    fn writer() {
        let mut fib2_location = 0;
        let mut fib3_location = 0;
        let mut call_fib_location = 0;
        let mut call_fib2_location = 0;

        let (w, symtab, rela) = asm(|w| {
            let fib = w.define_label("fib", true);
            let l1 = w.issue_label();
            let l2 = w.issue_label();
            {
                w.cmpl(Edi, 0x1i8)?;
                w.jle(Short(l2))?;
                w.movl(Edx, 0x1)?;
                w.movl(Eax, 0x1)?;
                w.movl(Ecx, 0x0)?;
            }
            w.start_label(l1);
            {
                w.movl(Esi, Eax)?;
                w.addl(Eax, Ecx)?;
                w.addl(Edx, 0x1i8)?;
                w.movl(Ecx, Esi)?;
                w.cmpl(Edi, Edx)?;
                w.jne(Short(l1))?;
                w.retq()?;
            }
            w.start_label(l2);
            {
                w.movl(Eax, 0x1)?;
                w.retq()?;
            }

            let fib2 = w.define_label("fib2", true);
            fib2_location = w.location();
            {
                w.addl(Edi, 0x1i8)?;
                w.callq(fib)?;
                call_fib_location = w.location();
                w.retq()?;
            }

            w.define_label("fib3", true);
            fib3_location = w.location();
            {
                w.addl(Edi, 0x1i8)?;
                w.callq(fib2)?;
                call_fib2_location = w.location();
                w.retq()?;
            }

            Ok(())
        });

        assert_as!(
            w,
            r#"
                fib:
                  cmp edi, 0x1
                  jle 2f
                  mov edx, 0x1
                  mov eax, 0x1
                  mov ecx, 0x0
                1:
                  mov esi, eax
                  add eax, ecx
                  add edx, 0x1
                  mov ecx, esi
                  cmp edi, edx
                  jne 1b
                  ret
                2:
                  mov eax, 0x1
                  ret
                fib2:
                  add edi, 0x1
                  call [rip + fib@GOTPCREL]
                  ret
                fib3:
                  add edi, 0x1
                  call [rip + fib2@GOTPCREL]
                  ret
            "#,
        );
        assert_eq!(
            symtab,
            vec![
                SymtabEntry::new("fib".to_string(), true, Some(0)),
                SymtabEntry::new("fib2".to_string(), true, Some(fib2_location)),
                SymtabEntry::new("fib3".to_string(), true, Some(fib3_location)),
            ]
        );
        assert_eq!(
            rela,
            vec![
                RelaEntry::new(call_fib_location - 4, 0, -4),
                RelaEntry::new(call_fib2_location - 4, 1, -4),
            ]
        );
    }
}
