//! Incomplete ELF implementation.

mod adapter;
pub mod format;

pub use adapter::into_relocatable_object;
pub use format::Elf;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asm::*;
    use crate::binutils::readelf;
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
        let obj = into_relocatable_object("elf.o", obj);
        obj.write(&mut f)?;
        Ok(())
    }

    fn output_lines(input: &str) -> Vec<String> {
        input.trim().lines().map(|s| s.trim().to_owned()).collect()
    }

    #[test]
    fn elf_format() {
        let dir = tempdir().unwrap();
        assert!(create_test_elf_object(dir.path().join("elf.o")).is_ok());

        assert_eq!(
            output_lines(&readelf(dir.path(), &["-h", "elf.o"])),
            output_lines(
                r###"
ELF Header:
  Magic:   7f 45 4c 46 02 01 01 00 00 00 00 00 00 00 00 00
  Class:                             ELF64
  Data:                              2's complement, little endian
  Version:                           1 (current)
  OS/ABI:                            UNIX - System V
  ABI Version:                       0
  Type:                              REL (Relocatable file)
  Machine:                           Advanced Micro Devices X86-64
  Version:                           0x1
  Entry point address:               0x0
  Start of program headers:          0 (bytes into file)
  Start of section headers:          480 (bytes into file)
  Flags:                             0x0
  Size of this header:               64 (bytes)
  Size of program headers:           0 (bytes)
  Number of program headers:         0
  Size of section headers:           64 (bytes)
  Number of section headers:         11
  Section header string table index: 10
        "###
            )
        );
        assert_eq!(
            output_lines(&readelf(dir.path(), &["-S", "elf.o"])),
            output_lines(
                r###"
There are 11 section headers, starting at offset 0x1e0:

Section Headers:
  [Nr] Name              Type             Address           Offset
       Size              EntSize          Flags  Link  Info  Align
  [ 0]                   NULL             0000000000000000  00000040
       0000000000000000  0000000000000000           0     0     0
  [ 1] .text             PROGBITS         0000000000000000  00000040
       0000000000000016  0000000000000000  AX       0     0     16
  [ 2] .data             PROGBITS         0000000000000000  00000056
       0000000000000000  0000000000000000  WA       0     0     16
  [ 3] .rodata           PROGBITS         0000000000000000  00000056
       0000000000000006  0000000000000000   A       0     0     16
  [ 4] .bss              NOBITS           0000000000000000  0000005c
       0000000000000000  0000000000000000  WA       0     0     16
  [ 5] .strtab           STRTAB           0000000000000000  0000005c
       0000000000000022  0000000000000000           0     0     1
  [ 6] .symtab           SYMTAB           0000000000000000  0000007e
       00000000000000d8  0000000000000018           5     7     8
  [ 7] .rela.text        RELA             0000000000000000  00000156
       0000000000000030  0000000000000018   I       6     1     8
  [ 8] .rela.data        RELA             0000000000000000  00000186
       0000000000000000  0000000000000018   I       6     2     8
  [ 9] .rela.rodata      RELA             0000000000000000  00000186
       0000000000000000  0000000000000018   I       6     3     8
  [10] .shstrtab         STRTAB           0000000000000000  00000186
       0000000000000057  0000000000000000           0     0     1
Key to Flags:
  W (write), A (alloc), X (execute), M (merge), S (strings), I (info),
  L (link order), O (extra OS processing required), G (group), T (TLS),
  C (compressed), x (unknown), o (OS specific), E (exclude),
  D (mbind), l (large), p (processor specific)
"###
            )
        );
        assert_eq!(
            output_lines(&readelf(dir.path(), &["-s", "elf.o"])),
            output_lines(
                r###"
Symbol table '.symtab' contains 9 entries:
   Num:    Value          Size Type    Bind   Vis      Ndx Name
     0: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT  UND
     1: 0000000000000000     0 FILE    LOCAL  DEFAULT  ABS elf.o
     2: 0000000000000000     0 SECTION LOCAL  DEFAULT    1 .text
     3: 0000000000000000     0 SECTION LOCAL  DEFAULT    2 .data
     4: 0000000000000000     0 SECTION LOCAL  DEFAULT    3 .rodata
     5: 0000000000000000     0 SECTION LOCAL  DEFAULT    4 .bss
     6: 0000000000000000     0 NOTYPE  LOCAL  DEFAULT    3 hello_text
     7: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT  UND puts
     8: 0000000000000000     0 NOTYPE  GLOBAL DEFAULT    1 puts_hello
    "###
            )
        );
        assert_eq!(
            output_lines(&readelf(dir.path(), &["-r", "elf.o"])),
            output_lines(
                r###"
Relocation section '.rela.text' at offset 0x156 contains 2 entries:
  Offset          Info           Type           Sym. Value    Sym. Name + Addend
000000000007  000600000002 R_X86_64_PC32     0000000000000000 hello_text - 4
00000000000d  000700000009 R_X86_64_GOTPCREL 0000000000000000 puts - 4
"###
            )
        );
    }
}
