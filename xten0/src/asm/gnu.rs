use once_cell::sync::Lazy;
use regex::Regex;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::process::Command;
use tempfile::tempdir;

static DISASM_INSTRUCTION_PART: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"^\s*[0-9a-f]+:\s*(?:[0-9a-f][0-9a-f]\s)+\s*(.*)$").unwrap());

fn exec(dir: impl AsRef<Path>, program: &str, args: &[&str]) {
    let status = Command::new(program)
        .current_dir(dir)
        .args(args)
        .status()
        .unwrap();
    assert!(status.success(), "{} failed with: {}", program, status);
}

pub fn asm(asm: &str) -> Vec<u8> {
    let dir = tempdir().unwrap();

    // Write to asm.S
    let mut h = File::create(dir.path().join("asm.S")).unwrap();
    writeln!(&mut h, ".intel_syntax noprefix").unwrap();
    writeln!(&mut h, "{}", asm).unwrap();
    // Assemble asm.S to asm.o
    exec(dir.path(), "as", &["-o", "asm.o", "asm.S"]);
    // Extract .text section into mc.bin
    exec(
        dir.path(),
        "objcopy",
        &["--dump-section", ".text=mc.bin", "asm.o"],
    );
    // Read the machine-code from mc.bin
    let mut out = Vec::new();
    File::open(dir.path().join("mc.bin"))
        .unwrap()
        .read_to_end(&mut out)
        .unwrap();
    out
}

pub fn disasm(mc: Vec<u8>) -> String {
    let dir = tempdir().unwrap();

    // Write to mc.bin
    let mut h = File::create(dir.path().join("mc.bin")).unwrap();
    h.write_all(&mc).unwrap();
    h.flush().unwrap();
    // Disassemble mc.bin
    let output = Command::new("objdump")
        .current_dir(dir.path())
        .args(&["-D", "-b", "binary", "-m", "i386:x86-64", "mc.bin"])
        .output()
        .unwrap();
    let output = String::from_utf8(output.stdout).unwrap();
    // Omit encoding parts
    output
        .lines()
        .skip(7)
        .map(|line| match DISASM_INSTRUCTION_PART.captures(line) {
            Some(captures) => captures.get(1).unwrap().as_str().to_owned(),
            _ => panic!("Unhandled line: {}", line),
        })
        .collect::<Vec<_>>()
        .join("\n")
}

macro_rules! assert_as {
    ($src:expr, $( $t:tt )*) => {
        let dest = format!($( $t )*);
        assert_eq!(
            $crate::asm::gnu::disasm($src),
            $crate::asm::gnu::disasm($crate::asm::gnu::asm(&dest)),
        );
    };
}

#[test]
fn test_asm() {
    assert_eq!(asm("nop"), vec![0x90]);
}

#[test]
fn test_disasm() {
    assert_eq!(disasm(vec![0x90]), "nop".to_string());
}
