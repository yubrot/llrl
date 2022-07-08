use once_cell::sync::Lazy;
use regex::Regex;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::process::Command;
use tempfile::tempdir;

static DISASM_INSTRUCTION_PART: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"^\s*[0-9a-f]+:\s*(?:[0-9a-f][0-9a-f]\s)+\s*(.*)$").unwrap());

fn exec(dir: impl AsRef<Path>, program: &str, args: &[&str]) -> String {
    let output = Command::new(program)
        .current_dir(dir)
        .args(args)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{} failed({}): {}",
        program,
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).to_string()
}

pub fn readelf(dir: impl AsRef<Path>, args: &[&str]) -> String {
    exec(dir, "readelf", args)
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

pub fn disasm(mc: &[u8]) -> String {
    let dir = tempdir().unwrap();

    // Write to mc.bin
    let mut h = File::create(dir.path().join("mc.bin")).unwrap();
    h.write_all(mc).unwrap();
    h.flush().unwrap();
    // Disassemble mc.bin
    let output = exec(
        dir.path(),
        "objdump",
        &["-D", "-b", "binary", "-m", "i386:x86-64", "mc.bin"],
    );
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
            $crate::binutils::disasm(&$src),
            $crate::binutils::disasm(&$crate::binutils::asm(&dest)),
        );
    };
}

#[test]
fn test_asm() {
    assert_eq!(asm("nop"), vec![0x90]);
}

#[test]
fn test_disasm() {
    assert_eq!(disasm(&[0x90]), "nop".to_string());
}
