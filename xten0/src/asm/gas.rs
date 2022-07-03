use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::process::Command;
use tempfile::tempdir;

fn exec(dir: impl AsRef<Path>, program: &str, args: &[&str]) {
    let status = Command::new(program)
        .current_dir(dir)
        .args(args)
        .status()
        .unwrap();
    assert!(status.success(), "{} failed with: {}", program, status);
}

pub fn gas(asm: &str) -> Vec<u8> {
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

macro_rules! assert_as {
    ($e:expr, $( $t:tt )*) => {
        let asm = format!($( $t )*);
        assert_eq!($e, $crate::asm::gas::gas(&asm), "{}", asm);
    };
}
