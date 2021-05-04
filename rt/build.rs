use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    link_llrt();
    println!("cargo:rustc-link-lib=dylib=gc");
}

fn link_llrt() {
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    Command::new("make").status().unwrap();
    Command::new("cp")
        .arg("libllrt.a")
        .arg(&out_dir)
        .status()
        .unwrap();

    println!("cargo:rustc-link-lib=static=llrt");
    println!("cargo:rustc-link-search=native={}", out_dir.display());
}
