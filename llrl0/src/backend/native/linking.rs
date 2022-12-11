use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

pub fn link<'a, O>(
    dest: &Path,
    objects: impl IntoIterator<Item = O>,
    mut emit_object_to_file: impl FnMut(&Path, O),
    clang_options: impl IntoIterator<Item = &'a str>,
) -> Result<String, String> {
    let tmp_dir = tempfile::TempDir::new().unwrap();

    // TODO: Not every function in every module is needed for the main.
    // Rather, most of them exist for macros. This can be stripped.
    let objects = objects
        .into_iter()
        .enumerate()
        .map(|(i, obj)| {
            let name = format!("{}.o", i);
            let path = tmp_dir.path().join(&name);
            emit_object_to_file(&path, obj);
            path
        })
        .collect::<Vec<_>>();

    File::create(&tmp_dir.path().join("libllrt.a"))
        .unwrap()
        .write_all(llrt::ARCHIVE)
        .unwrap();

    let mut clang_command = Command::new("clang");
    clang_command
        .arg("-o")
        .arg(dest)
        .args(&objects)
        .args([
            "-lgc",
            "-lm",
            &format!("-L{}", tmp_dir.path().display()),
            "-lllrt",
        ])
        .args(clang_options);

    let output = clang_command.output().expect("Failed to execute clang");

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).into_owned())
    }
}
