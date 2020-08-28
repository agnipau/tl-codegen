use {
    std::{env, fs, path::Path, process::Command},
    tl_codegen::codegen::Codegen,
};

fn run_rustfmt(path: &Path) -> Option<std::io::Error> {
    match Command::new("rustfmt").arg(path).output() {
        Ok(_) => None,
        Err(e) => Some(e),
    }
}

fn main() {
    let root_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let src_path = Path::new(&root_dir).join("td_api.tl");
    let out_path = Path::new(&root_dir).join("td_api.rs");

    let src = fs::read_to_string(&src_path)
        .unwrap_or_else(|_| panic!("Failed to read contents of {}", src_path.display()));
    let types =
        Codegen::new(&src).unwrap_or_else(|_| panic!("Failed to parse {}", src_path.display()));
    let final_str = types.to_string();
    fs::write(&out_path, final_str.as_bytes())
        .unwrap_or_else(|_| panic!("Failed to write at {}", out_path.display()));

    if let Some(e) = run_rustfmt(&out_path) {
        match e.kind() {
            std::io::ErrorKind::NotFound => eprintln!(
                "You don't have rustfmt in PATH, so the emitted code at {} is not formatted",
                out_path.display()
            ),
            _ => eprintln!("Error running rustfmt on {}: {}", out_path.display(), e),
        }
    }
}
