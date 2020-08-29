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
    let out_path_rs = Path::new(&root_dir).join("td_api.rs");
    let out_path_json = Path::new(&root_dir).join("td_api.json");

    let src = fs::read_to_string(&src_path)
        .unwrap_or_else(|_| panic!("Failed to read contents of {}", src_path.display()));
    let types =
        Codegen::new(&src).unwrap_or_else(|_| panic!("Failed to parse {}", src_path.display()));

    // Write to JSON file.
    fs::write(&out_path_json, types.to_json(true).as_bytes())
        .unwrap_or_else(|_| panic!("Failed to write at {}", out_path_json.display()));

    // Write to Rust file.
    fs::write(&out_path_rs, types.to_string().as_bytes())
        .unwrap_or_else(|_| panic!("Failed to write at {}", out_path_rs.display()));

    // Run rustfmt on the generated Rust file.
    if let Some(e) = run_rustfmt(&out_path_rs) {
        match e.kind() {
            std::io::ErrorKind::NotFound => eprintln!(
                "You don't have rustfmt in PATH, so the emitted code at {} is not formatted",
                out_path_rs.display()
            ),
            _ => eprintln!("Error running rustfmt on {}: {}", out_path_rs.display(), e),
        }
    }
}
