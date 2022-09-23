use std::process::Command;
use std::env;
use std::path::{Path};

fn main() {

    let out_dir = env::var("OUT_DIR").unwrap();

    let out_dir_path = Path::new(&out_dir);
    let path = out_dir_path.join("mawk-1.3.4-20200120");

    Command::new("cp").args(&["-R", "./mawk-1.3.4-20200120", &out_dir]).output().expect("unable to copy mawk to outdir");

    env::set_current_dir(path.to_str().expect("expect mawk to exist in cargo outdir")).expect("expected mawk to exist in out dir");

    Command::new("./configure")
        .output().expect("Failed to run `./configure while installing mawk");
    Command::new("make")
        .output().expect("Failed to run `make` while installing mawk");


    println!("cargo:rustc-link-search={}", path.to_str().expect("mawk outdir to exist"));
    println!("cargo:rustc-link-lib=static=regexp.o");
    println!("cargo:rerun-if-changed=build.rs");
}