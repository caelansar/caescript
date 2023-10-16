use chrono::{DateTime, Utc};
use std::process::Command;

fn main() {
    let output = Command::new("git")
        .args(["describe", "--tags", "--abbrev=0"])
        .output()
        .unwrap();
    let git_tag = String::from_utf8(output.stdout).unwrap();
    println!("cargo:rustc-env=GIT_VERSION={}", git_tag);

    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()
        .unwrap();
    let git_commit = String::from_utf8(output.stdout).unwrap();
    println!("cargo:rustc-env=GIT_COMMIT={}", git_commit);

    let now: DateTime<Utc> = Utc::now();
    let build_date = now.to_rfc3339();
    println!("cargo:rustc-env=GIT_DATE={}", build_date);
}
