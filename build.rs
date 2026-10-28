//! # Build script
//!
//! Bakes the enabled features, the target triple and the git revision
//! into the binary, for `unfog --version` to report them.

use pimalaya_cli::build::{features_env, git_envs, target_envs};

fn main() {
    features_env(include_str!("./Cargo.toml"));
    target_envs();
    git_envs();
}
