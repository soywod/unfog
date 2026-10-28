//! # Secret prompts
//!
//! Delegates to pimalaya-cli's OS-aware pickers: [`configure_password`]
//! offers the OS keyrings, [`configure_token`] the OAuth 2.0 token brokers.
//!
//! A known provider or broker yields an argv command, which serializes as a
//! TOML array; a custom command is a shell string. unfog only reads a
//! secret, so a missing one surfaces when the account is tested right after.

use anyhow::{Result, bail};
use pimalaya_cli::wizard::keyring::{self, SecretChoice};
use pimalaya_config::{command::CommandConfig, secret::Secret};

/// Prompts for a password through the shared keyring picker.
///
/// `key_default` seeds the keyring entry, typically `<account>-caldav`, and
/// is used verbatim, so a pre-existing secret is read exactly as named.
pub fn configure_password(label: &str, key_default: &str) -> Result<Secret> {
    to_secret(keyring::prompt_secret(label, key_default)?)
}

/// Prompts for an API token through the shared token picker.
///
/// It combines the OS keyrings with the OAuth 2.0 brokers when `oauth` is
/// true; a broker refreshes and prints a fresh token on every read.
pub fn configure_token(label: &str, key_default: &str, oauth: bool) -> Result<Secret> {
    to_secret(keyring::prompt_token(label, key_default, oauth)?)
}

/// Turns a picker choice into the [`Secret`] the configuration stores.
fn to_secret(choice: SecretChoice) -> Result<Secret> {
    Ok(match choice {
        SecretChoice::Command(argv) => command_secret(argv)?,
        SecretChoice::Shell(line) => shell_secret(&line)?,
        SecretChoice::Raw(secret) => Secret::Raw(secret),
    })
}

/// Builds a [`Secret::Command`] from an argv, the form a known keyring
/// provider or token broker yields. It serializes back as a TOML array.
fn command_secret(argv: Vec<String>) -> Result<Secret> {
    let Some((program, args)) = argv.split_first() else {
        bail!("Empty command for secret");
    };

    Ok(Secret::Command(CommandConfig::Argv {
        program: program.clone(),
        args: args.to_vec(),
    }))
}

/// Builds a [`Secret::Command`] from a shell command line, the form a user
/// typed by hand. It serializes back as a TOML string.
fn shell_secret(line: &str) -> Result<Secret> {
    let line = line.trim();

    if line.is_empty() {
        bail!("Empty shell command for secret");
    }

    Ok(Secret::Command(CommandConfig::Shell(line.to_owned())))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_command_secret_is_rejected() {
        assert!(command_secret(Vec::new()).is_err());
    }

    #[test]
    fn blank_shell_secret_is_rejected() {
        assert!(shell_secret("   ").is_err());
    }
}
