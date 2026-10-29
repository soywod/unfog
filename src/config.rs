//! # Configuration
//!
//! The TOML schema: a top-level block of rendering options, plus one
//! `[accounts.<name>]` block per account carrying a sub-block per
//! backend.
//!
//! The global block is folded under the selected account at load time,
//! so a value set once at the top applies everywhere it is not
//! overridden.
//!
//! `deny_unknown_fields` is set on the leaf blocks but deliberately not
//! on [`Config`] and [`AccountConfig`], so a front end reading the same
//! file can add its own sections without breaking this one.

use std::{collections::HashMap, fmt, path::PathBuf};

#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
use anyhow::Result;
use clap::ValueEnum;
use crossterm::style::Color;
use pimalaya_cli::table::ContentArrangement;
#[cfg(feature = "caldav")]
use pimalaya_config::secret::Secret;
use pimalaya_config::toml::opt_shell_expanded_path;
#[cfg(any(feature = "vdir", feature = "pimdir"))]
use pimalaya_config::toml::shell_expanded_path;
#[cfg(feature = "caldav")]
use pimalaya_config::toml::shell_expanded_string;
#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
use pimalaya_config::toml::to_string;
use pimalaya_config::{command::CommandConfig, toml::TomlConfig};
#[cfg(feature = "caldav")]
use pimalaya_stream::tls::{Rustls, RustlsCrypto, Tls, TlsProvider};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
#[cfg(feature = "caldav")]
use url::Url;

/// Skips a field equal to its type's default, so a wizard-generated
/// account omits a defaulted scalar rather than spelling it out.
fn is_default<T: Default + PartialEq>(value: &T) -> bool {
    *value == T::default()
}

/// The whole configuration file: global options and every account.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Config {
    /// Table rendering options shared by every list command.
    #[serde(default)]
    pub table: TableConfig,
    /// `tasklist` command options.
    #[serde(default)]
    pub tasklist: TasklistConfig,
    /// `task` command options.
    #[serde(default)]
    pub task: TaskConfig,
    /// `capture` options.
    #[serde(default, skip_serializing_if = "is_default")]
    pub capture: CaptureConfig,
    /// Time tracking options.
    #[serde(default, skip_serializing_if = "is_default")]
    pub time: TimeConfig,
    /// `account list` rendering options, read from this block only.
    #[serde(default)]
    pub account: AccountListingConfig,
    /// One `[accounts.<name>]` block per account.
    pub accounts: HashMap<String, AccountConfig>,
}

impl TomlConfig for Config {
    type Account = AccountConfig;

    fn project_name() -> &'static str {
        env!("CARGO_PKG_NAME")
    }

    fn take_named_account(&mut self, name: &str) -> Option<(String, Self::Account)> {
        self.accounts.remove_entry(name)
    }

    fn take_default_account(&mut self) -> Option<(String, Self::Account)> {
        let name = self
            .accounts
            .iter()
            .find_map(|(name, account)| account.default.then(|| name.clone()))?;

        self.take_named_account(&name)
    }
}

#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
/// Reading order of a generated account's groups.
///
/// What the account is, then the backend it reads tasks from, then the
/// rendering options. An unlisted key still renders, after these, so a
/// new [`AccountConfig`] field can never go missing from one.
const RENDER_ORDER: [&str; 8] = [
    "default", "vdir", "pimdir", "caldav", "tasklist", "task", "capture", "time",
];

#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
/// Keys naming what a backend group points at, lifted to its top.
///
/// Serialized alphabetically, `caldav.server` would otherwise read
/// under the `caldav.auth` credential authenticating against it.
const ENDPOINT_KEYS: [&str; 5] = ["discover", "server", "home", "home-dir", "root"];

#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
impl AccountConfig {
    /// Renders this account as an `[accounts.<name>]` block.
    ///
    /// The serializer decides what is written; this restores the
    /// reading order alphabetical dotted keys lose: groups reordered
    /// per [`RENDER_ORDER`], endpoint first per [`ENDPOINT_KEYS`].
    pub fn render(&self, name: &str) -> Result<String> {
        // NOTE: borrowed rather than built into a `Config`, which would
        // mean cloning the account. The emitter only wants an
        // `accounts` table, so any shape carrying one will do.
        #[derive(Serialize)]
        struct AccountDocument<'a> {
            accounts: HashMap<&'a str, &'a AccountConfig>,
        }

        let document = AccountDocument {
            accounts: HashMap::from([(name, self)]),
        };
        let rendered = to_string(&document)?;

        let (header, body) = match rendered.split_once('\n') {
            Some((header, body)) => (header, body),
            None => return Ok(rendered),
        };

        let mut groups: Vec<(String, Vec<&str>)> = Vec::new();

        for line in body.lines().filter(|line| !line.trim().is_empty()) {
            let key = line.split(['.', ' ']).next().unwrap_or(line).to_string();

            match groups.iter_mut().find(|(name, _)| *name == key) {
                Some((_, lines)) => lines.push(line),
                None => groups.push((key, vec![line])),
            }
        }

        groups.sort_by_key(|(key, _)| {
            RENDER_ORDER
                .iter()
                .position(|known| known == key)
                .unwrap_or(RENDER_ORDER.len())
        });

        let mut document = format!("{header}\n");

        for (index, (_, mut lines)) in groups.into_iter().enumerate() {
            if index > 0 {
                document.push('\n');
            }

            // NOTE: the endpoint is what the group is about, so it reads
            // first; the credentials and the quirks qualify it.
            lines.sort_by_key(|line| {
                let field = line.split(['.', ' ']).nth(1).unwrap_or_default();

                ENDPOINT_KEYS
                    .iter()
                    .position(|known| *known == field)
                    .unwrap_or(ENDPOINT_KEYS.len())
            });

            for line in lines {
                document.push_str(line);
                document.push('\n');
            }
        }

        Ok(document)
    }
}

/// The documented sample configuration, named wherever this crate
/// reports a missing or an incomplete one.
#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
pub const CONFIG_SAMPLE_URL: &str =
    "https://github.com/pimalaya/unfog/blob/master/config.sample.toml";

/// How to get a configuration, for the errors meeting none.
#[cfg(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
))]
pub const NO_CONFIG_HINT: &str = "run `unfog configure` to generate one, or write it by hand: \
     https://github.com/pimalaya/unfog/blob/master/config.sample.toml";

/// How to get a configuration, for the errors meeting none.
#[cfg(not(all(
    feature = "wizard",
    any(feature = "caldav", feature = "vdir", feature = "pimdir")
)))]
pub const NO_CONFIG_HINT: &str = "write one by hand: \
     https://github.com/pimalaya/unfog/blob/master/config.sample.toml";

/// One account: the backends it reads tasks from, plus whatever it
/// overrides of the global rendering options.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct AccountConfig {
    /// Whether a command given no `-a/--account` picks this account.
    #[serde(default, skip_serializing_if = "is_default")]
    pub default: bool,
    /// Table rendering options shared by every list command.
    #[serde(default)]
    pub table: TableConfig,
    /// `tasklist` command options.
    #[serde(default)]
    pub tasklist: TasklistConfig,
    /// `task` command options.
    #[serde(default)]
    pub task: TaskConfig,
    /// `capture` options.
    #[serde(default, skip_serializing_if = "is_default")]
    pub capture: CaptureConfig,
    /// Time tracking options.
    #[serde(default, skip_serializing_if = "is_default")]
    pub time: TimeConfig,
    /// vdir backend: one local directory per tasklist.
    #[cfg(feature = "vdir")]
    pub vdir: Option<VdirConfig>,
    /// pimdir backend: a local store a sync engine fills.
    #[cfg(feature = "pimdir")]
    pub pimdir: Option<PimdirConfig>,
    /// CalDAV backend: a remote calendar home-set.
    #[cfg(feature = "caldav")]
    pub caldav: Option<CaldavConfig>,
}

/// Tasklist-level options.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TasklistConfig {
    /// Tasklist id the `task` commands use when their `-l/--tasklist`
    /// flag is omitted.
    pub default: Option<String>,
    /// `tasklist list` options.
    #[serde(default)]
    pub list: TasklistListConfig,
}

/// `tasklist list` options.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TasklistListConfig {
    /// Per-column colors of the rendered table.
    #[serde(default)]
    pub table: TasklistListTableConfig,
}

/// Per-column colors of the `tasklist list` table.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TasklistListTableConfig {
    /// Color of the ID column, red by default.
    pub id_color: Option<Color>,
    /// Color of the NAME column, green by default.
    pub name_color: Option<Color>,
    /// Color of the DESCRIPTION column, uncolored by default.
    pub description_color: Option<Color>,
    /// Color of the COLOR column, uncolored by default.
    pub color_color: Option<Color>,
}

/// Task-level options.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TaskConfig {
    /// Command a task is edited through, spawned on the path of a
    /// temporary iCalendar file it edits in place.
    ///
    /// A shell line or an argv list, the path appended as its last
    /// argument: `composer = "tcal edit"` runs `tcal edit <PATH>`.
    pub composer: Option<CommandConfig>,
    /// `task list` options.
    #[serde(default)]
    pub list: TaskListConfig,
}

/// `task list` options.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TaskListConfig {
    /// Per-column colors of the rendered table.
    #[serde(default)]
    pub table: TaskListTableConfig,
}

/// Per-column colors of the `task list` table.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TaskListTableConfig {
    /// Color of the ID column, red by default.
    pub id_color: Option<Color>,
    /// Color of the SUMMARY column, green by default.
    pub summary_color: Option<Color>,
    /// Color of the DUE column, dark yellow by default.
    pub due_color: Option<Color>,
    /// Color of the STATUS column, uncolored by default.
    pub status_color: Option<Color>,
    /// Color of the PRIORITY column, uncolored by default.
    pub priority_color: Option<Color>,
    /// Color of the CATEGORIES column, blue by default.
    pub categories_color: Option<Color>,
}

/// `capture` options.
#[derive(Clone, Debug, Default, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct CaptureConfig {
    /// Tasklist `capture` writes to, before `tasklist.default` and the
    /// tasklist the backend marks as the default one.
    pub tasklist: Option<String>,
}

/// Time tracking options.
#[derive(Clone, Debug, Default, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TimeConfig {
    /// Where `task start` writes a time entry, `local` by default.
    pub strategy: Option<TimeStrategy>,
    /// The calendar the `event` and `journal` strategies write finished
    /// entries to, and `worktime` reads: a collection of the account
    /// `time.account` names, else of this one. Best a calendar of its own,
    /// holding nothing but time entries.
    pub collection: Option<String>,
    /// The account holding `time.collection`, when not this one: tasks on
    /// one server, time entries in another's calendar.
    pub account: Option<String>,
    /// The directory the `local` strategy writes to outside the pimdir
    /// backend, `$XDG_DATA_HOME/unfog/time` by default. Shell-expanded
    /// at load.
    #[serde(default, deserialize_with = "opt_shell_expanded_path")]
    pub directory: Option<PathBuf>,
}

/// Where a time entry goes (pimdir GUIDE.md §18).
#[derive(
    Clone, Copy, Debug, Default, Eq, PartialEq, Deserialize, Serialize, JsonSchema, ValueEnum,
)]
#[serde(rename_all = "kebab-case")]
pub enum TimeStrategy {
    /// A VEVENT, `DTEND` set on stop, in `time.collection`.
    Event,
    /// A pair of VJOURNALs, `CATEGORIES:start` then `stop`, in
    /// `time.collection`.
    Journal,
    /// The event shape kept on the device: the local time collection of
    /// a pimdir store, else a vdir directory. Never synced.
    #[default]
    Local,
}

impl fmt::Display for TimeStrategy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Event => write!(f, "event"),
            Self::Journal => write!(f, "journal"),
            Self::Local => write!(f, "local"),
        }
    }
}

/// `account list` rendering options. Top-level only.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct AccountListingConfig {
    /// `account list` options.
    #[serde(default)]
    pub list: AccountListingListConfig,
}

/// `account list` options.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct AccountListingListConfig {
    /// Per-column colors of the rendered table.
    #[serde(default)]
    pub table: AccountListingTableConfig,
}

/// Per-column colors of the `account list` table.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct AccountListingTableConfig {
    /// Color of the NAME column, green by default.
    pub name_color: Option<Color>,
    /// Color of the BACKENDS column, blue by default.
    pub backends_color: Option<Color>,
    /// Color of the DEFAULT column, uncolored by default.
    pub default_color: Option<Color>,
}

/// Global / per-account table rendering quirks shared across every list
/// command.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TableConfig {
    /// `comfy_table` preset string. Defaults to `UTF8_FULL_CONDENSED`.
    pub preset: Option<String>,
    /// Column-arrangement strategy. Defaults to `Dynamic`.
    pub arrangement: Option<TableArrangementConfig>,
}

/// How a table spreads its columns over the terminal width.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub enum TableArrangementConfig {
    /// Fit the content to the terminal width. The default.
    #[default]
    Dynamic,
    /// Fit the content to the terminal width, always filling it.
    DynamicFullWidth,
    /// Leave every column at its natural width.
    Disabled,
}

impl From<TableArrangementConfig> for ContentArrangement {
    fn from(arrangement: TableArrangementConfig) -> Self {
        match arrangement {
            TableArrangementConfig::Dynamic => ContentArrangement::Dynamic,
            TableArrangementConfig::DynamicFullWidth => ContentArrangement::DynamicFullWidth,
            TableArrangementConfig::Disabled => ContentArrangement::Disabled,
        }
    }
}

/// vdir backend configuration.
#[cfg(feature = "vdir")]
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct VdirConfig {
    /// Filesystem path of the vdir home: the directory holding one
    /// subdirectory per tasklist. Shell-expanded at load, so `~` and
    /// environment variables both work.
    #[serde(deserialize_with = "shell_expanded_path")]
    pub home_dir: PathBuf,
}

/// pimdir backend configuration.
///
/// A pimdir store is an offline cache a sync engine fills, not a
/// server: unfog reads what the store holds and stages its writes
/// for the next sync to push.
#[cfg(feature = "pimdir")]
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct PimdirConfig {
    /// The store directory, holding the SQLite index and the blob
    /// tree. Shell-expanded at load.
    #[serde(deserialize_with = "shell_expanded_path")]
    pub root: PathBuf,
    /// The account whose collections this client reads, the name the
    /// sync engine groups them under (pimdir SPEC 9.2).
    ///
    /// Usually left unset: a store synced by one account is read as
    /// that one. Set it for a store several accounts share, where
    /// guessing would show the wrong tasklists.
    #[serde(default)]
    pub account: Option<String>,
}

/// CalDAV backend configuration.
///
/// Locating the calendar home-set takes one of three routes, from most
/// to least discovery: `discover` resolves a bare domain, `server`
/// names the context root to walk from, `home` pins the home-set.
#[cfg(feature = "caldav")]
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct CaldavConfig {
    /// Bare domain resolved to a context root through RFC 6764 SRV
    /// records and the `.well-known` path.
    ///
    /// It costs DNS and HTTP round-trips on every run: prefer `server`
    /// once the answer is known.
    pub discover: Option<String>,
    /// DAV context root. Principal and calendar-home-set discovery
    /// start here, skipping the `.well-known` step.
    ///
    /// Accepts a full URL, a bare domain or `domain:port`, a bare
    /// authority defaulting to `https`.
    pub server: Option<String>,
    /// Pre-resolved calendar home-set URL, skipping every discovery
    /// step.
    pub home: Option<Url>,
    /// TLS configuration.
    #[serde(default)]
    pub tls: TlsConfig,
    /// Authentication configuration.
    pub auth: CaldavAuthConfig,
}

/// CalDAV authentication configuration.
#[cfg(feature = "caldav")]
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub enum CaldavAuthConfig {
    /// No credentials, for a server that asks for none.
    None,
    /// HTTP Basic (RFC 7617), the usual username and password, often
    /// an app password.
    Basic {
        /// The login to present.
        #[serde(deserialize_with = "shell_expanded_string")]
        username: String,
        /// The password, read from the configuration or from the
        /// standard output of a command.
        password: Secret,
    },
    /// HTTP Bearer (RFC 6750), a provider-issued API token.
    Bearer {
        /// The token, read from the configuration or from the standard
        /// output of a command. An OAuth 2.0 token broker is a command
        /// like any other.
        token: Secret,
    },
}

/// SSL/TLS configuration.
#[cfg(feature = "caldav")]
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct TlsConfig {
    /// TLS implementation. Unset takes the first compiled in, rustls
    /// before native-tls.
    pub provider: Option<TlsProviderConfig>,
    /// rustls options, ignored when native-tls is the provider.
    #[serde(default)]
    pub rustls: RustlsConfig,
    /// PEM certificate to trust, shell-expanded at load.
    ///
    /// Pinned to the server's leaf under rustls and taken as a root
    /// certificate under native-tls.
    #[serde(default, deserialize_with = "opt_shell_expanded_path")]
    pub cert: Option<PathBuf>,
}

/// Which TLS implementation carries the connection.
#[cfg(feature = "caldav")]
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub enum TlsProviderConfig {
    /// The pure-Rust rustls stack.
    Rustls,
    /// The platform's own TLS stack.
    NativeTls,
}

/// rustls-specific options.
#[cfg(feature = "caldav")]
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct RustlsConfig {
    /// Crypto provider. Unset prefers ring, else aws-lc-rs.
    pub crypto: Option<RustlsCryptoConfig>,
}

/// Which cryptographic provider backs rustls.
#[cfg(feature = "caldav")]
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub enum RustlsCryptoConfig {
    /// The aws-lc-rs provider.
    Aws,
    /// The ring provider.
    Ring,
}

#[cfg(feature = "caldav")]
impl TlsConfig {
    /// Builds the runtime [`Tls`] handle the connect helpers expect,
    /// folding in the protocol-level `alpn` list.
    ///
    /// The TOML schema never exposes `tls.rustls.alpn` directly, the
    /// per-backend `alpn` field standing for it. An empty list skips
    /// ALPN, so every caller says what it negotiates.
    pub fn into_tls(self, alpn: Vec<String>) -> Tls {
        Tls {
            provider: self.provider.map(|provider| match provider {
                TlsProviderConfig::Rustls => TlsProvider::Rustls,
                TlsProviderConfig::NativeTls => TlsProvider::NativeTls,
            }),
            rustls: Rustls {
                crypto: self.rustls.crypto.map(|crypto| match crypto {
                    RustlsCryptoConfig::Aws => RustlsCrypto::Aws,
                    RustlsCryptoConfig::Ring => RustlsCrypto::Ring,
                }),
                alpn,
            },
            cert: self.cert,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::env::var;

    use super::*;

    /// The home directory the tilde in a test path stands for.
    fn home() -> PathBuf {
        PathBuf::from(var("HOME").expect("HOME must be set to expand a tilde"))
    }

    /// Expansion happens at deserialize, so no call site can read a
    /// literal `./~/…` directory under the current one.
    #[cfg(feature = "vdir")]
    #[test]
    fn a_vdir_home_expands_its_tilde_at_deserialize() {
        let config: VdirConfig = toml::from_str(r#"home-dir = "~/calendars""#).unwrap();
        assert_eq!(config.home_dir, home().join("calendars"));
    }

    #[cfg(feature = "pimdir")]
    #[test]
    fn a_pimdir_root_expands_its_tilde_at_deserialize() {
        let config: PimdirConfig = toml::from_str(r#"root = "~/store""#).unwrap();
        assert_eq!(config.root, home().join("store"));
    }

    /// The optional path takes the same treatment, and an absent key
    /// still reaches `None` rather than the deserializer.
    #[cfg(feature = "caldav")]
    #[test]
    fn a_tls_certificate_expands_its_tilde_and_stays_optional() {
        let config: TlsConfig = toml::from_str(r#"cert = "~/ca.pem""#).unwrap();
        assert_eq!(config.cert, Some(home().join("ca.pem")));

        let config: TlsConfig = toml::from_str("").unwrap();
        assert_eq!(config.cert, None);
    }

    /// Every spelling a CalDAV endpoint is written in round-trips: a
    /// full URL stays one, and a bare authority is kept verbatim for
    /// the client to resolve.
    #[cfg(feature = "caldav")]
    #[test]
    fn a_caldav_server_takes_a_url_or_a_bare_authority() {
        for spelling in [
            "https://dav.example.org/dav/",
            "http://dav.example.org:8008/",
            "example.org",
            "dav.example.org:8443",
        ] {
            let document = format!("server = \"{spelling}\"\nauth = \"none\"");
            let config: CaldavConfig = toml::from_str(&document).unwrap();
            assert_eq!(config.server.as_deref(), Some(spelling));
        }
    }

    #[test]
    fn the_sample_configuration_parses() {
        let config: Config = toml::from_str(include_str!("../config.sample.toml")).unwrap();
        assert!(config.accounts["example"].default);
    }

    /// The wizard writes no `default = false` line, the rest of the
    /// family omitting it too.
    #[cfg(all(
        feature = "wizard",
        any(feature = "caldav", feature = "vdir", feature = "pimdir")
    ))]
    #[test]
    fn a_rendered_account_spells_the_default_flag_only_when_it_is_set() {
        let mut account = AccountConfig::default();
        assert!(!account.render("personal").unwrap().contains("default ="));

        account.default = true;
        assert!(
            account
                .render("personal")
                .unwrap()
                .contains("default = true")
        );
    }
}
