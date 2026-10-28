//! # CalDAV client
//!
//! unfog's wrapper around [`io_webdav`]'s std client. Builds a
//! connected client from a [`CaldavConfig`], leaving its discovery
//! caches populated so a command runs no extra round-trip.

use std::{
    ops::{Deref, DerefMut},
    path::PathBuf,
};

use anyhow::{Result, anyhow};
use io_http::{rfc6750::bearer::HttpAuthBearer, rfc7617::basic::HttpAuthBasic};
use io_pim_discovery::{
    rfc6764::{client::DiscoveryWebdavClientStd, service::DiscoveryDavService},
    shared::dns::system_resolver,
};
use io_webdav::{
    client::{WebdavClientStd, WebdavClientStdConnectOptions},
    rfc4918::WebdavAuth,
};
use pimalaya_cli::printer::Printer;
use pimalaya_config::{secret::SecretResolver, toml::TomlConfig};
use pimalaya_stream::{proxy::Proxy, tls::Tls};
use secrecy::ExposeSecret;
use url::Url;

use crate::{
    account::context::Account,
    cli::load_config,
    config::{CaldavAuthConfig, CaldavConfig},
};

/// The DNS-over-TCP resolver used when the host exposes none.
const DEFAULT_RESOLVER: &str = "tcp://1.1.1.1:53";

/// A connected CalDAV client bundled with the merged runtime
/// [`Account`], for the protocol-specific subcommands.
pub struct CaldavClient {
    inner: WebdavClientStd,
    /// The merged account the commands read their rendering from.
    pub account: Account,
}

impl CaldavClient {
    /// Wraps an already-connected client.
    pub fn new(inner: WebdavClientStd, account: Account) -> Self {
        Self { inner, account }
    }
}

impl Deref for CaldavClient {
    type Target = WebdavClientStd;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for CaldavClient {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// Opens a connected CalDAV client and walks the discovery chain,
/// resolving the credential through `resolver` so a command another
/// backend of the same account names too is spawned once.
///
/// Follows whichever route the config sets: `home` pins the home-set
/// and skips discovery, `server` names the context root the principal
/// walk starts from, `discover` resolves a bare domain to it (RFC 6764).
pub fn connect(config: &CaldavConfig, resolver: &mut SecretResolver) -> Result<WebdavClientStd> {
    let auth = build_auth(&config.auth, resolver)?;
    let tls = build_tls(config);

    if let Some(home) = &config.home {
        let opts = WebdavClientStdConnectOptions {
            tls,
            proxy: Proxy::None,
        };
        let mut client = WebdavClientStd::connect(home, auth, opts)?;
        client.calendar_home_set = Some(home.clone());
        return Ok(client);
    }

    let server = match &config.server {
        Some(server) => parse_server(server)?,
        None => {
            let domain = config
                .discover
                .as_ref()
                .ok_or_else(|| anyhow!("CalDAV config needs `discover`, `server` or `home`"))?;
            resolve_server(domain, &tls)?
        }
    };

    let opts = WebdavClientStdConnectOptions {
        tls,
        proxy: Proxy::None,
    };
    let mut client = WebdavClientStd::connect(&server, auth, opts)?;
    client.calendar_home_set()?;

    Ok(client)
}

/// Parses a `server` config string into a [`Url`].
///
/// Accepts a full URL, a bare domain, or `domain:port`; anything
/// without an explicit `http` or `https` scheme defaults to `https://`,
/// since `url` would otherwise read the leading label of `domain:port`
/// as the scheme.
pub fn parse_server(server: &str) -> Result<Url> {
    let url = match Url::parse(server) {
        Ok(url) if matches!(url.scheme(), "http" | "https") => url,
        _ => Url::parse(&format!("https://{server}"))?,
    };

    Ok(url)
}

/// Resolves a bare domain to a CalDAV context root through RFC 6764.
fn resolve_server(domain: &str, tls: &Tls) -> Result<Url> {
    let mut client = DiscoveryWebdavClientStd::new(resolver()).with_tls(tls.clone());
    Ok(client.resolve(domain, DiscoveryDavService::Caldav)?)
}

/// The resolver discovery queries.
///
/// The system one, so the domain is not leaked to a third party,
/// falling back to the Cloudflare default on hosts exposing none.
pub fn resolver() -> Url {
    system_resolver().unwrap_or_else(|| {
        DEFAULT_RESOLVER
            .parse()
            .expect("DEFAULT_RESOLVER must be a valid URL")
    })
}

/// The TLS profile CalDAV connects with.
///
/// WebDAV speaks HTTP/1.1 only, so the ALPN list pins it rather than
/// letting a server negotiate HTTP/2.
fn build_tls(config: &CaldavConfig) -> Tls {
    config.tls.clone().into_tls(vec!["http/1.1".into()])
}

/// Resolves the configured credentials into the auth io-webdav sends.
///
/// A secret backed by a command is run here, at first use, unless
/// `resolver` already holds what that same command answered.
fn build_auth(config: &CaldavAuthConfig, resolver: &mut SecretResolver) -> Result<WebdavAuth> {
    Ok(match config {
        CaldavAuthConfig::None => WebdavAuth::None,
        CaldavAuthConfig::Basic { username, password } => WebdavAuth::Basic(HttpAuthBasic {
            username: username.clone(),
            password: resolver.resolve(password.clone())?,
        }),
        CaldavAuthConfig::Bearer { token } => {
            let token = resolver.resolve(token.clone())?;
            WebdavAuth::Bearer(HttpAuthBearer::new(token.expose_secret()))
        }
    })
}

/// Loads the configuration, picks the active account, then connects.
///
/// Bails when the account carries no `[caldav]` block.
pub fn build_caldav_client(
    printer: &mut impl Printer,
    config_paths: &[PathBuf],
    account_name: Option<&str>,
) -> Result<CaldavClient> {
    let mut config = load_config(printer, config_paths)?;
    let (name, mut account_config) = config
        .take_account(account_name)?
        .ok_or_else(|| anyhow!("Cannot find account"))?;

    let caldav_config = account_config
        .caldav
        .take()
        .ok_or_else(|| anyhow!("CalDAV configuration is missing for account `{name}`"))?;

    let account = Account::from(config).merge(Account::from(account_config));
    let inner = connect(&caldav_config, &mut SecretResolver::new())?;

    Ok(CaldavClient::new(inner, account))
}

#[cfg(test)]
mod tests {
    use super::parse_server;

    /// Every spelling a user writes a context root in resolves, and a
    /// bare authority takes `https` rather than reading its first label
    /// as a scheme.
    #[test]
    fn a_bare_authority_defaults_to_https_and_a_full_url_is_kept() {
        let parsed = |server| parse_server(server).unwrap().to_string();

        assert_eq!(
            parsed("https://dav.example.org/dav/"),
            "https://dav.example.org/dav/"
        );
        assert_eq!(
            parsed("http://dav.example.org:8008/"),
            "http://dav.example.org:8008/"
        );
        assert_eq!(parsed("example.org"), "https://example.org/");
        assert_eq!(
            parsed("dav.example.org:8443"),
            "https://dav.example.org:8443/"
        );
    }
}
