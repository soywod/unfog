//! # CalDAV wizard
//!
//! Two entry points, one per way the endpoint became known. A discovery entry
//! pins the context root and the advertised methods, so
//! [`configure_discovered`] prompts only what is left, while
//! [`configure_manual`] takes a typed server URL and prompts the scheme too.
//!
//! Neither connects: the wizard validates the whole account once at the end,
//! and the runtime walks the principal and calendar home-set from the stored
//! `server`.

use anyhow::Result;
use pimalaya_cli::prompt;
use url::Url;

use crate::{
    config::{CaldavAuthConfig, CaldavConfig},
    wizard::{
        search::{AuthCaps, Discovered},
        secret,
    },
};

const BASIC: &str = "Basic (username + password)";
const BEARER: &str = "Bearer (API token)";

/// Configures CalDAV from a discovered entry: the context root is pinned and
/// the scheme picked among those advertised, skipped when only one
/// qualifies.
pub fn configure_discovered(
    account_name: &str,
    email: &str,
    discovered: &Discovered,
) -> Result<CaldavConfig> {
    let auth = prompt_auth(
        account_name,
        discovered.login_default(email).as_deref(),
        discovered.auth,
    )?;

    Ok(config(discovered.server.clone(), auth))
}

/// Configures CalDAV from a typed server URL.
///
/// Nothing was discovered, so every scheme is offered. Radicale, Baikal and
/// friends publish neither an SRV record nor a `.well-known` redirect, and
/// refusing them would put the servers unfog's users run out of reach.
pub fn configure_manual(account_name: &str, server: Url) -> Result<CaldavConfig> {
    let auth = prompt_auth(account_name, None, AuthCaps::default())?;
    Ok(config(server, auth))
}

/// Prompts the HTTP authentication scheme from `caps`, then its credentials.
///
/// Every scheme is offered when nothing was advertised, so an undiscovered
/// server is never left unconfigurable. The token flow shows the OAuth
/// brokers only when a grant was advertised, or when nothing was.
fn prompt_auth(
    account_name: &str,
    login_hint: Option<&str>,
    caps: AuthCaps,
) -> Result<CaldavAuthConfig> {
    let mut schemes = Vec::new();

    if caps.basic || !caps.any() {
        schemes.push(BASIC);
    }

    if caps.token() || !caps.any() {
        schemes.push(BEARER);
    }

    let scheme = if schemes.len() == 1 {
        schemes[0]
    } else {
        prompt::item("CalDAV authentication:", schemes, None)?
    };

    let key = format!("{account_name}-caldav");

    Ok(match scheme {
        BASIC => {
            let username = prompt::text("CalDAV username:", login_hint)?;
            let password = secret::configure_password("CalDAV password", &key)?;
            CaldavAuthConfig::Basic { username, password }
        }
        _ => {
            let token =
                secret::configure_token("CalDAV API token", &key, caps.oauth || !caps.any())?;
            CaldavAuthConfig::Bearer { token }
        }
    })
}

/// Folds the endpoint and credentials into a config block.
///
/// The context root is stored as `server`, not a bare `discover` domain:
/// discovery already ran, and pinning what it found spares every later run
/// the DNS and HTTP round-trips.
fn config(server: Url, auth: CaldavAuthConfig) -> CaldavConfig {
    CaldavConfig {
        discover: None,
        server: Some(server.to_string()),
        home: None,
        tls: Default::default(),
        auth,
    }
}
