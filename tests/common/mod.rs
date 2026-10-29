//! # Test helpers
//!
//! Drives the built binary against a CalDAV server, the way a person
//! runs it, and talks to that server as another client would.
//!
//! The server is a local Radicale (tests/radicale.sh) unless
//! `UNFOG_E2E_URL` names another, reached over plain HTTP: a live
//! provider is reached through the guard proxy (tests/live.sh), which
//! carries TLS and refuses every request outside the scratch collections.
//!
//! Every collection a test writes is its own, created for the run under
//! the name `unfog-e2e-<run>-<n>` and deleted however the test ends.

#![allow(dead_code)]

use std::{
    env, fs,
    io::{Read, Write},
    net::TcpStream,
    path::{Path, PathBuf},
    process::{Command, Output},
    sync::{
        Mutex, MutexGuard, Once, PoisonError,
        atomic::{AtomicUsize, Ordering},
    },
    time::{SystemTime, UNIX_EPOCH},
};

use serde_json::Value;

/// The prefix every scratch collection carries, so a sweep finds the
/// leftovers of an earlier run and nothing else.
pub const PREFIX: &str = "unfog-e2e-";

/// A task another client wrote: folded lines, parameters, an escape, two
/// `CATEGORIES` lines, a cased `Inbox` and properties unfog does not
/// model.
pub const FOREIGN: &str = concat!(
    "BEGIN:VCALENDAR\r\n",
    "VERSION:2.0\r\n",
    "PRODID:-//Other Client//Tasks 1.0//EN\r\n",
    "BEGIN:VTODO\r\n",
    "UID:foreign-1@other.example\r\n",
    "DTSTAMP:20261001T080000Z\r\n",
    "SUMMARY;LANGUAGE=en:A summary long enough that its writer folded it over\r\n",
    "  two lines\\, with an escape\r\n",
    "CATEGORIES:work,Inbox\r\n",
    "CATEGORIES:phone-ish\r\n",
    "PRIORITY:5\r\n",
    "X-OTHER-CLIENT-STATE;X-PARAM=1:kept\r\n",
    "END:VTODO\r\n",
    "END:VCALENDAR\r\n",
);

/// A meeting relating to a document: opaque, ended by a duration.
pub const MEETING: &str = concat!(
    "BEGIN:VCALENDAR\r\n",
    "VERSION:2.0\r\n",
    "PRODID:-//Other Client//Calendar 1.0//EN\r\n",
    "BEGIN:VEVENT\r\n",
    "UID:meeting-1@other.example\r\n",
    "DTSTAMP:20261027T080000Z\r\n",
    "DTSTART:20261027T090000Z\r\n",
    "DURATION:PT1H\r\n",
    "SUMMARY:Agenda review\r\n",
    "RELATED-TO:agenda-doc-42\r\n",
    "END:VEVENT\r\n",
    "END:VCALENDAR\r\n",
);

/// The server a suite runs against.
#[derive(Clone, Debug)]
pub struct Server {
    /// Its base URL, plain HTTP, ending in `/`.
    pub url: String,
    /// The path of the calendar home, ending in `/`.
    pub home: String,
    /// The login.
    pub user: String,
    /// The command printing the password on its first line, when the
    /// password is not the literal `test` of the local Radicale.
    pub password_command: Option<String>,
    /// The password, loaded once, never printed.
    password: String,
    /// The run's id, in every scratch name.
    pub run: String,
    /// The calendar a provider refusing `MKCALENDAR` lends the run
    /// (`UNFOG_E2E_CALENDAR`, its id under the home): every scratch is
    /// that calendar, and only the items a test added are removed.
    pub borrowed: Option<String>,
}

/// How many scratch collections this process created.
static COUNT: AtomicUsize = AtomicUsize::new(0);

/// The sweep of an earlier run's leftovers, once per process.
static SWEPT: Once = Once::new();

/// One test at a time: every test lists the whole home, which holds the
/// scratch calendars of the others.
static SERIAL: Mutex<()> = Mutex::new(());

/// Holds the suite to one test at a time, until dropped, a test that
/// failed before letting the next run.
pub fn serial() -> MutexGuard<'static, ()> {
    SERIAL.lock().unwrap_or_else(PoisonError::into_inner)
}

impl Server {
    /// The server the environment names, a local Radicale by default.
    pub fn from_env() -> Self {
        let url = env::var("UNFOG_E2E_URL").unwrap_or_else(|_| "http://127.0.0.1:5243/".into());
        let url = format!("{}/", url.trim_end_matches('/'));
        let home = env::var("UNFOG_E2E_HOME").unwrap_or_else(|_| "/test/".into());
        let home = format!("/{}/", home.trim_matches('/'));
        let user = env::var("UNFOG_E2E_USER").unwrap_or_else(|_| "test".into());
        let password_command = env::var("UNFOG_E2E_PASSWORD_COMMAND").ok();

        let password = match &password_command {
            None => String::from("test"),
            Some(command) => {
                let output = Command::new("sh").arg("-c").arg(command).output().unwrap();
                assert!(output.status.success(), "the password command failed");
                let text = String::from_utf8(output.stdout).unwrap();
                text.lines().next().unwrap_or_default().to_owned()
            }
        };

        let run = env::var("UNFOG_E2E_RUN").unwrap_or_else(|_| {
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs()
                .to_string()
        });

        let server = Self {
            url,
            home,
            user,
            password_command,
            password,
            run,
            borrowed: env::var("UNFOG_E2E_CALENDAR").ok(),
        };

        SWEPT.call_once(|| server.sweep());
        server
    }

    /// The `Authorization` header value.
    fn authorization(&self) -> String {
        format!(
            "Basic {}",
            base64(format!("{}:{}", self.user, self.password).as_bytes())
        )
    }

    /// Sends one request as another client would, over a connection of
    /// its own.
    pub fn http(&self, method: &str, path: &str, headers: &[(&str, &str)], body: &str) -> Response {
        let authority = self
            .url
            .trim_start_matches("http://")
            .trim_end_matches('/')
            .to_owned();

        let mut stream = TcpStream::connect(&authority).unwrap();
        let mut request = format!(
            "{method} {path} HTTP/1.1\r\nHost: {authority}\r\nAuthorization: {}\r\n\
             Content-Length: {}\r\nConnection: close\r\n",
            self.authorization(),
            body.len()
        );
        for (name, value) in headers {
            request.push_str(&format!("{name}: {value}\r\n"));
        }
        request.push_str("\r\n");
        request.push_str(body);
        stream.write_all(request.as_bytes()).unwrap();

        let mut raw = Vec::new();
        stream.read_to_end(&mut raw).unwrap();
        Response::parse(&raw)
    }

    /// A new scratch calendar of this run taking `components`, deleted
    /// when dropped.
    pub fn scratch(&self, components: &[&str]) -> Scratch {
        match self.try_scratch(components) {
            Ok(scratch) => scratch,
            Err(response) => panic!(
                "MKCALENDAR ({components:?}) answered {} {}",
                response.status, response.body
            ),
        }
    }

    /// A new scratch calendar taking `components`, or the response of a
    /// server refusing it.
    pub fn try_scratch(&self, components: &[&str]) -> Result<Scratch, Response> {
        if let Some(id) = &self.borrowed {
            let mut scratch = Scratch {
                server: self.clone(),
                id: id.clone(),
                path: format!("{}{id}/", self.home),
                before: None,
            };
            scratch.before = Some(scratch.names());
            return Ok(scratch);
        }

        let n = COUNT.fetch_add(1, Ordering::SeqCst);
        let id = format!("{PREFIX}{}-{n}", self.run);
        let path = format!("{}{id}/", self.home);

        let set: String = components
            .iter()
            .map(|component| format!("<C:comp name=\"{component}\"/>"))
            .collect();
        let body = format!(
            "<?xml version=\"1.0\" encoding=\"utf-8\"?>\
             <C:mkcalendar xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:caldav\">\
             <D:set><D:prop><D:displayname>{id}</D:displayname>\
             <C:supported-calendar-component-set>{set}</C:supported-calendar-component-set>\
             </D:prop></D:set></C:mkcalendar>"
        );

        let response = self.http(
            "MKCALENDAR",
            &path,
            &[("Content-Type", "application/xml; charset=utf-8")],
            &body,
        );
        if response.status != 201 {
            return Err(response);
        }

        Ok(Scratch {
            server: self.clone(),
            id,
            path,
            before: None,
        })
    }

    /// Deletes every collection of the home carrying the scratch prefix,
    /// the leftovers of a run that could not clean up.
    pub fn sweep(&self) {
        // NOTE: a borrowed calendar is never swept: each test removes what
        // it added, and what it held before is the lender's.
        if self.borrowed.is_some() {
            return;
        }

        let response = self.http(
            "PROPFIND",
            &self.home,
            &[("Depth", "1"), ("Content-Type", "application/xml")],
            "<?xml version=\"1.0\"?><D:propfind xmlns:D=\"DAV:\"><D:prop><D:resourcetype/>\
             </D:prop></D:propfind>",
        );

        for href in hrefs(&response.body) {
            let name = href
                .trim_end_matches('/')
                .rsplit('/')
                .next()
                .unwrap_or_default();
            if name.starts_with(PREFIX) {
                self.http("DELETE", &href, &[], "");
            }
        }
    }
}

/// A scratch calendar, deleted when dropped.
pub struct Scratch {
    server: Server,
    /// Its id, the last segment of its path.
    pub id: String,
    /// Its path, ending in `/`.
    pub path: String,
    /// On a borrowed calendar, the items it held when lent, never
    /// removed.
    before: Option<Vec<String>>,
}

impl Scratch {
    /// The names of the resources it holds.
    fn names(&self) -> Vec<String> {
        let response = self.server.http(
            "PROPFIND",
            &self.path,
            &[("Depth", "1"), ("Content-Type", "application/xml")],
            "<?xml version=\"1.0\"?><D:propfind xmlns:D=\"DAV:\"><D:prop><D:getetag/>\
             </D:prop></D:propfind>",
        );

        hrefs(&response.body)
            .into_iter()
            .filter(|href| !href.ends_with('/'))
            .filter_map(|href| Some(href.rsplit('/').next()?.to_owned()))
            .collect()
    }

    /// The path of the resource `name` in it.
    pub fn item(&self, name: &str) -> String {
        format!("{}{name}", self.path)
    }

    /// PUTs `body` as `name`, as another client would, and answers the
    /// bytes the server serves back for it.
    pub fn put(&self, name: &str, body: &str) -> String {
        let response = self.server.http(
            "PUT",
            &self.item(name),
            &[("Content-Type", "text/calendar; charset=utf-8")],
            body,
        );
        assert!(
            matches!(response.status, 201 | 204),
            "PUT {name} answered {} {}",
            response.status,
            response.body
        );
        self.get(name).expect("the resource just written")
    }

    /// The bytes the server serves for `name`, `None` when it has none.
    pub fn get(&self, name: &str) -> Option<String> {
        let response = self.server.http("GET", &self.item(name), &[], "");
        match response.status {
            200 => Some(response.body),
            404 => None,
            status => panic!("GET {name} answered {status} {}", response.body),
        }
    }

    /// Every resource of the calendar, by name, with its bytes.
    pub fn items(&self) -> Vec<(String, String)> {
        let response = self.server.http(
            "PROPFIND",
            &self.path,
            &[("Depth", "1"), ("Content-Type", "application/xml")],
            "<?xml version=\"1.0\"?><D:propfind xmlns:D=\"DAV:\"><D:prop><D:getetag/>\
             </D:prop></D:propfind>",
        );

        hrefs(&response.body)
            .into_iter()
            .filter(|href| !href.ends_with('/'))
            .filter_map(|href| {
                let name = href.rsplit('/').next()?.to_owned();
                let body = self.get(&name)?;
                Some((name, body))
            })
            .collect()
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        if let Some(before) = &self.before {
            for name in self.names() {
                if before.contains(&name) {
                    continue;
                }
                let response = self.server.http("DELETE", &self.item(&name), &[], "");
                if !matches!(response.status, 200 | 204 | 404) {
                    eprintln!("DELETE {name} answered {}; remove it by hand", response.status);
                }
            }
            return;
        }

        let response = self.server.http("DELETE", &self.path, &[], "");
        if !matches!(response.status, 200 | 204 | 404) {
            eprintln!(
                "DELETE {} answered {}; the next run sweeps it",
                self.path, response.status
            );
        }
    }
}

/// A response, its body de-chunked.
#[derive(Clone, Debug)]
pub struct Response {
    pub status: u16,
    pub headers: Vec<(String, String)>,
    pub body: String,
}

impl Response {
    fn parse(raw: &[u8]) -> Self {
        let split = raw
            .windows(4)
            .position(|window| window == b"\r\n\r\n")
            .expect("a response head");
        let head = String::from_utf8_lossy(&raw[..split]).into_owned();
        let mut body = raw[split + 4..].to_vec();

        let mut lines = head.lines();
        let status = lines
            .next()
            .and_then(|line| line.split_whitespace().nth(1))
            .and_then(|code| code.parse().ok())
            .unwrap_or(0);
        let headers: Vec<(String, String)> = lines
            .filter_map(|line| line.split_once(':'))
            .map(|(name, value)| (name.trim().to_ascii_lowercase(), value.trim().to_owned()))
            .collect();

        let chunked = headers.iter().any(|(name, value)| {
            name == "transfer-encoding" && value.eq_ignore_ascii_case("chunked")
        });
        if chunked {
            body = dechunk(&body);
        }

        Self {
            status,
            headers,
            body: String::from_utf8_lossy(&body).into_owned(),
        }
    }

    /// The value of the header `name`.
    pub fn header(&self, name: &str) -> Option<&str> {
        self.headers
            .iter()
            .find(|(key, _)| key.eq_ignore_ascii_case(name))
            .map(|(_, value)| value.as_str())
    }
}

/// A chunked body joined.
fn dechunk(mut raw: &[u8]) -> Vec<u8> {
    let mut body = Vec::new();

    while let Some(end) = raw.windows(2).position(|window| window == b"\r\n") {
        let size = String::from_utf8_lossy(&raw[..end]);
        let size = usize::from_str_radix(size.split(';').next().unwrap().trim(), 16).unwrap_or(0);
        if size == 0 {
            break;
        }
        let start = end + 2;
        body.extend_from_slice(&raw[start..start + size]);
        raw = &raw[start + size + 2..];
    }

    body
}

/// Every `href` of a multistatus body, percent-encoding left as is.
pub fn hrefs(body: &str) -> Vec<String> {
    let mut hrefs = Vec::new();
    let mut rest = body;

    while let Some(at) = rest.find("href>") {
        rest = &rest[at + 5..];
        let Some(end) = rest.find('<') else {
            break;
        };
        let href = rest[..end].trim();
        if !href.is_empty() {
            let path = match href.find("://") {
                Some(scheme) => href[scheme + 3..]
                    .find('/')
                    .map_or("/", |slash| &href[scheme + 3 + slash..]),
                None => href,
            };
            hrefs.push(path.to_owned());
        }
        rest = &rest[end..];
    }

    hrefs
}

/// Standard base64 with padding.
fn base64(bytes: &[u8]) -> String {
    const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::new();

    for chunk in bytes.chunks(3) {
        let n = chunk.iter().enumerate().fold(0u32, |n, (index, byte)| {
            n | (u32::from(*byte) << (16 - 8 * index))
        });
        for index in 0..4 {
            match index <= chunk.len() {
                true => out.push(ALPHABET[((n >> (18 - 6 * index)) & 63) as usize] as char),
                false => out.push('='),
            }
        }
    }

    out
}

/// The binary under test, run against one configuration file.
pub struct Unfog {
    /// The configuration the binary reads.
    pub config: PathBuf,
    /// Where it keeps the local time directory and other files.
    pub dir: PathBuf,
}

impl Unfog {
    /// A configuration under `dir` holding one default account over
    /// `server`, then `extra` verbatim (time options, say).
    pub fn new(dir: &Path, server: &Server, extra: &str) -> Self {
        let password = match &server.password_command {
            Some(command) => format!(
                "caldav.auth.basic.password.command = {}\n",
                toml_string(command)
            ),
            None => String::from("caldav.auth.basic.password.raw = \"test\"\n"),
        };

        let toml = format!(
            "[accounts.e2e]\ndefault = true\ncaldav.home = \"{}{}\"\n\
             caldav.auth.basic.username = {}\n{password}time.directory = \"{}\"\n{extra}",
            server.url.trim_end_matches('/'),
            server.home,
            toml_string(&server.user),
            dir.join("time").display(),
        );

        let config = dir.join("config.toml");
        fs::write(&config, toml).unwrap();

        Self {
            config,
            dir: dir.to_path_buf(),
        }
    }

    /// Runs the binary with `args`, under `--json` when `json` says so.
    pub fn run(&self, json: bool, args: &[&str]) -> Output {
        let mut command = Command::new(env!("CARGO_BIN_EXE_unfog"));
        command.arg("-c").arg(&self.config).arg("-b").arg("caldav");

        if json {
            command.arg("--json");
        }

        command.args(args).output().unwrap()
    }

    /// Runs the binary under `--json`, failing the test unless it
    /// succeeds, and parses what it printed.
    pub fn json(&self, args: &[&str]) -> Value {
        let output = self.run(true, args);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            output.status.success(),
            "unfog {args:?} failed: {stdout}{}",
            String::from_utf8_lossy(&output.stderr)
        );
        serde_json::from_str(&stdout).unwrap()
    }
}

/// A TOML basic string.
fn toml_string(value: &str) -> String {
    format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\""))
}

/// The lines of `bytes` that differ from `base`, both unfolded, as
/// `(removed, added)`.
pub fn changed_lines(base: &str, bytes: &str) -> (Vec<String>, Vec<String>) {
    let unfold = |text: &str| -> Vec<String> {
        text.replace("\r\n ", "")
            .replace("\r\n\t", "")
            .split("\r\n")
            .filter(|line| !line.is_empty())
            .map(str::to_owned)
            .collect()
    };
    let (base, bytes) = (unfold(base), unfold(bytes));

    let removed = base
        .iter()
        .filter(|line| !bytes.contains(line))
        .cloned()
        .collect();
    let added = bytes
        .iter()
        .filter(|line| !base.contains(line))
        .cloned()
        .collect();
    (removed, added)
}
