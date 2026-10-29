//! # Writer vectors
//!
//! pimdir's GTD writer vectors (vectors/gtd/writers.json), run against
//! unfog's writers: each case an input VTODO, an edit and the bytes the
//! edit writes, so unfog and the Pimalaya app edit a task alike.
//!
//! The vectors are read from the pimdir checkout beside this one, or from
//! the directory `UNFOG_PIMDIR_VECTORS` names; without either the run says
//! so and passes over them.

use std::{fs, path::PathBuf};

use jiff::{Timestamp, civil::DateTime, tz::TimeZone};
use serde_json::Value;

use crate::{
    gtd::capture::capture_body,
    shared::{
        item::TaskItem,
        task::{
            Task,
            fields::revise,
            gtd::{GtdState, own_categories, set_contact, set_state},
        },
    },
};

/// What an edit came to.
#[derive(Debug, PartialEq)]
enum Outcome {
    /// These bytes were written.
    Written(Vec<u8>),
    /// Nothing was written.
    Unchanged,
    /// The edit was refused.
    Refused,
}

/// The directory holding writers.json.
fn directory() -> PathBuf {
    match std::env::var_os("UNFOG_PIMDIR_VECTORS") {
        Some(dir) => PathBuf::from(dir),
        None => PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../pimdir/vectors/gtd"),
    }
}

/// The written bytes, else nothing when the edit changed none.
fn written(input: &[u8], output: Vec<u8>, now: Timestamp) -> Outcome {
    match output == input {
        true => Outcome::Unchanged,
        false => Outcome::Written(revise(&output, now).unwrap()),
    }
}

/// Runs `edit` over `input` at `now` with unfog's writers.
fn run(edit: &Value, input: &[u8], now: Timestamp) -> Outcome {
    let op = edit["op"].as_str().unwrap();

    match op {
        "set-state" => {
            let state = edit["state"]
                .as_str()
                .map(|state| GtdState::of(state).unwrap());
            match set_state(input, state, now) {
                Ok(Some(bytes)) => Outcome::Written(bytes),
                Ok(None) => Outcome::Unchanged,
                Err(_) => Outcome::Refused,
            }
        }
        "set-categories" => {
            let categories: Vec<String> = edit["categories"]
                .as_array()
                .unwrap()
                .iter()
                .map(|value| value.as_str().unwrap().to_owned())
                .collect();
            match own_categories(input, &categories) {
                Ok(output) => written(input, output, now),
                Err(_) => Outcome::Refused,
            }
        }
        "set-contact" => {
            let contact = match (edit["name"].as_str(), edit["address"].as_str()) {
                (Some(name), Some(address)) => format!("{name} <{address}>"),
                (Some(name), None) => name.to_owned(),
                (None, Some(address)) => address.to_owned(),
                (None, None) => String::new(),
            };
            match set_contact(input, Some(&contact)) {
                Ok(output) => written(input, output, now),
                Err(_) => Outcome::Refused,
            }
        }
        op => panic!("no writer for `{op}`"),
    }
}

/// The unfolded lines of the VTODO of `bytes`.
fn vtodo_lines(bytes: &[u8]) -> Vec<String> {
    let text = String::from_utf8_lossy(bytes)
        .replace("\r\n ", "")
        .replace("\r\n\t", "");
    let start = text.find("BEGIN:VTODO\r\n").unwrap() + "BEGIN:VTODO\r\n".len();
    let end = text.find("END:VTODO\r\n").unwrap();
    text[start..end]
        .split("\r\n")
        .filter(|line| !line.is_empty())
        .map(str::to_owned)
        .collect()
}

#[test]
fn unfog_writes_as_pimdir_writer_vectors_say() {
    let dir = directory();
    let Ok(json) = fs::read_to_string(dir.join("writers.json")) else {
        eprintln!(
            "no pimdir writer vectors at {}; passing over them",
            dir.display()
        );
        return;
    };
    let vectors: Value = serde_json::from_str(&json).unwrap();

    let now = vectors["now"].as_str().unwrap();
    let now = DateTime::strptime("%Y%m%dT%H%M%S", now.trim_end_matches('Z'))
        .unwrap()
        .to_zoned(TimeZone::UTC)
        .unwrap()
        .timestamp();

    let mut failures = Vec::new();

    for case in vectors["cases"].as_array().unwrap() {
        let label = case["label"].as_str().unwrap();
        let edit = &case["edit"];
        let expect = &case["expect"];
        let input = case["input"]
            .as_str()
            .map(|path| fs::read(dir.join(path)).unwrap());

        match edit["op"].as_str().unwrap() {
            "capture" => {
                let summary = edit["summary"].as_str().unwrap();
                let body = capture_body(summary, None, None, now).unwrap();
                let lines = vtodo_lines(&body);

                for line in expect["vtodo"].as_array().unwrap() {
                    let line = line.as_str().unwrap();
                    if !lines.iter().any(|held| held == line) {
                        failures.push(format!("{label}: no `{line}` in {lines:?}"));
                    }
                }
                for name in expect["names"].as_array().unwrap() {
                    let name = format!("{}:", name.as_str().unwrap());
                    if !lines.iter().any(|held| held.starts_with(&name)) {
                        failures.push(format!("{label}: no {name} in {lines:?}"));
                    }
                }
            }
            "captured-at" => {
                let item = TaskItem {
                    contents: input.unwrap(),
                    ..Default::default()
                };
                let task = Task::project(&item).remove(0);
                let (_, value) = task.captured_key();
                if value != expect["value"].as_str().unwrap() {
                    failures.push(format!("{label}: read {value}"));
                }
            }
            _ => {
                let input = input.unwrap();
                let outcome = run(edit, &input, now);
                let wanted = match (&expect["bytes"], &expect["unchanged"], &expect["refused"]) {
                    (Value::String(path), _, _) => {
                        Outcome::Written(fs::read(dir.join(path)).unwrap())
                    }
                    (_, Value::Bool(true), _) => Outcome::Unchanged,
                    (_, _, Value::Bool(true)) => Outcome::Refused,
                    _ => panic!("{label}: no expectation"),
                };

                if outcome != wanted {
                    let show = |outcome: &Outcome| match outcome {
                        Outcome::Written(bytes) => String::from_utf8_lossy(bytes).into_owned(),
                        other => format!("{other:?}"),
                    };
                    failures.push(format!(
                        "{label}:\n--- wanted\n{}\n--- written\n{}",
                        show(&wanted),
                        show(&outcome)
                    ));
                }
            }
        }
    }

    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}
