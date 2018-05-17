#!/usr/bin/env run-cargo-script
//! ```cargo
//! [package]
//! name = "cmus-title"
//! version = "0.0.0"
//! license = "CC0-1.0"
//! authors = ["wariuni <wariuni@gmail.com>"]
//! repository = "https://github.com/wariuni/daily-scripts"
//!
//! [dependencies]
//! structopt = "0.2.8"
//! ```
#[macro_use]
extern crate structopt;

use structopt::StructOpt as _StructOpt;

use std::io::{self, BufWriter, Write as _Write};
use std::process::{Command, Stdio};
use std::time::Duration;
use std::{str, thread};

#[derive(StructOpt)]
#[structopt(name = "cmus-title", usage = "cmus-title [-i|--interval <interval>]")]
struct Opt {
    #[structopt(
        short = "i", long = "interval", help = "Interval (by millisecond)", default_value = "1000"
    )]
    interval: u64,
}

fn main() {
    let Opt { interval } = Opt::from_args();
    let interval = Duration::from_millis(interval);
    let stdout = io::stdout();
    let mut stdout = BufWriter::new(stdout.lock());
    loop {
        match cmus_remote_query() {
            Err(e) => match e.kind() {
                io::ErrorKind::NotFound => {
                    writeln!(stdout, "  \"cmus-remote\" not found").unwrap()
                }
                _ => writeln!(stdout, "  {}", e).unwrap(),
            },
            Ok(cmus_output) => match cmus_status(&cmus_output) {
                CmusStatus::CmusNotRunning => writeln!(stdout).unwrap(),
                CmusStatus::Stopped => writeln!(stdout, "  Selecting").unwrap(),
                CmusStatus::Paused(title) => writeln!(stdout, "  {}", title).unwrap(),
                CmusStatus::Playing(title) => writeln!(stdout, "  {}", title).unwrap(),
            },
        }
        stdout.flush().unwrap();
        thread::sleep(interval);
    }
}

fn cmus_remote_query() -> io::Result<String> {
    let mut cmus_stdout = Command::new("cmus-remote")
        .arg("-Q")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()?
        .stdout
        .unwrap();
    let mut buf = Vec::with_capacity(2048);
    let _ = io::copy(&mut cmus_stdout, &mut buf)?;
    String::from_utf8(buf).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))
}

fn cmus_status(cmus_stdout: &str) -> CmusStatus {
    let mut lines = cmus_stdout.lines();
    let music_status = match lines.next() {
        Some("status stopped") => return CmusStatus::Stopped,
        Some("status paused") => MusicStatus::Paused,
        Some("status playing") => MusicStatus::Playing,
        _ => return CmusStatus::CmusNotRunning,
    };
    lines
        .filter(|l| l.starts_with("tag title "))
        .map(|l| music_status.with_title(unsafe { str::from_utf8_unchecked(&l.as_bytes()[10..]) }))
        .next()
        .unwrap_or(CmusStatus::CmusNotRunning)
}

enum CmusStatus<'a> {
    CmusNotRunning,
    Stopped,
    Paused(&'a str),
    Playing(&'a str),
}

#[derive(Clone, Copy)]
enum MusicStatus {
    Paused,
    Playing,
}

impl MusicStatus {
    fn with_title(self, title: &str) -> CmusStatus {
        match self {
            MusicStatus::Paused => CmusStatus::Paused(title),
            MusicStatus::Playing => CmusStatus::Playing(title),
        }
    }
}
