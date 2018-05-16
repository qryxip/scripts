#!/usr/bin/env run-cargo-script
//! ```cargo
//! [package]
//! name = "wi2-300-login"
//! version = "0.0.0"
//! license = "CC0-1.0"
//! authors = ["wariuni <wariuni@gmail.com>"]
//! repository = "https://github.com/wariuni/daily-scripts"
//!
//! [dependencies]
//! failure = "0.1.1"
//! itertools = "0.7.8"
//! regex = "1.0.0"
//! reqwest = "0.8.5"
//! select = "0.4.2"
//! serde = "1.0.55"
//! serde_derive = "1.0.55"
//! serde_urlencoded = "0.5.1"
//! structopt = "0.2.8"
//! term = "0.5.1"
//! ```
#[macro_use]
extern crate failure;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate structopt;

extern crate itertools;
extern crate regex;
extern crate reqwest;
extern crate select;
extern crate serde;
extern crate serde_urlencoded;
extern crate term;

use failure::ResultExt as _ResultExt;
use itertools::Itertools as _Itertools;
use regex::Regex;
use reqwest::header::{Headers, Location, UserAgent};
use reqwest::{Method, RedirectPolicy, Response, StatusCode};
use select::document::Document;
use select::predicate::{Attr, Name, Predicate as _Predicate};
use serde::Serialize;
use structopt::StructOpt as _StructOpt;

use std::io::{self, Read, Write as _Write};
use std::process::{self, Command};
use std::time::Duration;

static USER_AGENT: &str = "wi2-300-login <https://github.com/wariuni/daily-scripts>";
static ENVCHAIN_GROUP: &str = "wi2-300";
static ENVVAR_NAME_USERNAME: &str = "WI2_300_USERNAME";
static ENVVAR_NAME_PASSWORD: &str = "WI2_300_PASSWORD";

#[derive(StructOpt)]
#[structopt(name = "wi2-300-login", usage = "wi2-300-login [-t|--timeout <timeout>]")]
struct Opt {
    #[structopt(long = "timeout", short = "t", help = "Timeout (by second)")]
    timeout: Option<u64>,
}

fn main() -> std::result::Result<(), failure::Error> {
    let timeout = Opt::from_args().timeout.map(Duration::from_secs);
    let username = load_from_envchain(ENVVAR_NAME_USERNAME)?;
    let password = load_from_envchain(ENVVAR_NAME_PASSWORD)?;
    let client = reqwest_client(timeout)?;
    http_get(&client, "/robots.txt", &[404])?;
    let response = http_get(&client, "/wi2net/Login/2/", &[200, 302])?;
    if response.status() == StatusCode::Found {
        eprintln!("You have already logged in, or you are connecting to different SSID");
        process::exit(1);
    }
    let payload = Payload {
        postKey: extract_token(response)?,
        id: username,
        pass: password,
    };
    let response = http_post_urlencoded(&client, "/wi2net/Login/1/?Wi2=1", &[200, 302], &payload)?;
    if response.status() == StatusCode::Ok {
        eprintln!("You have already logged in, or you are connecting to different SSID");
        process::exit(1);
    }
    if let Some(location) = response.headers().get::<Location>() {
        let correct_location = Regex::new(r"\A/wi2net/Top/2/?(\?SSID=[0-9a-f]+)?/?\z").unwrap();
        if correct_location.is_match(location) {
            println!("Successfully logged in.");
            return Ok(());
        }
    }
    eprintln!("Failed to login. Are username or password correct?");
    process::exit(1);
}

fn load_from_envchain(envvar_name: &'static str) -> std::result::Result<String, failure::Error> {
    let output = Command::new("envchain")
        .arg(ENVCHAIN_GROUP)
        .arg("sh")
        .arg("-c")
        .arg(format!("printf %s ${}", envvar_name))
        .output()
        .context("Failed to execute \"envchain\"")?;
    if output.stdout.is_empty() {
        bail!("Got empty value: {:?} on {:?}", envvar_name, ENVCHAIN_GROUP);
    } else if output.status.success() && !output.stdout.is_empty() && output.stderr.is_empty() {
        Ok(String::from_utf8(output.stdout)?)
    } else {
        bail!(
            "{:?} ({})",
            String::from_utf8_lossy(&output.stderr),
            output.status
        );
    }
}

fn reqwest_client(timeout: Option<Duration>) -> reqwest::Result<reqwest::Client> {
    reqwest::Client::builder()
        .redirect(RedirectPolicy::none())
        .timeout(timeout)
        .default_headers({
            let mut headers = Headers::new();
            headers.set(UserAgent::new(USER_AGENT));
            headers
        })
        .build()
}

fn http_get(
    client: &reqwest::Client,
    relative_url: &str,
    expected_statuses: &[u16],
) -> std::result::Result<Response, failure::Error> {
    http_request(client, relative_url, expected_statuses, Method::Get, None)
}

fn http_post_urlencoded(
    client: &reqwest::Client,
    relative_url: &str,
    expected_statuses: &[u16],
    body: &impl Serialize,
) -> std::result::Result<Response, failure::Error> {
    let body = serde_urlencoded::to_string(body)?;
    http_request(client, relative_url, expected_statuses, Method::Get, body)
}

fn http_request(
    client: &reqwest::Client,
    relative_url: &str,
    expected_statuses: &[u16],
    method: Method,
    body: impl Into<Option<String>>,
) -> std::result::Result<Response, failure::Error> {
    static BASE: &str = "https://service.wi2.ne.jp";
    let url = format!("{}{}", BASE, relative_url);
    let mut term_stdout = term::stdout().unwrap();

    let _ = term_stdout.attr(term::Attr::Bold);
    write!(term_stdout, "{}", method)?;
    let _ = term_stdout.reset();
    let _ = term_stdout
        .fg(123)
        .or_else(|_| term_stdout.fg(10))
        .or_else(|_| term_stdout.fg(2));
    write!(term_stdout, " {}", url)?;
    let _ = term_stdout.reset();
    print!(" ... ");
    io::stdout().flush().unwrap();

    let response = {
        let mut request = client.request(method, &url);
        if let Some(body) = body.into() {
            request.body(body);
        }
        request.send()?
    };
    let success = expected_statuses.contains(&response.status().as_u16());

    let _ = if success {
        term_stdout
            .fg(118)
            .or_else(|_| term_stdout.fg(10))
            .or_else(|_| term_stdout.fg(2))
    } else {
        term_stdout
            .fg(196)
            .or_else(|_| term_stdout.fg(9))
            .or_else(|_| term_stdout.fg(1))
    };
    writeln!(term_stdout, "{}", response.status())?;
    let _ = term_stdout.reset();
    io::stdout().flush().unwrap();

    if !success {
        bail!(
            "Expected [{}], got {}",
            expected_statuses
                .iter()
                .map(|&n| StatusCode::try_from(n).unwrap_or_else(|_| StatusCode::Unregistered(n)))
                .format(", "),
            response.status()
        );
    }
    Ok(response)
}

fn extract_token(response: impl Read) -> std::result::Result<String, failure::Error> {
    Document::from_read(response)?
        .find(Name("input").and(Attr("name", "postKey")))
        .next()
        .and_then(|node| node.attr("value"))
        .map(str::to_owned)
        .ok_or_else(|| failure::err_msg("Not found: <input name=\"postKey\", value=...>"))
}

#[allow(non_snake_case)]
#[derive(Serialize)]
struct Payload {
    postKey: String,
    id: String,
    pass: String,
}
