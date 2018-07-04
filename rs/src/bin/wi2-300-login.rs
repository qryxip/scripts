#!/usr/bin/env run-cargo-script
//! ```cargo
//! [package]
//! name = "wi2-300-login"
//! version = "0.0.0"
//! license = "CC0-1.0"
//! authors = ["wariuni <wariuni@gmail.com>"]
//! repository = "https://github.com/wariuni/scripts"
//!
//! [dependencies]
//! failure = "~0.1.1"
//! itertools = "~0.7.8"
//! maplit = "~1.0.1"
//! regex = "~1.0.1"
//! reqwest = "~0.8.6"
//! select = "~0.4.2"
//! serde = "~1.0.69"
//! serde_urlencoded = "~0.5.2"
//! structopt = "~0.2.10"
//! termcolor = "~0.3.6"
//! ```
#[macro_use]
extern crate failure;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate structopt;

extern crate itertools;
extern crate regex;
extern crate reqwest;
extern crate select;
extern crate serde;
extern crate serde_urlencoded;
extern crate termcolor;

use failure::ResultExt as _ResultExt;
use itertools::Itertools as _Itertools;
use regex::Regex;
use reqwest::header::{Headers, Location, UserAgent};
use reqwest::{IntoUrl, Method, RedirectPolicy, Response, StatusCode, Url, UrlError};
use select::document::Document;
use select::predicate::{Attr, Name, Predicate as _Predicate};
use serde::Serialize;
use structopt::StructOpt as _StructOpt;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor as _WriteColor};

use std::io::Write as _Write;
use std::process::{self, Command};
use std::str::FromStr;
use std::time::Duration;

static USER_AGENT: &str = "wi2-300-login <https://github.com/wariuni/scripts>";
static ENVCHAIN_GROUP: &str = "wi2-300";
static ENVVAR_NAME_USERNAME: &str = "WI2_300_USERNAME";
static ENVVAR_NAME_PASSWORD: &str = "WI2_300_PASSWORD";

#[derive(StructOpt)]
#[structopt(
    name = "wi2-300-login", usage = "wi2-300-login [-t|--timeout <timeout>] [-C|--color <WHEN>]"
)]
struct Opt {
    #[structopt(
        short = "t", long = "timeout", help = "Timeout (by second)", raw(display_order = "1")
    )]
    timeout: Option<u64>,
    #[structopt(
        short = "C",
        long = "color",
        name = "WHEN",
        help = "Color",
        default_value = "auto",
        raw(possible_values = r#"&["always", "ansi", "auto", "never"]"#, display_order = "2")
    )]
    color_choice: ColorChoiceFromStr,
}

struct ColorChoiceFromStr(ColorChoice);

impl FromStr for ColorChoiceFromStr {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s {
            "always" => Ok(ColorChoiceFromStr(ColorChoice::Always)),
            "ansi" => Ok(ColorChoiceFromStr(ColorChoice::AlwaysAnsi)),
            "auto" => Ok(ColorChoiceFromStr(ColorChoice::Auto)),
            "never" => Ok(ColorChoiceFromStr(ColorChoice::Never)),
            _ => unreachable!("should be filtered by clap"),
        }
    }
}

fn main() -> Result<(), failure::Error> {
    let Opt {
        timeout,
        color_choice: ColorChoiceFromStr(color_choice),
    } = Opt::from_args();
    let timeout = timeout.map(Duration::from_secs);

    let username = load_from_envchain(ENVVAR_NAME_USERNAME)?;
    let password = load_from_envchain(ENVVAR_NAME_PASSWORD)?;

    let client = EchoingClient::new(timeout, "https://service.wi2.ne.jp", color_choice)?;
    client.get("/robots.txt").acceptable(&[404]).send()?;
    let res = client
        .get("/wi2net/Login/2/")
        .acceptable(&[200, 302])
        .send()?;
    if res.status() == StatusCode::Found {
        static MES: &str = "You have already logged in, or you are connecting to different SSID";
        die(MES, color_choice);
    }
    let token = extract_token(&res.try_into_document()?)?;
    let res = client
        .post("/wi2net/Login/1/?Wi2=1")
        .acceptable(&[200, 302])
        .form(&hashmap!("postKey" => token, "id" => username, "pass" => password))
        .send()?;
    if let Some(location) = res.headers().get::<Location>() {
        let correct_location = Regex::new(r"\A/wi2net/Top/2/?(\?SSID=[0-9a-f]+)?/?\z").unwrap();
        if correct_location.is_match(location) {
            println!("Successfully logged in.");
            return Ok(());
        }
    }
    static MES: &str = "Failed to login. Are username or password correct?";
    die(MES, color_choice);
}

fn load_from_envchain(envvar_name: &'static str) -> Result<String, failure::Error> {
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
        let (stderr, status) = (String::from_utf8_lossy(&output.stderr), output.status);
        bail!("{:?} ({})", stderr, status);
    }
}

fn extract_token(document: &Document) -> Result<String, failure::Error> {
    document
        .find(Name("input").and(Attr("name", "postKey")))
        .next()
        .and_then(|node| node.attr("value"))
        .map(ToOwned::to_owned)
        .ok_or_else(|| failure::err_msg("Not found: <input name=\"postKey\", value=...>"))
}

fn die(message: &'static str, color_choice: ColorChoice) -> ! {
    let mut stderr = StandardStream::stderr(color_choice);
    stderr
        .set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))
        .unwrap();
    writeln!(stderr, "{}", message).unwrap();
    stderr.flush().unwrap();
    process::exit(1)
}

trait TryIntoDocument {
    fn try_into_document(self) -> reqwest::Result<Document>;
}

impl TryIntoDocument for Response {
    fn try_into_document(mut self) -> reqwest::Result<Document> {
        let text = self.text()?;
        Ok(Document::from(text.as_str()))
    }
}

struct EchoingClient {
    inner: reqwest::Client,
    base: Url,
    color_choice: ColorChoice,
}

impl EchoingClient {
    fn new(
        timeout: Option<Duration>,
        base: impl IntoUrl,
        color_choice: ColorChoice,
    ) -> Result<Self, failure::Error> {
        let inner = reqwest::Client::builder()
            .redirect(RedirectPolicy::none())
            .timeout(timeout)
            .default_headers({
                let mut headers = Headers::new();
                headers.set(UserAgent::new(USER_AGENT));
                headers
            })
            .build()?;
        let base = base.into_url()?;
        Ok(Self {
            inner,
            base,
            color_choice,
        })
    }

    fn get(&self, url: &str) -> EchoingRequest {
        self.request(Method::Get, StatusCode::Ok, url)
    }

    fn post(&self, url: &str) -> EchoingRequest {
        self.request(Method::Post, StatusCode::Found, url)
    }

    fn request(&self, method: Method, acceptable: StatusCode, url: &str) -> EchoingRequest {
        let inner = Url::parse(url)
            .or_else(|err| match err {
                UrlError::RelativeUrlWithoutBase => self.base.join(url),
                err => Err(err),
            })
            .map(|url| self.inner.request(method, url));
        EchoingRequest {
            inner,
            client: self.inner.clone(),
            acceptable: vec![acceptable],
            out: StandardStream::stdout(self.color_choice),
        }
    }
}

struct EchoingRequest {
    inner: Result<reqwest::RequestBuilder, UrlError>,
    client: reqwest::Client,
    acceptable: Vec<StatusCode>,
    out: StandardStream,
}

impl EchoingRequest {
    fn form(mut self, form: &(impl Serialize + ?Sized)) -> Self {
        if let Ok(inner) = self.inner.as_mut() {
            inner.form(form);
        }
        self
    }

    fn acceptable(self, acceptable: &[u16]) -> Self {
        let acceptable = acceptable
            .iter()
            .map(|&n| StatusCode::try_from(n).unwrap_or_else(|_| StatusCode::Unregistered(n)))
            .collect();
        Self { acceptable, ..self }
    }

    fn send(mut self) -> Result<Response, failure::Error> {
        fn echo_method_and_url(req: &reqwest::Request, out: &mut StandardStream) {
            out.set_color(ColorSpec::new().set_bold(true)).unwrap();
            write!(out, "{} ", req.method()).unwrap();
            out.set_color(ColorSpec::new().set_fg(Some(Color::Cyan)))
                .unwrap();
            write!(out, "{}", req.url()).unwrap();
            out.reset().unwrap();
            write!(out, " ... ").unwrap();
            out.flush().unwrap();
        }

        fn echo_status(status: StatusCode, success: bool, out: &mut StandardStream) {
            let color = if success { Color::Green } else { Color::Red };
            out.set_color(ColorSpec::new().set_bold(true).set_fg(Some(color)))
                .unwrap();
            writeln!(out, "{}", status).unwrap();
            out.reset().unwrap();
            out.flush().unwrap();
        }

        let req = self.inner?.build()?;
        echo_method_and_url(&req, &mut self.out);
        let res = self.client.execute(req)?;
        let status = res.status();
        let success = self.acceptable.contains(&status);
        echo_status(status, success, &mut self.out);
        if !success {
            let acceptable = self.acceptable.iter().format(", ");
            bail!("Got {}, expected [{}]", status, acceptable);
        }
        Ok(res)
    }
}
