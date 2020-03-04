//! A typechecker for the lambda-Pi calculus modulo rewriting.

extern crate pretty_env_logger;

use byte_unit::{Byte, ByteError};
use kontroli::command::Command;
use kontroli::rule::Rule;
use kontroli::scope::Symbols;
use kontroli::signature::Signature;
use kontroli::{parse, parsebuffer, rule, scope, signature, typing};
use nom::error::VerboseError;
use std::convert::{TryFrom, TryInto};
use std::path::PathBuf;
use std::{fmt, io};
use structopt::StructOpt;

#[derive(Debug)]
enum CliError {
    Io(io::Error),
    Type(typing::Error),
    Scope(scope::Error),
    Rule(rule::Error),
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Io(ref err) => err.fmt(f),
            Self::Type(ref err) => err.fmt(f),
            Self::Scope(ref err) => err.fmt(f),
            Self::Rule(ref err) => err.fmt(f),
        }
    }
}

impl std::error::Error for CliError {}

impl From<io::Error> for CliError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<typing::Error> for CliError {
    fn from(err: typing::Error) -> Self {
        Self::Type(err)
    }
}

impl From<scope::Error> for CliError {
    fn from(err: scope::Error) -> Self {
        Self::Scope(err)
    }
}

impl From<rule::Error> for CliError {
    fn from(err: rule::Error) -> Self {
        Self::Rule(err)
    }
}

#[derive(Debug)]
struct MyByteError(ByteError);

impl ToString for MyByteError {
    fn to_string(&self) -> String {
        match &self.0 {
            ByteError::ValueIncorrect(s) => "Incorrect byte value: ".to_owned() + &s.clone(),
            ByteError::UnitIncorrect(s) => "Incorrect byte unit: ".to_owned() + &s.clone(),
        }
    }
}

fn parse_byte<S: AsRef<str>>(s: S) -> Result<Byte, MyByteError> {
    Byte::from_str(s).map_err(MyByteError)
}

#[derive(Debug, StructOpt)]
/// A typechecker for the lambda-Pi calculus modulo rewriting
struct Opt {
    /// Reduce terms modulo eta
    #[structopt(long)]
    eta: bool,

    /// Only parse, neither scope nor typecheck
    #[structopt(long)]
    no_scope: bool,

    /// Only parse and scope, do not typecheck
    #[structopt(long)]
    no_check: bool,

    /// Size of the parse buffer
    #[structopt(long, default_value = "64MB", parse(try_from_str = parse_byte))]
    buffer: Byte,
    //#[structopt(long, default_value = "1024")]
    //buffer: usize,
    /// Files to process (cumulative)
    #[structopt(name = "FILE")]
    files: Vec<PathBuf>,
}

fn handle(cmd: Command, sig: &mut Signature) -> Result<(), CliError> {
    match cmd {
        Command::DCmd(sym, dcmd) => {
            println!("{}", sym);
            let entry = signature::Entry::new(dcmd, &*sig)?;
            let info = signature::SymInfo::new(&sym, entry);
            if sig.insert(sym, info).is_some() {
                panic!("symbol redeclaration");
            };
            Ok(())
        }
        Command::Rule(unchecked) => {
            let rule: Rule = Rule::try_from(unchecked)?;
            sig.get_mut(&rule.lhs.symbol)
                .expect("rule")
                .add_rule(rule)
                .expect("static");
            Ok(())
        }
    }
}

fn run<R>(read: R, opt: &Opt, syms: &mut Symbols, sig: &mut Signature) -> Result<(), CliError>
where
    R: io::Read,
{
    use parsebuffer::ParseBuffer;
    let pb: ParseBuffer<_, _, _> = ParseBuffer {
        buf: circular::Buffer::with_capacity(opt.buffer.get_bytes().try_into().unwrap()),
        read,
        parse: parse::parse_toplevel,
        fail: |e: nom::Err<VerboseError<&[u8]>>| format!("{:#?}", e),
    };

    for entry in pb {
        let i = entry.expect("parse error");
        if let Some(cmd) = i {
            if opt.no_scope {
                continue;
            }
            let cmd = cmd.scope(syms)?;
            if opt.no_check {
                continue;
            }
            handle(cmd, sig)?;
        }
    }
    Ok(())
}

fn main() -> Result<(), CliError> {
    pretty_env_logger::init();

    let mut sig: Signature = Default::default();
    let mut syms: Symbols = Default::default();

    let opt = Opt::from_args();

    if opt.files.is_empty() {
        run(io::stdin(), &opt, &mut syms, &mut sig)?;
    } else {
        for filename in &opt.files {
            let file = std::fs::File::open(filename)?;
            run(file, &opt, &mut syms, &mut sig)?;
        }
    }
    Ok(())
}
