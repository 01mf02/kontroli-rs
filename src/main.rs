//! A typechecker for the lambda-Pi calculus modulo rewriting.

extern crate circular;
extern crate lazy_st;
extern crate nom;
extern crate pretty_env_logger;
#[macro_use]
extern crate log;

mod command;
mod parse;
mod parsebuffer;
mod parseerror;
mod pattern;
mod precommand;
mod prepattern;
mod preterm;
mod reduce;
mod rule;
mod scope;
mod signature;
mod stack;
mod subst;
mod symbol;
mod term;
mod typing;

use crate::rule::Rule;
use crate::scope::Symbols;
use crate::signature::Signature;
use byte_unit::{Byte, ByteError};
use nom::error::VerboseError;
use std::convert::TryInto;
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
        "byte error".to_string()
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

impl command::Command {
    fn handle(self, sig: &mut Signature) -> Result<(), CliError> {
        match self {
            Self::DCmd(sym, dcmd) => {
                println!("{}", sym);
                let entry = signature::Entry::new(dcmd, &*sig)?;
                let info = signature::SymInfo::new(&sym, entry);
                if sig.insert(sym, info).is_some() {
                    panic!("symbol redeclaration");
                };
                Ok(())
            }
            Self::Rule(ctx, pat, rhs) => {
                let rule = Rule::new(ctx, pat, rhs)?;
                sig.get_mut(&rule.symbol)
                    .expect("rule")
                    .add_rule(rule)
                    .expect("static");
                Ok(())
            }
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
            cmd.scope(syms)?.handle(sig)?;
        }
    }
    Ok(())
}

fn main() -> Result<(), CliError> {
    pretty_env_logger::init();

    let mut sig: Signature = Default::default();
    let mut syms: Symbols = Default::default();

    let opt = Opt::from_args();

    if opt.files.len() == 0 {
        run(io::stdin(), &opt, &mut syms, &mut sig)?;
    } else {
        for filename in &opt.files {
            let file = std::fs::File::open(filename)?;
            run(file, &opt, &mut syms, &mut sig)?;
        }
    }
    Ok(())
}
