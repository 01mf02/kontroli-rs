extern crate circular;
extern crate lazy_st;
extern crate nom;
extern crate pretty_env_logger;
#[macro_use]
extern crate log;

mod command;
mod parse;
mod parsebuffer;
mod pattern;
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
use nom::error::VerboseError;
use std::fmt;

#[derive(Debug)]
enum CliError {
    Io(std::io::Error),
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

impl From<std::io::Error> for CliError {
    fn from(err: std::io::Error) -> Self {
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

fn run(filename: &str) -> Result<(), CliError> {
    use parsebuffer::ParseBuffer;
    let pb: ParseBuffer<_, _, _> = ParseBuffer {
        buf: circular::Buffer::with_capacity(64 * 1024 * 1024),
        read: std::fs::File::open(filename)?,
        parse: parse::parse_toplevel,
        fail: |e: nom::Err<VerboseError<&[u8]>>| format!("{:#?}", e),
    };

    let mut sig: Signature = Default::default();
    let mut syms: Symbols = Default::default();

    for entry in pb {
        let i = entry.expect("parse error");
        if let Some(cmd) = i {
            cmd.scope(&mut syms)?.handle(&mut sig)?;
        }
    }
    Ok(())
}

fn main() -> Result<(), CliError> {
    pretty_env_logger::init();

    let mut args = std::env::args();
    let _ = args.next().expect("first arg is program path");
    let filename = args
        .next()
        .expect("please pass a file path as first argument");
    run(&filename)?;
    Ok(())
}
