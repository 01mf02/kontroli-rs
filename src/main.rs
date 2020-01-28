extern crate circular;
extern crate lazy_st;
extern crate nom;

use fnv::FnvHashMap;

mod command;
mod parse;
mod parsebuffer;
mod reduce;
mod rule;
mod scope;
mod signature;
mod subst;
mod term;
mod typing;

use command::*;
use rule::*;
use signature::Signature;
use term::*;

use nom::error::VerboseError;
use std::convert::TryFrom;

#[derive(Debug)]
enum CliError {
    Io(std::io::Error),
    Type(typing::Error),
    Scope(scope::Error),
}

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Self::Io(ref err) => err.fmt(f),
            Self::Type(ref err) => err.fmt(f),
            Self::Scope(ref err) => err.fmt(f),
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

fn run(filename: &str) -> Result<(), CliError> {
    use parsebuffer::ParseBuffer;
    let pb: ParseBuffer<_, _, _> = ParseBuffer {
        buf: circular::Buffer::with_capacity(64 * 1024 * 1024),
        read: std::fs::File::open(filename)?,
        parse: parse::parse_toplevel,
        fail: |e: nom::Err<VerboseError<&[u8]>>| format!("{:#?}", e),
    };

    let mut sig: Signature = Signature::new();

    for entry in pb {
        let i = entry.expect("parse error");
        if let Some(Command::DCmd(id, args, dcmd)) = i {
            println!("{}", id);
            let dcmd = dcmd.parametrise(args).scope(&sig, &mut Vec::new())?;
            let entry = signature::Entry::try_from((dcmd, &sig))?;
            if sig.insert(id, entry).is_some() {
                panic!("symbol redeclaration");
            };
        }
    }
    Ok(())
}

fn main() -> Result<(), CliError> {
    let mut args = std::env::args();
    let _ = args.next().expect("first arg is program path");
    let filename = args
        .next()
        .expect("please pass a file path as first argument");
    run(&filename)?;
    Ok(())
}
