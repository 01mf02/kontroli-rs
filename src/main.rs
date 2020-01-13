extern crate circular;
extern crate lazy_st;
extern crate nom;

use fnv::FnvHashMap;

mod command;
mod parse;
mod parsebuffer;
mod reduce;
mod scope;
mod subst;
mod term;
mod typing;

use command::*;
use term::*;

use nom::error::VerboseError;

#[derive(Debug)]
enum CliError {
    Io(std::io::Error),
    Type(typing::Error),
}

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Self::Io(ref err) => err.fmt(f),
            Self::Type(ref err) => err.fmt(f),
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

fn run(filename: &str) -> Result<(), CliError> {
    use parsebuffer::ParseBuffer;
    let pb: ParseBuffer<_, _, _> = ParseBuffer {
        buf: circular::Buffer::with_capacity(64 * 1024 * 1024),
        read: std::fs::File::open(filename)?,
        parse: parse::parse_toplevel,
        fail: |e: nom::Err<VerboseError<&[u8]>>| format!("{:#?}", e),
    };

    let mut symbols: scope::SymTable = FnvHashMap::default();
    let sig: reduce::Signature = FnvHashMap::default();

    for entry in pb {
        let i = entry.expect("parse error");
        if let Some(Command::DCmd(id, args, dcmd)) = i {
            println!("{}", id);
            let dcmd = dcmd.parametrise(args).scope(&symbols, &mut Vec::new());
            match dcmd {
                DCommand::Declaration(tm) => match tm.infer(&sig, &mut Vec::new())? {
                    Term::Kind | Term::Type => Ok(()),
                    _ => Err(typing::Error::SortExpected),
                },
                _ => Ok(()),
            }?;
            if symbols.insert(id, ()).is_some() {
                panic!("symbol redeclaration");
            };
        }
    }
    Ok(())
}

fn main() {
    let mut args = std::env::args();
    let _ = args.next().expect("first arg is program path");
    let filename = args
        .next()
        .expect("please pass a file path as first argument");
    run(&filename).expect("should parse file correctly");
}
