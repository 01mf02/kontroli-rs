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

fn run(filename: &str) -> std::io::Result<()> {
    use parsebuffer::ParseBuffer;
    let mut pb: ParseBuffer<_, _, _> = ParseBuffer {
        buf: circular::Buffer::with_capacity(64 * 1024 * 1024),
        read: std::fs::File::open(filename)?,
        parse: parse::parse_toplevel,
        fail: |e: nom::Err<VerboseError<&[u8]>>| format!("{:#?}", e),
    };

    let mut symbols: scope::SymTable = FnvHashMap::default();

    for entry in pb {
        let i = entry.expect("parse error");
        if let Some(Command::DCmd(id, args, dcmd)) = i {
            println!("{}", id);
            let dcmd = dcmd.parametrise(args).scope(&symbols, &mut Vec::new());
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
