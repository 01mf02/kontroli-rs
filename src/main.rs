extern crate circular;
extern crate lazy_st;
extern crate nom;

use fnv::FnvHashMap;
use std::io::Read;

use nom::{Err, IResult, Offset};

mod command;
mod parse;
mod reduce;
mod scope;
mod subst;
mod term;
mod typing;

use command::*;
use term::*;

fn run(filename: &str) -> std::io::Result<()> {
    let mut file = std::fs::File::open(filename)?;

    // circular::Buffer is a ring buffer abstraction that separates reading and consuming data
    // it can grow its internal buffer and move data around if we reached the end of that buffer
    let mut b = circular::Buffer::with_capacity(64 * 1024 * 1024);
    let mut symbols: scope::SymTable = FnvHashMap::default();

    loop {
        let consumed = match parse::parse_toplevel(b.data()) {
            Err(Err::Incomplete(_)) => {
                // ensure that we have some space available in the buffer
                if b.available_space() == 0 {
                    if b.position() == 0 {
                        //println!("growing buffer capacity");

                        // double buffer capacity
                        b.grow(b.capacity() * 2);
                    } else {
                        b.shift();
                    }
                }

                // read from file to free space of buffer
                let read_bytes = file.read(b.space()).expect("should write");
                b.fill(read_bytes);
                //println!("Read {} bytes from file", read_bytes);

                if b.available_data() == 0 {
                    // no more data to read or parse, stopping the reading loop
                    break;
                } else if read_bytes == 0 {
                    panic!("incomplete parse at end of file");
                }

                // we did not consume anything
                0
            }

            Err(e) => {
                panic!("parse error: {:#?}", e);
            }

            Ok((remaining, toplevel)) => {
                if let Some(Command::DCmd(id, args, dcmd)) = toplevel {
                    println!("{}", id);
                    let dcmd = dcmd.parametrise(args);
                    let dcmd = scope::dcommand(&symbols, &mut Vec::new(), dcmd);
                    if symbols.insert(id, ()).is_some() {
                        panic!("symbol redeclaration");
                    };
                }

                b.data().offset(remaining)
            }
        };
        b.consume(consumed);
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
