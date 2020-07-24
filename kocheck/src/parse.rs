use crate::{Error, Opt, ParseBuffer};
use kontroli::error::Error as KoError;
use kontroli::parse::Command;
use nom::error::VerboseError;
use std::convert::TryInto;
use std::io::Read;

/// Produce a stream of commands from given input.
pub fn parse<R: Read>(read: R, opt: &Opt) -> impl Iterator<Item = Result<Command, Error>> {
    use kontroli::parse::{opt_lex, phrase, Parse, Parser};
    let parse: fn(&[u8]) -> Parse<_> = |i| opt_lex(phrase(Command::parse))(i);
    let mut pb = ParseBuffer {
        buf: circular::Buffer::with_capacity(opt.buffer.get_bytes().try_into().unwrap()),
        read,
        parse,
        fail: |_: nom::Err<VerboseError<&[u8]>>| Error::Ko(KoError::Parse),
    };
    pb.fill().unwrap();
    // consider only non-whitespace entries
    pb.map(|entry| entry.transpose()).flatten()
}
