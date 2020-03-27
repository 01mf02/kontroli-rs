//! A typechecker for the lambda-Pi calculus modulo rewriting.

extern crate circular;
extern crate pretty_env_logger;

mod parsebuffer;
mod parseerror;

use byte_unit::{Byte, ByteError};
use crossbeam_channel::{bounded, unbounded};
use kontroli::command::Command;
use kontroli::precommand::Precommand;
use kontroli::{parse, signature};
use kontroli::{Error, Signature, Symbols};
use nom::error::VerboseError;
use std::convert::TryInto;
use std::io::Read;
use std::path::PathBuf;
use std::thread;
use structopt::StructOpt;

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

#[derive(Clone, Debug, StructOpt)]
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

    /// Parse given number of commands in advance (∞ if argument omitted)
    #[structopt(long, short = "j")]
    jobs: Option<Option<usize>>,

    /// Files to process (cumulative)
    #[structopt(name = "FILE")]
    files: Vec<PathBuf>,
}

fn handle(cmd: Command, sig: &mut Signature) -> Result<(), Error> {
    match cmd {
        Command::DCmd(sym, dcmd) => {
            println!("{}", sym);
            Ok(sig.insert(&sym, signature::Entry::new(dcmd, &*sig)?)?)
        }
        Command::Rule(rule) => Ok(sig.add_rule(rule)?),
    }
}

type Item = Result<Precommand, Error>;

fn produce<R: Read>(read: R, opt: &Opt) -> impl Iterator<Item = Item> {
    use parse::{opt_lexeme, phrase, Parse, Parser};
    let parse: fn(&[u8]) -> Parse<_> = |i| opt_lexeme(phrase(Precommand::parse))(i);
    parsebuffer::ParseBuffer {
        buf: circular::Buffer::with_capacity(opt.buffer.get_bytes().try_into().unwrap()),
        read,
        parse,
        fail: |e: nom::Err<VerboseError<&[u8]>>| Error::Parse(format!("{:#?}", e)),
    }
    // consider only the non-whitespace entries
    .map(|entry| entry.transpose())
    .flatten()
}

fn consume(opt: &Opt, mut iter: impl Iterator<Item = Item>) -> Result<(), Error> {
    let mut sig: Signature = Default::default();
    let mut syms: Symbols = Default::default();

    sig.eta = opt.eta;

    // run as long as we receive items
    iter.try_for_each(|cmd| {
        // abort if there was a parse error
        let cmd = cmd?;

        if opt.no_scope {
            return Ok(());
        }
        let cmd = cmd.scope(&mut syms)?;

        if opt.no_check {
            return Ok(());
        }
        handle(cmd, &mut sig)?;

        Ok(())
    })
}

/// Return stdin if no files given, else lazily open and return the files.
fn reads<'a>(files: &'a [PathBuf]) -> Box<dyn Iterator<Item = Result<Box<dyn Read>, Error>> + 'a> {
    if files.is_empty() {
        let read: Box<dyn Read> = Box::new(std::io::stdin());
        Box::new(std::iter::once(Ok(read)))
    } else {
        Box::new(files.iter().map(|file| {
            let read: Box<dyn Read> = Box::new(std::fs::File::open(file)?);
            Ok(read)
        }))
    }
}

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    let opt = Opt::from_args();

    // lazily produce precommands from all specified files
    let items = reads(&opt.files)
        .flat_map(|read| Ok::<_, Error>(produce(read?, &opt)))
        .flatten();

    match opt.jobs {
        Some(channel) => {
            let (sender, receiver) = match channel {
                Some(capacity) => bounded(capacity),
                None => unbounded(),
            };

            let optr = opt.clone();
            let consumer = thread::spawn(move || consume(&optr, receiver.iter()));

            items.for_each(|cmd| sender.send(cmd).unwrap());

            // signalise that we are done sending precommands
            // (otherwise the consumer will eventually wait forever)
            drop(sender);

            // wait for all commands to be consumed
            consumer.join().unwrap()?;
        }
        None => consume(&opt, items)?,
    }

    Ok(())
}
