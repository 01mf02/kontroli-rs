//! A typechecker for the lambda-Pi calculus modulo rewriting.

extern crate pretty_env_logger;

use byte_unit::{Byte, ByteError};
use kontroli::command::Command;
use kontroli::parsebuffer::ParseBuffer;
use kontroli::precommand::Precommand;
use kontroli::{parse, signature};
use kontroli::{Error, Rule, Signature, Symbols};
use nom::error::VerboseError;
use std::convert::{TryFrom, TryInto};
use std::io;
use std::path::PathBuf;
use std::sync::mpsc::{channel, Receiver};
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
        Command::Rule(unchecked) => Ok(sig.add_rule(Rule::try_from(unchecked)?)?),
    }
}

fn produce<R>(read: R, opt: &Opt) -> impl Iterator<Item = Result<Precommand, Error>>
where
    R: io::Read,
{
    ParseBuffer {
        buf: circular::Buffer::with_capacity(opt.buffer.get_bytes().try_into().unwrap()),
        read,
        parse: parse::parse_toplevel,
        fail: |e: nom::Err<VerboseError<&[u8]>>| Error::Parse(format!("{:#?}", e)),
    }
    // consider only the non-whitespace entries
    .map(|entry| entry.transpose())
    .flatten()
}

fn consume(opt: &Opt, receiver: &Receiver<Result<Precommand, Error>>) -> Result<(), Error> {
    let mut sig: Signature = Default::default();
    let mut syms: Symbols = Default::default();

    sig.eta = opt.eta;

    // run as long as we receive items from the sender
    receiver.iter().try_for_each(|cmd| {
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

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    let opt = Opt::from_args();

    let (sender, receiver) = channel();
    let send = |cmd| sender.send(cmd).unwrap();

    let optr = opt.clone();
    let consumer = thread::spawn(move || consume(&optr, &receiver));

    if opt.files.is_empty() {
        produce(io::stdin(), &opt).for_each(send);
    } else {
        for filename in &opt.files {
            let file = std::fs::File::open(filename)?;
            produce(file, &opt).for_each(send);
        }
    };

    // signalise that we are done sending commands
    // (otherwise the consumer will eventually wait forever)
    drop(sender);

    // wait for all commands to be consumed
    consumer.join().unwrap()?;

    Ok(())
}
