//! A typechecker for the lambda-Pi calculus modulo rewriting.

extern crate circular;
extern crate pretty_env_logger;

mod parsebuffer;
mod parseerror;

use byte_unit::{Byte, ByteError};
use crossbeam_channel::{bounded, unbounded};
use kontroli::error::Error as KoError;
use kontroli::parse::Command;
use kontroli::scope::{self, Symbols};
use nom::error::VerboseError;
use std::convert::TryInto;
use std::io::{self, Read};
use std::path::{self, Path, PathBuf};
use structopt::StructOpt;

#[derive(Debug)]
pub enum Error {
    Module,
    Io(io::Error),
    Ko(KoError),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<KoError> for Error {
    fn from(err: KoError) -> Self {
        Self::Ko(err)
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

#[derive(Clone, Debug, StructOpt)]
/// A typechecker for the lambda-Pi calculus modulo rewriting
struct Opt {
    /// Reduce terms modulo eta
    ///
    /// When this flag is enabled, checking whether
    /// `\ x => t` and `u` are convertible will succeed if
    /// `\ x => t` and `\ y => u y` are convertible.
    #[structopt(long)]
    eta: bool,

    /// Only parse, neither scope nor typecheck
    #[structopt(long)]
    no_scope: bool,

    /// Only parse and scope, do not typecheck
    #[structopt(long)]
    no_check: bool,

    /// Size of the parse buffer
    ///
    /// The parser repeatedly reads data into a buffer and parses a command.
    /// If a command does not fit into the buffer,
    /// the buffer size is doubled and parsing is retried,
    /// until either parsing succeeds or the whole file is read.
    ///
    /// Therefore, the buffer size should be chosen to be
    /// larger than the size of the largest expected command,
    /// to avoid unnecessary retries.
    #[structopt(long, default_value = "64MB", parse(try_from_str = parse_byte))]
    buffer: Byte,

    /// Parse given number of commands in advance (∞ if argument omitted)
    ///
    /// If this option is used, commands are parsed and checked simultaneously.
    /// If this option is given with a number n, then
    /// maximally n commands are parsed in advance.
    /// If this option is given without an extra argument, then
    /// the number of commands parsed in advance is unbounded.
    ///
    /// Note that unbounded parsing can lead to high memory usage!
    #[structopt(long, short = "c")]
    channel_capacity: Option<Option<usize>>,

    /// Typecheck concurrently
    ///
    /// If this option is used, type checking tasks are executed in parallel.
    /// If this option is given with a number n, then
    /// maximally n type checking tasks are concurrently executed.
    /// If this option is given without an extra argument, then
    /// the number of concurrently executed tasks is
    /// determined automatically from the number of CPUs.
    ///
    /// This option enables the parsing of commands in advance ("-c"),
    /// by default with an unbounded capacity.
    #[structopt(long, short = "j")]
    jobs: Option<Option<usize>>,

    /// Files to process (cumulative)
    ///
    /// Checking multiple files is equivalent to checking their concatenation.
    #[structopt(name = "FILE")]
    files: Vec<PathBuf>,
}

enum Event {
    Command(Command),
    Module(Vec<String>),
}

type RCommand = Result<Command, Error>;
type REvent = Result<Event, Error>;

fn produce<R: Read>(read: R, opt: &Opt) -> impl Iterator<Item = RCommand> {
    use kontroli::parse::{opt_lex, phrase, Parse, Parser};
    let parse: fn(&[u8]) -> Parse<_> = |i| opt_lex(phrase(Command::parse))(i);
    parsebuffer::ParseBuffer {
        buf: circular::Buffer::with_capacity(opt.buffer.get_bytes().try_into().unwrap()),
        read,
        parse,
        fail: |_: nom::Err<VerboseError<&[u8]>>| Error::Ko(KoError::Parse),
    }
    // consider only the non-whitespace entries
    .map(|entry| entry.transpose())
    .flatten()
}

fn consume_seq(opt: &Opt, mut iter: impl Iterator<Item = REvent>) -> Result<(), Error> {
    use colosseum::unsync::Arena;
    use kontroli::rc::{Intro, Rule, Signature, Typing};

    let arena = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut sig: Signature = Signature::new();

    sig.eta = opt.eta;

    let mut handle = |event: Event| -> Result<(), KoError> {
        match event {
            Event::Module(path) => Ok(syms.set_path(path)),
            Event::Command(cmd) => {
                if opt.no_scope {
                    return Ok(());
                }

                let cmd: Result<_, KoError> = cmd.scope(&syms)?.map_id_err(|id| {
                    println!("{}", id);
                    let id: &str = arena.alloc(id);
                    Ok(syms.insert(id)?)
                });

                if opt.no_check {
                    return Ok(());
                }

                match cmd? {
                    scope::Command::Intro(sym, it) => {
                        let typing = Typing::new(Intro::from(it), &sig)?.check(&sig)?;
                        Ok(sig.insert(sym, typing)?)
                    }
                    scope::Command::Rule(rule) => Ok(sig.add_rule(Rule::from(rule))?),
                }
            }
        }
    };

    // run as long as we receive items, and abort if there was a parse error
    iter.try_for_each(|cmd| handle(cmd?).map_err(Error::Ko))
}

fn consume_par(opt: &Opt, iter: impl Iterator<Item = REvent> + Send) -> Result<(), Error> {
    use colosseum::sync::Arena;
    use kontroli::arc::{Intro, Rule, Signature, Typing};
    use rayon::iter::{ParallelBridge, ParallelIterator};

    // this is required to constrain the lifetimes to be equal
    type Check<'s> = (Typing<'s>, Signature<'s>);

    let arena = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut sig: Signature = Signature::new();

    sig.eta = opt.eta;

    let mut handle = |event: Event| -> Result<Option<Check>, KoError> {
        match event {
            Event::Module(path) => {
                syms.set_path(path);
                Ok(None)
            }
            Event::Command(cmd) => {
                if opt.no_scope {
                    return Ok(None);
                }

                let cmd: Result<_, KoError> = cmd.scope(&syms)?.map_id_err(|id| {
                    println!("{}", id);
                    let id: &str = arena.alloc(id);
                    Ok(syms.insert(id)?)
                });

                if opt.no_check {
                    return Ok(None);
                }

                match cmd? {
                    scope::Command::Intro(sym, it) => {
                        // defer checking to later
                        let typing = Typing::new(Intro::from(it), &sig)?;
                        sig.insert(sym, typing.clone())?;
                        Ok(Some((typing, sig.clone())))
                    }
                    scope::Command::Rule(rule) => {
                        sig.add_rule(Rule::from(rule))?;
                        Ok(None)
                    }
                }
            }
        }
    };

    let check = |(typing, sig): Check| -> Result<(), KoError> {
        let _ = typing.check(&sig)?;
        Ok(())
    };

    // run as long as we receive items, and abort if there was a parse error
    iter.map(|cmd| handle(cmd?).map_err(Error::Ko))
        .map(|es| es.transpose())
        .flatten()
        .par_bridge()
        .try_for_each(|ts| check(ts?).map_err(Error::Ko))
}

type ModuleRead = (Vec<String>, Box<dyn Read>);

/// Return stdin if no files given, else lazily open and return the files.
fn reads<'a>(files: &'a [PathBuf]) -> Box<dyn Iterator<Item = Result<ModuleRead, Error>> + 'a> {
    if files.is_empty() {
        let read: Box<dyn Read> = Box::new(io::stdin());
        Box::new(std::iter::once(Ok((Vec::new(), read))))
    } else {
        Box::new(files.iter().map(|file| {
            let module = module_path(file).ok_or(Error::Module)?;
            let read: Box<dyn Read> = Box::new(std::fs::File::open(file)?);
            Ok((module, read))
        }))
    }
}

/// Return the module path corresponding to a file path.
fn module_path(path: &Path) -> Option<Vec<String>> {
    let components: Vec<_> = path
        .parent()
        .map(|p| p.components().collect())
        .unwrap_or_default();
    let mpath: Option<Vec<_>> = components
        .into_iter()
        .map(|component| match component {
            path::Component::Normal(name) => Some(name),
            _ => None,
        })
        .collect();
    let mut mpath = mpath?;
    mpath.push(path.file_stem()?);
    mpath
        .iter()
        .map(|s| Some(String::from(s.to_str()?)))
        .collect()
}

fn produce_events((path, read): ModuleRead, opt: &Opt) -> impl Iterator<Item = REvent> {
    let path = std::iter::once(Ok(Event::Module(path)));
    let cmds = produce(read, &opt).map(|cmd| cmd.map(Event::Command));
    path.chain(cmds)
}

/// Flatten an iterator of results of iterators of results into an iterator of results.
///
/// Source: <https://www.reddit.com/r/rust/comments/9u6846/rust_puzzle_flatten_a_nested_iterator_of_results>
pub fn flatten_nested_results<O, I, T, E>(outer: O) -> impl Iterator<Item = Result<T, E>>
where
    O: Iterator<Item = Result<I, E>>,
    I: Iterator<Item = Result<T, E>>,
{
    outer.flat_map(|inner_result| {
        let (v, r) = match inner_result {
            Ok(v) => (Some(v), None),
            Err(e) => (None, Some(Err(e))),
        };
        v.into_iter().flatten().chain(r)
    })
}

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    let opt = Opt::from_args();

    // if a precise number of parallel jobs has been given
    if let Some(Some(jobs)) = opt.jobs {
        rayon::ThreadPoolBuilder::new()
            .num_threads(jobs)
            .build_global()
            .unwrap();
    }

    // lazily produce precommands from all specified files
    let items = reads(&opt.files).map(|mr| Ok(produce_events(mr?, &opt)));
    let mut items = flatten_nested_results(items);

    let parallel = opt.jobs.is_some();

    // if parallel execution is enabled, assume an unbounded channel by default
    let channel = if parallel {
        Some(opt.channel_capacity.unwrap_or(None))
    } else {
        opt.channel_capacity
    };

    match channel {
        Some(capacity) => {
            let (sender, receiver) = match capacity {
                Some(capacity) => bounded(capacity),
                None => unbounded(),
            };

            let optr = opt.clone();
            let consumer = std::thread::spawn(move || {
                if parallel {
                    consume_par(&optr, receiver.iter())
                } else {
                    consume_seq(&optr, receiver.iter())
                }
            });

            // sending fails prematurely if consumption fails
            // in that case, get the error below
            let _ = items.try_for_each(|cmd| sender.send(cmd));

            // signalise that we are done sending precommands
            // (otherwise the consumer will eventually wait forever)
            drop(sender);

            // wait for all commands to be consumed
            consumer.join().unwrap()?;
        }
        None => consume_seq(&opt, items)?,
    }
    Ok(())
}
