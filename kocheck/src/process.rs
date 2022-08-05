//! Processing of events & files.

use crate::{infer_checks, Command, Error, Event, Opt, PathRead, Stage};
use colosseum::sync::Arena;
use core::borrow::Borrow;
use kontroli::error::Error as KoError;
use kontroli::parse::Scoped;
use kontroli::{symbol, GCtx, Share, Symbols};

fn from_event<'s>(
    event: Event,
    syms: &mut Symbols<'s>,
    arena: &'s Arena<symbol::Owned>,
) -> Result<Option<Command<'s>>, KoError> {
    event
        .handle(syms)
        .map(|cmd| share(cmd, syms, arena))
        .transpose()
}

fn share<'s, S: Borrow<str> + Ord>(
    cmd: Scoped<S>,
    syms: &mut Symbols<'s>,
    arena: &'s Arena<symbol::Owned>,
) -> Result<Command<'s>, KoError> {
    match cmd.share(syms)? {
        kontroli::Command::Intro(id, it) => {
            let owned = symbol::Owned::new(id.clone());
            let id = syms.insert(id, arena.alloc(owned))?;
            Ok(Command::Intro(id, it))
        }
        kontroli::Command::Rules(rules) => Ok(Command::Rules(rules)),
    }
}

fn log_cmd<S: core::fmt::Display>(cmd: &Scoped<S>) {
    match cmd {
        Scoped::Intro(id, _, _) => log::info!("Introduce symbol {}", id),
        Scoped::Rules(rules) => log::info!("Add {} rules", rules.len()),
    }
}

/// Parse a sequence of commands from a reader.
fn parse(r: impl std::io::Read) -> impl Iterator<Item = Result<Scoped<String>, Error>> {
    use std::io::{BufRead, BufReader};
    let lines = BufReader::new(r).lines().map(|line| line.unwrap());
    kontroli::parse::Lazy::new(lines)
        .inspect(|cmd| cmd.iter().for_each(log_cmd))
        .map(|cmd| cmd.map_err(Error::Parse))
}

/// Process all given input files.
///
/// This should be functionally equivalent to
/// [consuming](consume) all events [produced](produce) from the input files.
pub fn run(opt: &Opt) -> Result<(), Error> {
    let arena: Arena<symbol::Owned> = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut gc = GCtx::new();

    gc.eta = opt.eta;

    for file in &opt.files {
        let file = PathRead::try_from(file)?;
        syms.set_path(file.path);

        let cmds = parse(file.read)
            .filter(|cmd| !opt.omits(Stage::Share) || cmd.is_err())
            .map(|cmd| share(cmd?, &mut syms, &arena).map_err(Error::Ko));

        infer_checks(cmds, opt, &mut gc)?
    }
    Ok(())
}

/// Produce a stream of events from a sequence of input files.
pub fn produce<F, E>(files: &[std::path::PathBuf], mut send: F) -> Result<(), Error>
where
    F: FnMut(Result<Event, Error>) -> Result<(), E>,
{
    for file in files {
        let file = PathRead::try_from(file)?;

        let head = core::iter::once(Ok(Event::Module(file.path)));
        let tail = parse(file.read).map(|cmd| cmd.map(Event::Command));

        // sending fails prematurely if consumption fails
        // in that case, handle the error after this function exits
        if head.chain(tail).try_for_each(&mut send).is_err() {
            return Ok(());
        }
    }
    Ok(())
}

/// Consume a stream of events by processing them one by one.
pub fn consume<I>(iter: I, opt: &Opt) -> Result<(), Error>
where
    I: Iterator<Item = Result<Event, Error>> + Send,
{
    let arena: Arena<symbol::Owned> = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut gc = GCtx::new();

    gc.eta = opt.eta;

    // run as long as we receive events, and abort on error
    let cmds = iter
        .filter(|event| !opt.omits(Stage::Share) || event.is_err())
        .map(|event| from_event(event?, &mut syms, &arena).map_err(Error::Ko))
        .flat_map(|ro| ro.transpose());

    infer_checks(cmds, opt, &mut gc)
}
