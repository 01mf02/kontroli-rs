//! Parallel event processing.

use crate::{parse, Error, Event, Opt, PathRead, Stage};
use colosseum::sync::Arena;
use core::{borrow::Borrow, convert::TryFrom};
use kontroli::arc::{Intro, Rule, Signature, Typing};
use kontroli::error::Error as KoError;
use kontroli::{Share, Symbol, Symbols};
use rayon::iter::{ParallelBridge, ParallelIterator};

type Command<'s> = kontroli::Command<Symbol<'s>, Intro<'s>, Rule<'s>>;

type Check<'s> = (Typing<'s>, Signature<'s>);

fn from_event<'s>(
    event: Event,
    syms: &mut Symbols<'s>,
    arena: &'s Arena<String>,
) -> Result<Option<Command<'s>>, KoError> {
    event
        .handle(syms)
        .map(|cmd| share(cmd, syms, arena))
        .transpose()
}

fn share<'s, S: Borrow<str> + Ord>(
    cmd: kontroli::scope::Command<S>,
    syms: &mut Symbols<'s>,
    arena: &'s Arena<String>,
) -> Result<Command<'s>, KoError> {
    match cmd {
        kontroli::Command::Intro(id, it) => {
            let it = it.share(syms)?;
            let id = syms.insert(arena.alloc(id))?;
            Ok(Command::Intro(id, it))
        }
        kontroli::Command::Rules(rules) => {
            let rules = rules.into_iter().map(|r| r.share(syms));
            Ok(Command::Rules(rules.collect::<Result<_, _>>()?))
        }
    }
}

fn infer<'s>(cmd: Command<'s>, sig: &mut Signature<'s>) -> Result<Option<Check<'s>>, KoError> {
    match cmd {
        kontroli::Command::Intro(sym, it) => {
            // defer checking to later
            let typing = Typing::new(it, &sig)?;
            sig.insert(sym, typing.clone())?;
            Ok(Some((typing, sig.clone())))
        }
        kontroli::Command::Rules(rules) => {
            sig.add_rules(rules.into_iter())?;
            Ok(None)
        }
    }
}

fn check((typing, sig): Check) -> Result<(), KoError> {
    let _ = typing.check(&sig)?;
    Ok(())
}

fn infer_checks<'s, I>(iter: I, checks: bool, sig: &mut Signature<'s>) -> Result<(), Error>
where
    I: Iterator<Item = Result<Command<'s>, Error>> + Send,
{
    iter.map(|cmd| infer(cmd?, sig).map_err(Error::Ko))
        .map(|ro| ro.transpose())
        .flatten()
        .filter(|cmd| checks || cmd.is_err())
        .par_bridge()
        .try_for_each(|ts| check(ts?).map_err(Error::Ko))
}

pub fn run(opt: &Opt) -> Result<(), Error> {
    let arena: Arena<String> = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut sig: Signature = Signature::new();

    sig.eta = opt.eta;

    for file in opt.files.iter() {
        let file = PathRead::try_from(file)?;
        syms.set_path(file.path);

        use Stage::{Check, Infer, Share};

        let cmds = kontroli::parse::lexes(&file.read)
            .map(|tokens| parse::<&str>(tokens?, &opt))
            .map(|res| res.transpose())
            .flatten()
            .filter(|cmd| !opt.omits(Share) || cmd.is_err())
            .map(|cmd| share(cmd?, &mut syms, &arena).map_err(Error::Ko))
            .filter(|cmd| !opt.omits(Infer) || cmd.is_err());

        infer_checks(cmds, !opt.omits(Check), &mut sig)?
    }
    Ok(())
}

pub fn consume<I>(iter: I, opt: &Opt) -> Result<(), Error>
where
    I: Iterator<Item = Result<Event, Error>> + Send,
{
    let arena: Arena<String> = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut sig: Signature = Signature::new();

    sig.eta = opt.eta;

    use Stage::{Check, Infer, Share};

    // run as long as we receive events, and abort on error
    let cmds = iter
        .filter(|event| !opt.omits(Share) || event.is_err())
        .map(|event| from_event(event?, &mut syms, &arena).map_err(Error::Ko))
        .map(|ro| ro.transpose())
        .flatten()
        .filter(|cmd| !opt.omits(Infer) || cmd.is_err());

    infer_checks(cmds, !opt.omits(Check), &mut sig)
}
