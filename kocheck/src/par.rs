//! Parallel event processing.

use crate::{Error, Event, Opt};
use colosseum::sync::Arena;
use kontroli::arc::{Intro, Rule, Signature, Typing};
use kontroli::error::Error as KoError;
use kontroli::parse;
use kontroli::scope::{self, Symbols};
use rayon::iter::{ParallelBridge, ParallelIterator};

struct Command<'s>(scope::Command<'s, scope::Symbol<'s>>);

type Check<'s> = (Typing<'s>, Signature<'s>);

impl<'s> Command<'s> {
    fn from_precommand(
        cmd: parse::Command,
        syms: &mut Symbols<'s>,
        arena: &'s Arena<String>,
    ) -> Result<Self, KoError> {
        match cmd.scope(&syms)? {
            scope::Command::Intro(id, it) => {
                Ok(scope::Command::Intro(syms.insert(arena.alloc(id))?, it))
            }
            scope::Command::Rules(rules) => Ok(scope::Command::Rules(rules)),
        }
        .map(Self)
    }

    fn from_event(
        event: Event,
        syms: &mut Symbols<'s>,
        arena: &'s Arena<String>,
    ) -> Result<Option<Command<'s>>, KoError> {
        event
            .handle(syms)
            .map(|precmd| Self::from_precommand(precmd, syms, arena))
            .transpose()
    }

    fn infer(self, sig: &mut Signature<'s>) -> Result<Option<Check<'s>>, KoError> {
        match self.0 {
            scope::Command::Intro(sym, it) => {
                // defer checking to later
                let typing = Typing::new(Intro::from(it), &sig)?;
                sig.insert(sym, typing.clone())?;
                Ok(Some((typing, sig.clone())))
            }
            scope::Command::Rules(rules) => {
                sig.add_rules(rules.into_iter().map(Rule::from))?;
                Ok(None)
            }
        }
    }
}

fn check((typing, sig): Check) -> Result<(), KoError> {
    let _ = typing.check(&sig)?;
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

    // run as long as we receive events, and abort on error
    iter.filter(|event| !opt.no_scope || event.is_err())
        .map(|event| Command::from_event(event?, &mut syms, &arena).map_err(Error::Ko))
        .map(|ro| ro.transpose())
        .flatten()
        .filter(|cmd| !opt.no_infer || cmd.is_err())
        .map(|cmd| cmd?.infer(&mut sig).map_err(Error::Ko))
        .map(|ro| ro.transpose())
        .flatten()
        .filter(|cmd| !opt.no_check || cmd.is_err())
        .par_bridge()
        .try_for_each(|ts| check(ts?).map_err(Error::Ko))
}
