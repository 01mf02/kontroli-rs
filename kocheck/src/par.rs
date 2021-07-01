//! Parallel event processing.

use crate::{Error, Event, Opt};
use colosseum::sync::Arena;
use kontroli::arc::{Intro, Rule, Signature, Typing};
use kontroli::error::Error as KoError;
use kontroli::{Share, Symbol, Symbols};
use rayon::iter::{ParallelBridge, ParallelIterator};

struct Command<'s>(kontroli::Command<Symbol<'s>, Intro<'s>, Rule<'s>>);

type Check<'s> = (Typing<'s>, Signature<'s>);

impl<'s> Command<'s> {
    fn from_precommand(
        cmd: super::event::Command,
        syms: &mut Symbols<'s>,
        arena: &'s Arena<String>,
    ) -> Result<Self, KoError> {
        match cmd {
            kontroli::Command::Intro(id, it) => {
                let it = it.share(syms)?;
                let id = syms.insert(arena.alloc(id))?;
                Ok(kontroli::Command::Intro(id, it))
            }
            kontroli::Command::Rules(rules) => {
                let rules = rules.into_iter().map(|r| r.share(syms));
                Ok(kontroli::Command::Rules(rules.collect::<Result<_, _>>()?))
            }
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
