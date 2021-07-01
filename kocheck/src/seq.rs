//! Sequential event processing.

use crate::{Error, Event, Opt};
use colosseum::unsync::Arena;
use kontroli::error::Error as KoError;
use kontroli::rc::{Intro, Rule, Signature, Typing};
use kontroli::{Share, Symbol, Symbols};

struct Command<'s>(kontroli::Command<Symbol<'s>, Intro<'s>, Rule<'s>>);

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

    fn infer_check(self, check: bool, sig: &mut Signature<'s>) -> Result<(), KoError> {
        match self.0 {
            kontroli::Command::Intro(sym, it) => {
                let typing = Typing::new(it, &sig)?;
                let typing = if check { typing.check(&sig)? } else { typing };
                Ok(sig.insert(sym, typing)?)
            }
            kontroli::Command::Rules(rules) => Ok(sig.add_rules(rules.into_iter())?),
        }
    }
}

pub fn consume<I>(iter: I, opt: &Opt) -> Result<(), Error>
where
    I: Iterator<Item = Result<Event, Error>>,
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
        .try_for_each(|cmd| cmd?.infer_check(!opt.no_check, &mut sig).map_err(Error::Ko))
}
