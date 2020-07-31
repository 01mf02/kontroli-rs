//! Sequential event processing.

use crate::{Error, Event, Opt};
use colosseum::unsync::Arena;
use kontroli::error::Error as KoError;
use kontroli::parse;
use kontroli::rc::{Intro, Rule, Signature, Typing};
use kontroli::scope::{self, Symbols};

struct Command<'s>(scope::Command<'s, scope::Symbol<'s>>);

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

    fn infer_check(self, check: bool, sig: &mut Signature<'s>) -> Result<(), KoError> {
        match self.0 {
            scope::Command::Intro(sym, it) => {
                let typing = Typing::new(Intro::from(it), &sig)?;
                let typing = if check { typing.check(&sig)? } else { typing };
                Ok(sig.insert(sym, typing)?)
            }
            scope::Command::Rules(rules) => Ok(sig.add_rules(rules.into_iter().map(Rule::from))?),
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
