use crate::{parse, Error, Event, Opt, PathRead, Stage};
use colosseum::unsync::Arena;
use core::{borrow::Borrow, convert::TryFrom};
use kontroli::error::Error as KoError;
use kontroli::rc::{Intro, Rule, Signature, Typing};
use kontroli::{Share, Symbol, Symbols};

type Command<'s> = kontroli::Command<Symbol<'s>, Intro<'s>, Rule<'s>>;

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

fn infer_check<'s>(cmd: Command<'s>, check: bool, sig: &mut Signature<'s>) -> Result<(), KoError> {
    match cmd {
        kontroli::Command::Intro(sym, it) => {
            let typing = Typing::new(it, &sig)?;
            let typing = if check { typing.check(&sig)? } else { typing };
            Ok(sig.insert(sym, typing)?)
        }
        kontroli::Command::Rules(rules) => Ok(sig.add_rules(rules.into_iter())?),
    }
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

        let mut cmds = kontroli::parse::lexes(&file.read)
            .map(|tokens| parse::<&str>(tokens?, &opt))
            .map(|res| res.transpose())
            .flatten()
            .filter(|cmd| !opt.omits(Share) || cmd.is_err())
            .map(|cmd| share(cmd?, &mut syms, &arena).map_err(Error::Ko))
            .filter(|cmd| !opt.omits(Infer) || cmd.is_err());

        cmds.try_for_each(|cmd| infer_check(cmd?, !opt.omits(Check), &mut sig).map_err(Error::Ko))?;
    }
    Ok(())
}

pub fn consume<I>(iter: I, opt: &Opt) -> Result<(), Error>
where
    I: Iterator<Item = Result<Event, Error>>,
{
    let arena: Arena<String> = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut sig: Signature = Signature::new();

    sig.eta = opt.eta;

    use Stage::{Check, Infer, Share};

    // run as long as we receive events, and abort on error
    let mut cmds = iter
        .filter(|event| !opt.omits(Share) || event.is_err())
        .map(|event| from_event(event?, &mut syms, &arena).map_err(Error::Ko))
        .map(|ro| ro.transpose())
        .flatten()
        .filter(|cmd| !opt.omits(Infer) || cmd.is_err());

    cmds.try_for_each(|cmd| infer_check(cmd?, !opt.omits(Check), &mut sig).map_err(Error::Ko))
}
