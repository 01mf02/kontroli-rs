use crate::{Error, Event, Opt, PathRead, Stage};
use colosseum::unsync::Arena;
use core::{borrow::Borrow, convert::TryFrom};
use kontroli::error::Error as KoError;
use kontroli::rc::{GCtx, Intro, Rule, Typing};
use kontroli::{Scope, Share, Symbol, Symbols};

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

fn infer_check<'s>(cmd: Command<'s>, check: bool, gc: &mut GCtx<'s>) -> Result<(), KoError> {
    match cmd {
        kontroli::Command::Intro(sym, it) => {
            let rewritable = it.rewritable();

            let typing = Typing::intro(it, gc)?;
            if check {
                typing.check(gc)?
            }
            Ok(gc.insert(sym, typing, rewritable)?)
        }
        kontroli::Command::Rules(rules) => {
            for rule in rules.clone() {
                if let Ok(rule) = kontroli::Rule::try_from(rule) {
                    let typing = Typing::rewrite(rule, gc)?;
                    if check {
                        typing.check(gc)?
                    }
                } else {
                    log::warn!("Rewrite rule contains unannotated variable")
                }
            }
            Ok(rules.into_iter().try_for_each(|r| gc.add_rule(r))?)
        }
    }
}

pub fn run(opt: &Opt) -> Result<(), Error> {
    let arena: Arena<String> = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut gc: GCtx = GCtx::new();

    gc.eta = opt.eta;

    for file in opt.files.iter() {
        let file = PathRead::try_from(file)?;
        syms.set_path(file.path);

        use kontroli::scope::Command as SCommand;
        use Stage::{Check, Infer, Scope, Share};

        use kontroli::parse::{term::scope_var, CmdIter};
        let mut cmds = CmdIter::new(&file.read, scope_var)
            .inspect(|cmd| cmd.iter().for_each(crate::log_cmd))
            .filter(|cmd| !opt.omits(Scope) || cmd.is_err())
            .map(|cmd| Ok::<_, Error>(cmd?.scope() as SCommand<&str>))
            .filter(|cmd| !opt.omits(Share) || cmd.is_err())
            .map(|cmd| share(cmd?, &mut syms, &arena).map_err(Error::Ko))
            .filter(|cmd| !opt.omits(Infer) || cmd.is_err());

        cmds.try_for_each(|cmd| infer_check(cmd?, !opt.omits(Check), &mut gc).map_err(Error::Ko))?;
    }
    Ok(())
}

pub fn consume<I>(iter: I, opt: &Opt) -> Result<(), Error>
where
    I: Iterator<Item = Result<Event, Error>>,
{
    let arena: Arena<String> = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut gc: GCtx = GCtx::new();

    gc.eta = opt.eta;

    use Stage::{Check, Infer, Share};

    // run as long as we receive events, and abort on error
    let mut cmds = iter
        .filter(|event| !opt.omits(Share) || event.is_err())
        .map(|event| from_event(event?, &mut syms, &arena).map_err(Error::Ko))
        .map(|ro| ro.transpose())
        .flatten()
        .filter(|cmd| !opt.omits(Infer) || cmd.is_err());

    cmds.try_for_each(|cmd| infer_check(cmd?, !opt.omits(Check), &mut gc).map_err(Error::Ko))
}
