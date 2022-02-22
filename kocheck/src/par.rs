//! Parallel event processing.

use crate::{Error, Event, Opt, PCommand, PathRead, Stage};
use colosseum::sync::Arena;
use core::borrow::Borrow;
use kontroli::error::Error as KoError;
use kontroli::kernel::{typing, GCtx, Intro, Rule};
use kontroli::{Share, Symbol, Symbols};
use rayon::iter::{ParallelBridge, ParallelIterator};

type Command<'s> = kontroli::Command<Symbol<'s>, Intro<'s>, Rule<'s>>;

type Checks<'s> = (Vec<typing::Check<'s>>, GCtx<'s>);

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
    cmd: PCommand<S>,
    syms: &mut Symbols<'s>,
    arena: &'s Arena<String>,
) -> Result<Command<'s>, KoError> {
    match cmd.share(syms)? {
        kontroli::Command::Intro(id, it) => {
            let id = syms.insert(arena.alloc(id))?;
            Ok(Command::Intro(id, it))
        }
        kontroli::Command::Rules(rules) => Ok(Command::Rules(rules)),
    }
}

fn infer<'s>(cmd: Command<'s>, gc: &mut GCtx<'s>) -> Result<Checks<'s>, KoError> {
    let mut checks = (Vec::new(), gc.clone());
    match cmd {
        kontroli::Command::Intro(sym, it) => {
            let rewritable = it.rewritable();

            // defer checking to later
            let (typing, check) = typing::intro(it, gc)?;
            check.into_iter().for_each(|chk| checks.0.push(chk));
            gc.insert(sym, typing, rewritable)?;
        }
        kontroli::Command::Rules(rules) => {
            for rule in rules.clone() {
                let rule = rule.map_lhs(kontroli::Pattern::from);
                if let Ok(rule) = kontroli::Rule::try_from(rule) {
                    checks.0.push(typing::rewrite(rule, gc)?);
                } else {
                    log::warn!("Rewrite rule contains unannotated variable")
                }
            }
            rules.into_iter().try_for_each(|r| gc.add_rule(r))?
        }
    }
    Ok(checks)
}

fn check((checks, gc): Checks) -> Result<(), KoError> {
    checks.into_iter().try_for_each(|chk| Ok(chk.check(&gc)?))
}

fn infer_checks<'s, I>(iter: I, opt: &Opt, gc: &mut GCtx<'s>) -> Result<(), Error>
where
    I: Iterator<Item = Result<Command<'s>, Error>> + Send,
{
    let mut iter = iter
        .map(|cmd| infer(cmd?, gc).map_err(Error::Ko))
        .filter(|cmd| !opt.omits(Stage::Check) || cmd.is_err());
    if opt.jobs.is_some() {
        iter.par_bridge()
            .try_for_each(|ts| check(ts?).map_err(Error::Ko))
    } else {
        iter.try_for_each(|ts| check(ts?).map_err(Error::Ko))
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

        use std::io::{BufRead, BufReader};
        let lines = BufReader::new(file.read).lines().map(|line| line.unwrap());
        let cmds = kontroli::parse::Lazy::new(lines)
            .inspect(|cmd| cmd.iter().for_each(crate::log_cmd))
            .filter(|cmd| !opt.omits(Stage::Share) || cmd.is_err())
            .map(|cmd| share(cmd?, &mut syms, &arena).map_err(Error::Ko))
            .filter(|cmd| !opt.omits(Stage::Infer) || cmd.is_err());

        infer_checks(cmds, opt, &mut gc)?
    }
    Ok(())
}

pub fn consume<I>(iter: I, opt: &Opt) -> Result<(), Error>
where
    I: Iterator<Item = Result<Event, Error>> + Send,
{
    let arena: Arena<String> = Arena::new();
    let mut syms: Symbols = Symbols::new();
    let mut gc: GCtx = GCtx::new();

    gc.eta = opt.eta;

    // run as long as we receive events, and abort on error
    let cmds = iter
        .filter(|event| !opt.omits(Stage::Share) || event.is_err())
        .map(|event| from_event(event?, &mut syms, &arena).map_err(Error::Ko))
        .map(|ro| ro.transpose())
        .flatten()
        .filter(|cmd| !opt.omits(Stage::Infer) || cmd.is_err());

    infer_checks(cmds, opt, &mut gc)
}
