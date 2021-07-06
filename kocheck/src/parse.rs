use crate::{Opt, Stage};
use kontroli::parse::{Command as PCommand, Parse, Token};
use kontroli::scope::{Command as SCommand, Scope};
use kontroli::Error;

pub fn parse<'s, S>(tokens: Vec<Token<'s>>, opt: &Opt) -> Result<Option<SCommand<S>>, Error>
where
    S: From<&'s str>,
{
    if opt.omits(Stage::Parse) {
        return Ok(None);
    }
    let cmd = PCommand::parse_vec(tokens)?;

    match &cmd {
        PCommand::Intro(id, _, _) => log::info!("Introduce symbol {}", id),
        PCommand::Rules(rules) => log::info!("Add {} rules", rules.len()),
    };

    if opt.omits(Stage::Scope) {
        return Ok(None);
    }
    Ok(Some(cmd.scope()))
}
