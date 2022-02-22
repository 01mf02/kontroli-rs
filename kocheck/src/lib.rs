mod error;
mod event;
mod opt;
pub mod par;
mod path_read;
mod stage;

pub use error::Error;
pub use event::Event;
pub use opt::Opt;
pub use path_read::PathRead;
pub use stage::Stage;

use kontroli::parse::{Atom, Command, Symb, Term};
pub type PCommand<S> = Command<S, S, Term<Atom<Symb<S>>, S>>;

pub fn log_cmd<S: core::fmt::Display>(cmd: &PCommand<S>) {
    match cmd {
        PCommand::Intro(id, _, _) => log::info!("Introduce symbol {}", id),
        PCommand::Rules(rules) => log::info!("Add {} rules", rules.len()),
    }
}
