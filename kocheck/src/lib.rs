mod error;
mod event;
mod opt;
mod path_read;
mod stage;

pub mod par;
pub mod seq;

pub use error::Error;
pub use event::Event;
pub use opt::Opt;
pub use path_read::PathRead;
pub use stage::Stage;

use kontroli::parse::{Atom, Command as PCommand, Symb, Term as PTerm};

pub fn log_cmd<S: core::fmt::Display>(cmd: &PCommand<S, S, PTerm<Atom<Symb<S>>, S>>) {
    match cmd {
        PCommand::Intro(id, _, _) => log::info!("Introduce symbol {}", id),
        PCommand::Rules(rules) => log::info!("Add {} rules", rules.len()),
    }
}
