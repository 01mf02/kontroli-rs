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

use kontroli::parse::Item;

pub fn log_cmd<S: core::fmt::Display>(cmd: &Item<S>) {
    match cmd {
        Item::Intro(id, _, _) => log::info!("Introduce symbol {}", id),
        Item::Rules(rules) => log::info!("Add {} rules", rules.len()),
    }
}
