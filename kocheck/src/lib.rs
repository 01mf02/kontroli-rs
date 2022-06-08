mod error;
mod event;
mod infer_checks;
mod opt;
mod path_read;
pub mod process;
mod stage;

pub use error::Error;
pub use event::Event;
pub use infer_checks::infer_checks;
pub use opt::Opt;
pub use path_read::PathRead;
pub use stage::Stage;

use kontroli::share::{Intro, Rule};

type Command<'s> = kontroli::Command<kontroli::Symbol<'s>, Intro<'s>, Rule<'s>>;
