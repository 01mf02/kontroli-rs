use core::fmt::{self, Display};
use kontroli::Symbols;

pub type Command = kontroli::scope::Command<kontroli::parse::Symbol>;

/// Commands with interspersed module opening.
pub enum Event {
    /// Open a new module
    Module(Vec<String>),
    /// Run a command inside the last opened module
    Command(Command),
}

impl Event {
    pub fn handle(self, syms: &mut Symbols) -> Option<Command> {
        match self {
            Self::Module(path) => {
                syms.set_path(path);
                None
            }
            Self::Command(cmd) => Some(cmd),
        }
    }
}

impl Display for Event {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Module(path) => {
                let path = path.iter().fold(String::new(), |acc, arg| acc + "/" + arg);
                write!(f, "Open module {}", path)
            }
            Self::Command(Command::Intro(id, _)) => write!(f, "Introduce symbol {}", id),
            Self::Command(Command::Rules(rules)) => write!(f, "Add {} rules", rules.len()),
        }
    }
}
