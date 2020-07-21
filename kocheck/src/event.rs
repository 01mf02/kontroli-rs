use kontroli::parse::Command;
use kontroli::scope::Symbols;

/// Commands with interspersed module opening.
pub enum Event {
    /// Open a new module
    Module(Vec<String>),
    /// Run a command inside the last opened module
    Command(Command),
}

impl Event {
    pub fn handle<'s>(self, syms: &mut Symbols<'s>) -> Option<Command> {
        match self {
            Self::Module(path) => {
                syms.set_path(path);
                None
            }
            Self::Command(cmd) => Some(cmd),
        }
    }

    pub fn echo(&self) {
        if let Self::Command(Command::Intro(id, _)) = self {
            println!("{}", id)
        }
    }
}
