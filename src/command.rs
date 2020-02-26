use crate::pattern::Pattern;
use crate::precommand::GDCommand;
use crate::symbol::Symbol;
use crate::term::RTerm;

pub enum Command {
    DCmd(Symbol, DCommand),
    Rule(Vec<String>, Pattern, RTerm),
}

pub type DCommand = GDCommand<RTerm, RTerm>;
