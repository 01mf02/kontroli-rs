use crate::precommand::GDCommand;
use crate::rule::Rule;
use crate::symbol::Symbol;
use crate::term::RTerm;

pub enum Command {
    DCmd(Symbol, DCommand),
    Rule(Rule),
}

pub type DCommand = GDCommand<RTerm, RTerm>;
