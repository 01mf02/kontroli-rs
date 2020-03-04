use crate::precommand::GDCommand;
use crate::rule::UncheckedRule;
use crate::symbol::Symbol;
use crate::term::RTerm;

pub enum Command {
    DCmd(Symbol, DCommand),
    Rule(UncheckedRule),
}

pub type DCommand = GDCommand<RTerm, RTerm>;
