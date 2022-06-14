use crate::Stage;
use clap::Parser;
use std::path::PathBuf;

#[derive(Clone, Debug, Parser)]
/// A typechecker for the lambda-Pi calculus modulo rewriting
pub struct Opt {
    /// Reduce terms modulo eta
    ///
    /// When this is enabled,
    /// `x => t` and `u` are convertible if
    /// `x => t` and `y => u y` are convertible.
    #[clap(long)]
    pub eta: bool,

    /// Perform only operations until (excluding) the given stage.
    #[clap(long, value_name = "STAGE", arg_enum)]
    pub omit: Option<Stage>,

    /// Parse N commands in advance (∞ if argument omitted)
    ///
    /// If this option is used, commands are parsed and checked simultaneously.
    /// If this option is given with a number N, then
    /// maximally N commands are parsed in advance.
    /// If this option is given without an extra argument, then
    /// the number of commands parsed in advance is unbounded.
    ///
    /// Note that unbounded parsing can lead to high memory usage!
    #[clap(long, short = 'c', value_name = "N")]
    pub channel_capacity: Option<Option<usize>>,

    /// Typecheck up to N commands concurrently (∞ if argument omitted)
    ///
    /// If this option is used, type checking tasks are executed in parallel.
    /// If this option is given with a number N, then
    /// at most N type checking tasks are executed concurrently.
    /// If this option is given without an extra argument, then
    /// the number of concurrently executed tasks is
    /// determined automatically from the number of CPUs.
    #[clap(long, short = 'j', value_name = "N")]
    pub jobs: Option<Option<usize>>,

    /// Files to process (cumulative)
    ///
    /// Every file is wrapped in a module corresponding to the file path.
    /// To read from standard input, use "-" as file name.
    #[clap(name = "FILE", required = true)]
    pub files: Vec<PathBuf>,
}

impl Opt {
    pub fn omits(&self, stage: Stage) -> bool {
        self.omit == Some(stage)
    }
}
