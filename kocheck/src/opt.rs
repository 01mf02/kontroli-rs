use crate::Stage;
use std::path::PathBuf;
use structopt::StructOpt;

// TODO: introduce --omit argument to replace no_* flags
#[derive(Clone, Debug, StructOpt)]
/// A typechecker for the lambda-Pi calculus modulo rewriting
pub struct Opt {
    /// Reduce terms modulo eta
    ///
    /// When this flag is enabled, checking whether
    /// `\ x => t` and `u` are convertible will succeed if
    /// `\ x => t` and `\ y => u y` are convertible.
    #[structopt(long)]
    pub eta: bool,

    /// Perform only operations until (excluding) the given stage.
    ///
    /// Possible values are: parse, scope, share, infer, check.
    #[structopt(long)]
    pub omit: Option<Stage>,

    /// Parse given number of commands in advance (∞ if argument omitted)
    ///
    /// If this option is used, commands are parsed and checked simultaneously.
    /// If this option is given with a number n, then
    /// maximally n commands are parsed in advance.
    /// If this option is given without an extra argument, then
    /// the number of commands parsed in advance is unbounded.
    ///
    /// Note that unbounded parsing can lead to high memory usage!
    #[structopt(long, short = "c")]
    pub channel_capacity: Option<Option<usize>>,

    /// Typecheck concurrently
    ///
    /// If this option is used, type checking tasks are executed in parallel.
    /// If this option is given with a number n, then
    /// maximally n type checking tasks are concurrently executed.
    /// If this option is given without an extra argument, then
    /// the number of concurrently executed tasks is
    /// determined automatically from the number of CPUs.
    ///
    /// This option enables the parsing of commands in advance ("-c"),
    /// by default with an unbounded capacity.
    #[structopt(long, short = "j")]
    pub jobs: Option<Option<usize>>,

    /// Files to process (cumulative)
    ///
    /// Every file is wrapped in a module corresponding to the file path.
    /// To read from standard input, use "-" as file name.
    #[structopt(name = "FILE", required = true)]
    pub files: Vec<PathBuf>,
}

impl Opt {
    pub fn omits(&self, stage: Stage) -> bool {
        self.omit == Some(stage)
    }
}
