use byte_unit::{Byte, ByteError};
use std::path::PathBuf;
use structopt::StructOpt;

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

    /// Only parse; neither scope, infer, nor check
    #[structopt(long)]
    pub no_scope: bool,

    /// Only parse and scope; neither infer nor check
    #[structopt(long)]
    pub no_infer: bool,

    /// Only parse, scope, and infer; do not check
    #[structopt(long)]
    pub no_check: bool,

    /// Size of the parse buffer
    ///
    /// The parser repeatedly reads data into a buffer and parses a command.
    /// If a command does not fit into the buffer,
    /// the buffer size is doubled and parsing is retried,
    /// until either parsing succeeds or the whole file is read.
    ///
    /// Therefore, the buffer size should be chosen to be
    /// larger than the size of the largest expected command,
    /// to avoid unnecessary retries.
    #[structopt(long, default_value = "64MB", parse(try_from_str = parse_byte))]
    pub buffer: Byte,

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

fn parse_byte<S: AsRef<str>>(s: S) -> Result<Byte, MyByteError> {
    Byte::from_str(s).map_err(MyByteError)
}

#[derive(Debug)]
struct MyByteError(ByteError);

impl ToString for MyByteError {
    fn to_string(&self) -> String {
        match &self.0 {
            ByteError::ValueIncorrect(s) => "Incorrect byte value: ".to_owned() + &s.clone(),
            ByteError::UnitIncorrect(s) => "Incorrect byte unit: ".to_owned() + &s.clone(),
        }
    }
}
