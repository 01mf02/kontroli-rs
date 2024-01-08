use clap::ValueEnum;

/// A stage in the processing of commands.
///
/// This is useful to omit certain parts of command processing.
/// Omitting one stage also omits all stages after it,
/// i.e. all stages greater than the stage.
#[derive(Clone, Debug, PartialEq, Eq, ValueEnum)]
pub enum Stage {
    /// Sharing
    Share,
    /// Type inference
    Infer,
    /// Type checking
    Check,
}
