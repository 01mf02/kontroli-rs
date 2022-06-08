/// A stage in the processing of commands.
///
/// This is useful to omit certain parts of command processing.
/// Omitting one stage also omits all stages after it,
/// i.e. all stages greater than the stage.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stage {
    /// Sharing
    Share,
    /// Type inference
    Infer,
    /// Type checking
    Check,
}

impl core::str::FromStr for Stage {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "share" => Ok(Self::Share),
            "infer" => Ok(Self::Infer),
            "check" => Ok(Self::Check),
            _ => Err("unknown stage: ".to_owned() + s),
        }
    }
}
