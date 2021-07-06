#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stage {
    Parse,
    Scope,
    Share,
    Infer,
    Check,
}

impl core::str::FromStr for Stage {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "parse" => Ok(Self::Parse),
            "scope" => Ok(Self::Scope),
            "share" => Ok(Self::Share),
            "infer" => Ok(Self::Infer),
            "check" => Ok(Self::Check),
            _ => Err("unknown stage: ".to_owned() + s),
        }
    }
}
