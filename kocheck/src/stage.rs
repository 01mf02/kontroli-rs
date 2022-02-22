#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stage {
    Share,
    Infer,
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
