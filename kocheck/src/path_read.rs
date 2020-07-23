use crate::Error;
use std::io::{self, Read};
use std::path::{self, Path, PathBuf};

/// Combination of a module path and a corresponding reader.
pub struct PathRead {
    pub path: Vec<String>,
    pub read: Box<dyn Read>,
}

fn is_dash(file: &PathBuf) -> bool {
    let parts: Vec<_> = file.iter().collect();
    match parts[..] {
        [name] => name == "-",
        _ => false,
    }
}

impl core::convert::TryFrom<&PathBuf> for PathRead {
    type Error = Error;

    fn try_from(file: &PathBuf) -> Result<Self, Error> {
        let path = module_path(file).ok_or(Error::Module)?;
        let read: Box<dyn Read> = if is_dash(file) {
            Box::new(io::stdin())
        } else {
            Box::new(std::fs::File::open(file)?)
        };
        Ok(Self { path, read })
    }
}

/// Return the module path corresponding to a file path.
fn module_path(path: &Path) -> Option<Vec<String>> {
    let components: Vec<_> = path
        .parent()
        .map(|p| p.components().collect())
        .unwrap_or_default();
    let mpath: Option<Vec<_>> = components
        .into_iter()
        .map(|component| match component {
            path::Component::Normal(name) => Some(name),
            _ => None,
        })
        .collect();
    let mut mpath = mpath?;
    mpath.push(path.file_stem()?);
    mpath
        .iter()
        .map(|s| Some(String::from(s.to_str()?)))
        .collect()
}
