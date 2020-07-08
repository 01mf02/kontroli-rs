use crate::Error;
use std::io::{self, Read};
use std::path::{self, Path, PathBuf};

/// Combination of a module path and a corresponding reader.
pub struct PathRead {
    pub path: Vec<String>,
    pub read: Box<dyn Read>,
}

type PathReads<'a> = Box<dyn Iterator<Item = Result<PathRead, Error>> + 'a>;

impl PathRead {
    fn from_stdin() -> Self {
        let path = Vec::new();
        let read: Box<dyn Read> = Box::new(io::stdin());
        Self { path, read }
    }

    fn from_pathbuf(file: &PathBuf) -> Result<Self, Error> {
        let path = module_path(file).ok_or(Error::Module)?;
        let read: Box<dyn Read> = Box::new(std::fs::File::open(file)?);
        Ok(Self { path, read })
    }

    /// Return stdin if no files given, else lazily open and return the files.
    pub fn from_pathbufs(files: &[PathBuf]) -> PathReads {
        if files.is_empty() {
            Box::new(std::iter::once(Ok(Self::from_stdin())))
        } else {
            Box::new(files.iter().map(Self::from_pathbuf))
        }
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
