use crate::error::Error;
use std::io::{self, Read};
use std::path::{self, Path, PathBuf};

pub type PathRead = (Vec<String>, Box<dyn Read>);
pub type PathReads<'a> = Box<dyn Iterator<Item = Result<PathRead, Error>> + 'a>;

/// Return stdin if no files given, else lazily open and return the files.
pub fn path_reads<'a>(files: &'a [PathBuf]) -> PathReads<'a> {
    if files.is_empty() {
        let read: Box<dyn Read> = Box::new(io::stdin());
        Box::new(std::iter::once(Ok((Vec::new(), read))))
    } else {
        Box::new(files.iter().map(|file| {
            let module = module_path(file).ok_or(Error::Module)?;
            let read: Box<dyn Read> = Box::new(std::fs::File::open(file)?);
            Ok((module, read))
        }))
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
