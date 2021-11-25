//! Count number of commands in a Dedukti file.

use std::fmt::{Display, Formatter};
use std::io::{self, Read};
use std::sync::atomic::{AtomicU64, Ordering};

struct Progress(AtomicU64);

impl Display for Progress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.load(Ordering::Relaxed))
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buffer)?;

    let cmds = status_line::StatusLine::new(Progress(AtomicU64::new(0)));

    for cmd in dedukti_parse::lexes(&buffer) {
        if cmd.is_ok() {
            cmds.0.fetch_add(1, Ordering::Relaxed);
        } else {
            let err = format!("lexing command {}", *cmds);
            return Err(io::Error::new(io::ErrorKind::InvalidData, err));
        }
    }
    println!("{}", *cmds);

    Ok(())
}
