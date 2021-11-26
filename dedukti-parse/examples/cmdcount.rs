//! Count number of commands in a Dedukti file.

use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buffer)?;

    let mut cmds = 0;

    for cmd in dedukti_parse::lexes(&buffer) {
        if cmd.is_ok() {
            cmds += 1;
        } else {
            let err = format!("lexing command {}", cmds);
            return Err(io::Error::new(io::ErrorKind::InvalidData, err));
        }
    }
    println!("{}", cmds);

    Ok(())
}
