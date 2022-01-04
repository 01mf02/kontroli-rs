//! Count number of commands in a Dedukti file.

use dedukti_parse::Token;
use std::io::{self, Read};
use std::fmt::Display;

fn print_cmd<S: Display>(cmd: Vec<Token<S>>) {
    let mut space = false;

    for token in cmd {
        if space && !matches!(token, Token::RBrk | Token::RPar | Token::Comma | Token::Dot) {
            print!(" ");
        }
        print!("{}", token);
        space = !matches!(token, Token::LBrk | Token::LPar | Token::Dot);
    }
    println!(".");
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buffer)?;

    for cmd in dedukti_parse::lexes(&buffer) {
        if let Ok(cmd) = cmd {
            print_cmd(cmd)
        } else {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "lexing command"));
        }
    }

    Ok(())
}
