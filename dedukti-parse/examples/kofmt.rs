//! Print commands in a Dedukti file, one command per line.

use dedukti_parse::Token;
use std::fmt::Display;
use std::io::{self, Read};

fn no_space_after<S>(token: &Token<S>) -> bool {
    matches!(token, Token::LBrk | Token::LPar | Token::Dot)
}

fn no_space_before<S>(token: &Token<S>) -> bool {
    matches!(token, Token::RBrk | Token::RPar | Token::Comma | Token::Dot)
}

fn print<S: Display>(cmd: impl Iterator<Item = Token<S>>) {
    let mut space = false;

    for token in cmd {
        if space && !no_space_before(&token) {
            print!(" ");
        }
        print!("{}", token);
        space = !no_space_after(&token);

        if matches!(token, Token::Dot) {
            println!("");
        }
    }
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();
    let mut stdin = io::stdin();
    stdin.read_to_string(&mut buffer)?;

    use logos::Logos;
    print(Token::lexer(&buffer));

    Ok(())
}
