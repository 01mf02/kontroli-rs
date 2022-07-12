//! Print commands in a Dedukti file, one command per line.

use clap::{ArgEnum, Parser};
use dedukti_parse::{term, Command, Error, Lazy, Strict, Symb, Token};
use std::fmt::{Debug, Display};
use std::io::{self};
use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq, Eq, ArgEnum)]
enum Parse {
    Strict,
    Lazy,
}

#[derive(Clone, Debug, Parser)]
pub struct Opt {
    /// Parse instead of only lex
    #[clap(long, value_name = "MODE", arg_enum)]
    parse: Option<Parse>,
    /// Separate symbols into constants and variables.
    #[clap(long)]
    scope: bool,
    /// Use string references when parsing strictly.
    #[clap(long)]
    share: bool,
    /// Print only a dot for each read command.
    ///
    /// This is useful to protocol when commands were read,
    /// for example with `ts -s %.s`.
    #[clap(long)]
    quiet: bool,

    files: Vec<PathBuf>,
}

fn no_space_after<S>(token: &Token<S>) -> bool {
    matches!(token, Token::LBrk | Token::LPar | Token::Dot)
}

fn no_space_before<S>(token: &Token<S>) -> bool {
    matches!(token, Token::RBrk | Token::RPar | Token::Comma | Token::Dot)
}

fn print_tokens<S: Display>(iter: impl Iterator<Item = Token<S>>) {
    let mut space = false;

    for token in iter {
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

fn print_cmds<C: Display, V: Display, Tm: Display>(
    iter: impl Iterator<Item = Result<Command<C, V, Tm>, Error>>,
    opt: &Opt,
) {
    for cmd in iter {
        let cmd = cmd.unwrap();
        if !opt.quiet {
            print!("{cmd}")
        }
        println!(".");
    }
}

fn main() -> io::Result<()> {
    let opt = Opt::parse();

    for path in &opt.files {
        if let Some(mode) = &opt.parse {
            type Atom<S> = term::Atom<Symb<S>>;
            match mode {
                Parse::Lazy => {
                    use std::io::{BufRead, BufReader};
                    let file = std::fs::File::open(path)?;
                    let lines = BufReader::new(file).lines().map(|line| line.unwrap());
                    if opt.scope {
                        print_cmds(Lazy::<_, Atom<String>, String>::new(lines), &opt)
                    } else {
                        print_cmds(Lazy::<_, Symb<String>, String>::new(lines), &opt)
                    }
                }
                Parse::Strict => {
                    let file = std::fs::read_to_string(path)?;
                    if opt.share {
                        if opt.scope {
                            print_cmds(Strict::<_, Atom<&str>, &str>::new(&file), &opt)
                        } else {
                            print_cmds(Strict::<_, Symb<&str>, &str>::new(&file), &opt)
                        }
                    } else {
                        if opt.scope {
                            print_cmds(Strict::<_, Atom<String>, String>::new(&file), &opt)
                        } else {
                            print_cmds(Strict::<_, Symb<String>, String>::new(&file), &opt)
                        }
                    }
                }
            }
        } else {
            let file = std::fs::read_to_string(path)?;
            use logos::Logos;
            let lexer = Token::lexer(&file);
            if opt.quiet {
                for token in lexer {
                    if token == Token::Dot {
                        println!(".")
                    }
                }
            } else {
                print_tokens(lexer)
            }
        };
    }
    Ok(())
}
