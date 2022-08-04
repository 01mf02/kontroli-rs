//! Print commands in a Dedukti file, one command per line.

use clap::{ArgEnum, Parser};
use dedukti_parse::{term, Command, Error, Lazy, Strict, Symb, Token};
use std::fmt::{Debug, Display};
use std::io::{self};
use std::path::PathBuf;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

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
    /// Print nothing.
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

fn print_token<S: Display>(token: Token<S>, space: &mut bool) {
    if *space && !no_space_before(&token) {
        print!(" ");
    }
    print!("{}", token);
    *space = !no_space_after(&token);

    if matches!(token, Token::Dot) {
        println!("");
    }
}

fn print_tokens<S: Display>(iter: impl Iterator<Item = Token<S>>, opt: &Opt) {
    let mut space = false;

    for token in iter {
        if matches!(token, Token::Dot) {
            log::info!("command lexed");
        }

        if !opt.quiet {
            print_token(token, &mut space)
        }
    }
}

fn print_cmds<C: Display, V: Display, Tm: Display>(
    iter: impl Iterator<Item = Result<Command<C, V, Tm>, Error>>,
    opt: &Opt,
) {
    for cmd in iter {
        let cmd = cmd.unwrap();
        log::info!("command parsed");
        if !opt.quiet {
            print!("{cmd}")
        }
    }
}

fn main() -> io::Result<()> {
    env_logger::Builder::from_env("LOG").init();

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
            print_tokens(Token::lexer(&file), &opt)
        };
    }
    Ok(())
}
