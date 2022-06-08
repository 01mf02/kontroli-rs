//! Parser for the Dedukti file format
//!
//! Example usage:
//!
//! ~~~
//! use dedukti_parse::{Command, Error, Strict, Symb};
//!
//! let cmds = "prop: Type. def proof : prop -> Type.";
//! let cmds = Strict::<_, Symb<&str>, &str>::new(&cmds);
//! let cmds: Result<Vec<_>, _> = cmds.collect();
//! assert_eq!(cmds?.len(), 2);
//! # Ok::<_, Error>(())
//! ~~~
#![no_std]

extern crate alloc;

pub mod cmd;
pub mod lex;
pub mod symb;
pub mod term;

pub use cmd::{Command, Intro, Rule};
pub use lex::Token;
pub use symb::Symb;
pub use term::{Atom, Scope, Term};

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::borrow::Borrow;
use logos::Logos;
use term::Ctx;

#[derive(Debug, PartialEq)]
pub enum Error {
    Command(cmd::Error),
    Term(term::Error),
    ExpectedInput,
}

pub struct Strict<'s, S, A, V>
where
    Token<S>: Logos<'s>,
{
    lexer: logos::Lexer<'s, Token<S>>,
    ctx: Ctx<A, V>,
}

pub struct Lazy<I, A, V> {
    lines: I,
    open_comments: usize,
    last: String,
    state: State<String, String, A, V>,
    ctx: Ctx<A, V>,
    buf: Vec<Command<String, V, Term<A, V>>>,
}

impl<'s, A, V> Strict<'s, &'s str, A, V> {
    pub fn new(s: &'s str) -> Self {
        Self {
            lexer: Token::lexer(s),
            ctx: Default::default(),
        }
    }
}

impl<I, A, V> Lazy<I, A, V> {
    pub fn new(lines: I) -> Self {
        Self {
            lines,
            open_comments: 0,
            last: String::new(),
            state: State::default(),
            ctx: Default::default(),
            buf: Vec::new(),
        }
    }
}

/// Hold the state of both command and term parsers.
struct State<S, C, A, V> {
    cmd: cmd::State<C, V, Term<A, V>>,
    trm: term::State<S, A, V>,
}

impl<S, C, A, V> State<S, C, A, V> {
    fn map_symb<T>(self, f: impl FnOnce(S) -> T) -> State<T, C, A, V> {
        State {
            cmd: self.cmd,
            trm: self.trm.map_symb(f),
        }
    }
}

impl<S, C, A, V> Default for State<S, C, A, V> {
    fn default() -> Self {
        Self {
            cmd: cmd::State::Init,
            trm: term::State::Init,
        }
    }
}

type Open = usize;

impl<S: Into<C> + Into<V>, C, A: Scope<S, V>, V: cmd::Joker> State<S, C, A, V> {
    fn feed<I>(
        self,
        ctx: &mut Ctx<A, V>,
        token: Token<S>,
        iter: &mut I,
    ) -> Result<(Self, Open), Error>
    where
        I: Iterator<Item = Token<S>>,
    {
        if self.cmd.expects_term() {
            let iter = &mut core::iter::once(token).chain(iter);
            match self.trm.parse(ctx, iter) {
                Ok((trm, None)) => Ok((State { cmd: self.cmd, trm }, 0)),
                Ok((trm, Some(Token::Comment(o)))) => Ok((State { cmd: self.cmd, trm }, o)),
                Ok((term::State::ATerm(None, tm), Some(tok))) => {
                    let cmd = self
                        .cmd
                        .apply(ctx.bound_mut(), tm, tok)
                        .map_err(Error::Command)?;
                    let trm = term::State::Init;
                    Ok((State { cmd, trm }, 0))
                }
                Ok((_, Some(_tok))) => panic!("unrecognised token"),
                Err(e) => Err(Error::Term(e)),
            }
        } else {
            assert!(matches!(self.trm, term::State::Init));
            if let Token::Comment(open) = token {
                return Ok((self, open));
            }
            let cmd = self
                .cmd
                .parse(ctx.bound_mut(), token)
                .map_err(Error::Command)?;
            Ok((State { cmd, trm: self.trm }, 0))
        }
    }
}

impl<'s, S: Into<V>, A: Scope<S, V>, V: cmd::Joker> Iterator for Strict<'s, S, A, V>
where
    Token<S>: Logos<'s>,
{
    type Item = Result<Command<S, V, Term<A, V>>, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut state = State::<S, S, A, V>::default();
        let mut token_seen = false;

        while let Some(next) = self.lexer.next() {
            token_seen = true;
            match state.feed(&mut self.ctx, next, &mut self.lexer) {
                Err(e) => return Some(Err(e)),
                Ok((st, open)) => match st.cmd {
                    cmd::State::Command(cmd) => return Some(Ok(cmd)),
                    _ if open > 0 => break,
                    _ => state = st,
                },
            }
        }
        token_seen.then(|| Err(Error::ExpectedInput))
    }
}

impl<I, A, V> Iterator for Lazy<I, A, V>
where
    I: Iterator,
    I::Item: Borrow<str>,
    A: for<'a> Scope<&'a str, V>,
    V: for<'a> From<&'a str> + cmd::Joker,
{
    type Item = Result<Command<String, V, Term<A, V>>, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.buf.pop() {
            return Some(Ok(next));
        }

        for line in &mut self.lines {
            let mut lexer = Token::lexer(line.borrow());

            // eat leading open comments
            self.open_comments = lex::comment(&mut lexer, self.open_comments);
            if self.open_comments > 0 {
                continue;
            }

            let mut state = core::mem::take(&mut self.state).map_symb(|s| {
                self.last = s;
                &self.last as &str
            });

            while let Some(next) = lexer.next() {
                match state.feed(&mut self.ctx, next, &mut lexer) {
                    Err(e) => return Some(Err(e)),
                    Ok((st, open)) => {
                        state = if let cmd::State::Command(cmd) = st.cmd {
                            self.buf.push(cmd);
                            State::default()
                        } else {
                            st
                        };
                        self.open_comments = open;
                    }
                };
            }

            self.state = state.map_symb(|s| s.to_string());

            if !self.buf.is_empty() {
                self.buf.reverse();
                return self.buf.pop().map(Ok);
            }
        }

        if self.open_comments > 0 || !matches!(self.state.cmd, cmd::State::Init) {
            Some(Err(Error::ExpectedInput))
        } else {
            None
        }
    }
}

pub type Scoped<S> = Command<S, S, Term<term::Atom<Symb<S>>, S>>;

impl<'s> Scoped<&'s str> {
    pub fn parse_str(s: &'s str) -> Result<Self, Error> {
        Strict::new(s).next().unwrap_or(Err(Error::ExpectedInput))
    }
}

impl Scoped<String> {
    pub fn parse_lines<S: Borrow<str>>(lines: impl Iterator<Item = S>) -> Result<Self, Error> {
        Lazy::new(lines).next().unwrap_or(Err(Error::ExpectedInput))
    }
}

#[test]
fn positive() -> Result<(), Error> {
    Command::parse_str("prop : Type.")?;
    Command::parse_str("imp: prop -> prop -> prop.")?;
    Command::parse_str("def prf: prop -> Type.")?;
    Command::parse_str("[x, y] prf (imp x y) --> prf x -> prf y.")?;
    Command::parse_str("thm imp_refl (x: prop) : prf (imp x x) := p: prf x => p.")?;
    Command::parse_str("[] eq _ _ --> false.")?;

    Command::parse_lines("prop :\nType.".lines())?;
    Command::parse_lines("imp : prop\n-> prop\n-> prop\n.".lines())?;
    Command::parse_lines("(; \n ;) prop : Type. (; \n ;)".lines())?;
    Command::parse_lines("imp : prop (; \n ;) -> (; ;) prop\n-> prop\n(; ;).".lines())?;
    Ok(())
}

#[test]
fn negative() {
    use cmd::Error::*;
    let parse_err = |s: &str| match Command::parse_str(s) {
        Err(Error::Command(e)) => e,
        _ => panic!("command error expected"),
    };
    assert_eq!(parse_err("."), ExpectedCmd);
    assert_eq!(parse_err("x ->"), ExpectedColon);
    assert_eq!(parse_err("def :"), ExpectedIdent);
    assert_eq!(parse_err("def d ->"), ExpectedColonOrColonEq);
    assert_eq!(parse_err("thm t := tm."), ExpectedColon);
    assert_eq!(parse_err("thm t :  ty."), ExpectedColonEq);
    assert_eq!(parse_err("thm t (->"), ExpectedIdent);
    assert_eq!(parse_err("thm t (x ->"), ExpectedColon);
    assert_eq!(parse_err("thm t (x : a -->"), ExpectedRPar);
    assert_eq!(parse_err("[->"), ExpectedCommaOrRBrk);
    assert_eq!(parse_err("[x ->"), ExpectedCommaOrRBrk);
    assert_eq!(parse_err("[x] l."), ExpectedLongArrow);
}
