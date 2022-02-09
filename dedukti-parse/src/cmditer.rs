use crate::{cmd, term, Command, Scope, Symb, Term, Token};
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

pub struct CmdIter<'s, S, A, V>
where
    Token<S>: Logos<'s>,
{
    lexer: logos::Lexer<'s, Token<S>>,
    ctx: Ctx<A, V>,
}

pub struct Lazy<I, A, V> {
    lines: I,
    last: String,
    state: State<String, String, A, V>,
    ctx: Ctx<A, V>,
    buf: Vec<Command<String, V, Term<A, V>>>,
}

impl<'s, A, V> CmdIter<'s, &'s str, A, V> {
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
            last: String::new(),
            state: State::default(),
            ctx: Default::default(),
            buf: Vec::new(),
        }
    }
}

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

impl<S: Into<C> + Into<V>, C, A: Scope<S, V>, V: cmd::Joker> State<S, C, A, V> {
    fn feed<I>(self, ctx: &mut Ctx<A, V>, token: Token<S>, iter: &mut I) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<S>>,
    {
        if self.cmd.expects_term() {
            let iter = &mut core::iter::once(token).chain(iter);
            match self.trm.parse(ctx, iter) {
                Ok(term::State::Term(tm, tok)) => {
                    let cmd = self
                        .cmd
                        .apply(ctx.bound_mut(), tm, tok)
                        .map_err(Error::Command)?;
                    let trm = term::State::Init;
                    Ok(State { cmd, trm })
                }
                Ok(trm) => Ok(State { cmd: self.cmd, trm }),
                Err(e) => Err(Error::Term(e)),
            }
        } else {
            assert!(matches!(self.trm, term::State::Init));
            let cmd = self
                .cmd
                .parse(ctx.bound_mut(), token)
                .map_err(Error::Command)?;
            Ok(State { cmd, trm: self.trm })
        }
    }
}

impl<'s, S: Into<V>, A: Scope<S, V>, V: cmd::Joker> Iterator for CmdIter<'s, S, A, V>
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
                Ok(st) => match st.cmd {
                    cmd::State::Command(cmd) => return Some(Ok(cmd)),
                    _ => state = st,
                },
            }
        }
        token_seen.then(|| Err(Error::ExpectedInput))
    }
}

// TODO: implement support for open multi-line comments
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
            let last = &mut self.last;
            let mut state = core::mem::take(&mut self.state).map_symb(|s| {
                *last = s;
                &*last as &str
            });

            let mut lexer = Token::lexer(line.borrow());
            while let Some(next) = lexer.next() {
                match state.feed(&mut self.ctx, next, &mut lexer) {
                    Err(e) => return Some(Err(e)),
                    Ok(st) => match st.cmd {
                        cmd::State::Command(cmd) => {
                            self.buf.push(cmd);
                            state = State::default()
                        }
                        _ => state = st,
                    },
                };
            }

            if !self.buf.is_empty() {
                self.buf.reverse();
                return self.buf.pop().map(Ok);
            }

            self.state = state.map_symb(|s| s.to_string());
        }

        if !matches!(self.state.cmd, cmd::State::Init) {
            Some(Err(Error::ExpectedInput))
        } else {
            None
        }
    }
}

impl<'s> Command<&'s str, &'s str, Term<term::Atom<Symb<&'s str>>, &'s str>> {
    pub fn parse_str(s: &'s str) -> Result<Self, Error> {
        CmdIter::new(s).next().unwrap_or(Err(Error::ExpectedInput))
    }
}

impl<'s> Command<String, String, Term<term::Atom<Symb<String>>, String>> {
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
