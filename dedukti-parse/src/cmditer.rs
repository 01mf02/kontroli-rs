use crate::{cmd, term, Command, Term, Token};
use alloc::vec::Vec;

#[derive(Debug, PartialEq)]
pub enum Error {
    Command(cmd::Error),
    Term(term::Error),
    ExpectedInput,
}

pub struct CmdIter<'s, S>
where
    Token<S>: logos::Logos<'s>,
{
    lexer: logos::Lexer<'s, Token<S>>,
    tokens: Vec<Token<S>>,
    stack: term::Stack<S>,
    bound: Vec<S>,
}

impl<'s> CmdIter<'s, &'s str> {
    pub fn new(s: &'s str) -> Self {
        use logos::Logos;
        Self {
            lexer: Token::lexer(s),
            tokens: Vec::new(),
            stack: Default::default(),
            bound: Vec::new(),
        }
    }
}

impl<'s, S: cmd::Joker> Iterator for CmdIter<'s, S>
where
    Token<S>: logos::Logos<'s>,
{
    type Item = Result<Command<S, S, Term<S>>, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        crate::period(&mut self.lexer, &mut self.tokens);
        if self.tokens.is_empty() {
            return None;
        }

        let mut cmds = cmd::State::Init;
        let mut trms = term::State::Init;

        let mut iter = self.tokens.drain(..).peekable();
        let stack = &mut self.stack;

        while iter.peek().is_some() {
            if cmds.expects_term() {
                match trms.parse(stack, &mut iter) {
                    Ok(term::State::Term(tm, tok)) => match cmds.apply(&mut self.bound, tm, tok) {
                        Ok(cmd::State::Command(cmd)) => return Some(Ok(cmd)),
                        Ok(st) => {
                            trms = term::State::Init;
                            cmds = st
                        }
                        Err(e) => return Some(Err(Error::Command(e))),
                    },
                    Ok(st) => trms = st,
                    Err(e) => return Some(Err(Error::Term(e))),
                };
            } else {
                assert!(matches!(trms, term::State::Init));
                match cmds.parse(&mut self.bound, iter.next().unwrap()) {
                    Ok(st) => cmds = st,
                    Err(e) => return Some(Err(Error::Command(e))),
                }
            }
        }
        Some(Err(Error::ExpectedInput))
    }
}

impl<'s> Command<&'s str, &'s str, Term<&'s str>> {
    pub fn parse_str(s: &'s str) -> Result<Self, Error> {
        let err = Err(Error::ExpectedInput);
        CmdIter::new(s).next().unwrap_or(err)
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
