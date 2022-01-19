use crate::{cmd, term, Bound, Command, Term, Token};
use alloc::{string::String, vec::Vec};

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
    bound: Bound,
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

impl<'s, S: Into<String>> Iterator for CmdIter<'s, S>
where
    Token<S>: logos::Logos<'s>,
{
    type Item = Result<Command<Term<S>>, Error>;
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
