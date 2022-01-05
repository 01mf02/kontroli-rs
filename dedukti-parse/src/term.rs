use crate::Token;
use alloc::{boxed::Box, vec::Vec};
use core::convert::TryFrom;
use core::fmt::{self, Display};

#[derive(Clone, Debug)]
pub enum Term<S> {
    // Symbol name, preceded by module path
    Symb(Vec<S>, S),
    // Application
    Appl(Box<Term<S>>, Vec<Term<S>>),
    Bind(Box<TermB<S>>),
}

#[derive(Clone, Debug)]
pub enum TermB<S> {
    // Abstraction (`x : A => t`)
    Abst(S, Option<Term<S>>, Term<S>),
    // Dependent product (`x : A -> t`)
    Prod(Option<S>, Term<S>, Term<S>),
}

impl<S> Term<S> {
    pub fn bind(tm: TermB<S>) -> Self {
        Self::Bind(Box::new(tm))
    }
}

impl<S: Display> Display for Term<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Symb(path, s) => {
                path.iter().try_for_each(|p| write!(f, "{}.", p))?;
                s.fmt(f)
            }
            Self::Appl(head, args) => {
                if !args.is_empty() {
                    write!(f, "(")?;
                }
                head.fmt(f)?;
                args.iter().try_for_each(|a| write!(f, " {}", a))?;
                if !args.is_empty() {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Self::Bind(b) => b.fmt(f),
        }
    }
}

impl<S: Display> Display for TermB<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Abst(x, Some(ty), tm) => write!(f, "({} : {} => {})", x, ty, tm),
            Self::Abst(x, None, tm) => write!(f, "({} => {})", x, tm),
            Self::Prod(None, ty, tm) => write!(f, "({} -> {})", ty, tm),
            Self::Prod(Some(x), ty, tm) => write!(f, "({} : {} -> {})", x, ty, tm),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    // TODO: eliminate this
    ExpectedInput,
    ExpectedIdent,
    ExpectedIdentOrLPar,
    AnonymousLambda,
    AbstractionWithoutRhs,
    UnclosedLPar,
}

#[derive(Debug)]
struct App<S>(Term<S>, Vec<Term<S>>);

impl<S> App<S> {
    fn new(tm: Term<S>) -> Self {
        Self(tm, Vec::new())
    }

    fn app(mut self, tm: Term<S>) -> Self {
        self.1.push(tm);
        self
    }
}

impl<S> From<Term<S>> for App<S> {
    fn from(tm: Term<S>) -> Self {
        match tm {
            Term::Appl(head, args) => App(*head, args),
            _ => App(tm, Vec::new()),
        }
    }
}

impl<S> From<App<S>> for Term<S> {
    fn from(app: App<S>) -> Self {
        if app.1.is_empty() {
            app.0
        } else {
            Self::Appl(Box::new(app.0), app.1)
        }
    }
}

/// An application possibly preceded by an abstraction.
#[derive(Debug)]
struct ATerm<S> {
    x: Option<S>,
    app: App<S>,
}

impl<S> ATerm<S> {
    fn apply(self, binder: Option<Binder>, tm: Term<S>) -> Term<S> {
        match (self.x, binder) {
            (None, None) => Term::from(self.app.app(tm)),
            (None, Some(Binder::Prod)) => {
                Term::Bind(Box::new(TermB::Prod(None, Term::from(self.app), tm)))
            }
            (None, Some(Binder::Abst)) => panic!("anonymous abstract"),
            (Some(x), Some(binder)) => {
                let ty = Term::from(self.app);
                Term::Bind(Box::new(match binder {
                    Binder::Abst => TermB::Abst(x, Some(ty), tm),
                    Binder::Prod => TermB::Prod(Some(x), ty, tm),
                }))
            }
            (Some(_), None) => panic!("close abstraction without binder"),
        }
    }
}

impl<S> TryFrom<ATerm<S>> for App<S> {
    type Error = Error;
    fn try_from(atm: ATerm<S>) -> Result<Self, Self::Error> {
        match atm.x {
            None => Ok(atm.app),
            Some(_) => Err(Error::AbstractionWithoutRhs),
        }
    }
}

/// A left parenthesis possibly preceded by an abstraction and/or an application.
#[derive(Debug)]
struct LPar<S> {
    x: Option<S>,
    app: Option<App<S>>,
}

impl<S> LPar<S> {
    fn app(self, tm: Term<S>) -> ATerm<S> {
        let app = match self.app {
            None => App::from(tm),
            Some(app) => app.app(tm),
        };
        ATerm { x: self.x, app }
    }
}

impl<S> From<ATerm<S>> for LPar<S> {
    fn from(at: ATerm<S>) -> Self {
        Self {
            x: at.x,
            app: Some(at.app),
        }
    }
}

#[derive(Debug)]
enum Binder {
    Prod,
    Abst,
}

// TODO: this is for a future parser that can resume from incomplete input
#[allow(dead_code)]
#[derive(Debug)]
enum State<S> {
    /// nothing
    Init,
    /// `s`
    Symb(S),
    /// `s :`
    VarOf(S),

    Cont(Cont<S>),
}

#[derive(Debug)]
enum Cont<S> {
    /// `x =>`
    VarArrow(S),
    /// possibly `x :`,
    /// followed by `t1 .. tn`,
    /// possibly followed by `=>` or `->`
    ATerm(ATerm<S>, Option<Binder>),
    /// possibly `x :`,
    /// possibly followed by `t1 .. tn`,
    /// followed by `(`
    LPar(LPar<S>),
}

#[derive(Debug)]
pub struct Stack<S>(Vec<Cont<S>>);

impl<S> Stack<S> {
    fn push(&mut self, x: Cont<S>) {
        self.0.push(x)
    }
}

impl<S> Default for Stack<S> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<S> Term<S> {
    fn reduce(mut self, stack: &mut Stack<S>) -> (Option<LPar<S>>, Self) {
        while let Some(cur) = stack.0.pop() {
            match cur {
                Cont::ATerm(atm, binder) => self = atm.apply(binder, self),
                Cont::VarArrow(x) => self = Term::Bind(Box::new(TermB::Abst(x, None, self))),
                Cont::LPar(lpar) => return (Some(lpar), self),
            }
        }
        (None, self)
    }
}

type OToken<S> = Option<Token<S>>;

fn post_dot<S, I>(mut s: S, iter: &mut I) -> Result<(Vec<S>, S, OToken<S>), Error>
where
    I: Iterator<Item = Token<S>>,
{
    let mut path = Vec::new();
    while let Some(tok) = iter.next() {
        match tok {
            Token::Ident(s2) => {
                path.push(s);
                s = s2;
                match iter.next() {
                    Some(Token::Dot) => continue,
                    tok => return Ok((path, s, tok)),
                }
            }
            _ => break,
        }
    }
    Err(Error::ExpectedIdent)
}

enum ATm<S> {
    Term(Cont<S>),
    ATm(ATerm<S>, OToken<S>),
}

impl<S> ATerm<S> {
    fn parse<I>(
        mut self,
        stack: &mut Stack<S>,
        mut token: OToken<S>,
        iter: &mut I,
    ) -> Result<ATm<S>, Error>
    where
        I: Iterator<Item = Token<S>>,
    {
        while let Some(tok) = token.take() {
            match tok {
                Token::Ident(s) => match iter.next() {
                    None => return Err(Error::ExpectedInput),
                    Some(Token::Dot) => {
                        let (path, name, tok) = post_dot(s, iter)?;
                        self.app.1.push(Term::Symb(path, name));
                        token = tok;
                        continue;
                    }
                    Some(other) => {
                        self.app.1.push(Term::Symb(Vec::new(), s));
                        token = Some(other);
                        continue;
                    }
                },
                Token::Arrow => return Ok(ATm::Term(Cont::ATerm(self, Some(Binder::Prod)))),
                Token::FatArrow if self.x.is_none() => return Err(Error::AnonymousLambda),
                Token::FatArrow => return Ok(ATm::Term(Cont::ATerm(self, Some(Binder::Abst)))),
                Token::LPar => return Ok(ATm::Term(Cont::LPar(LPar::from(self)))),
                Token::RPar => match Term::from(App::try_from(self)?).reduce(stack) {
                    // if we found a matching left parenthesis
                    (Some(lpar), tm) => self = lpar.app(tm),
                    (None, tm) => {
                        let app = App::from(tm);
                        return Ok(ATm::ATm(ATerm { x: None, app }, Some(Token::RPar)));
                    }
                },
                other => return Ok(ATm::ATm(self, Some(other))),
            }
            token = iter.next();
        }
        Ok(ATm::ATm(self, None))
    }

    fn finish(self, stack: &mut Stack<S>, token: OToken<S>) -> Result<(Term<S>, OToken<S>), Error> {
        match token {
            None => Err(Error::ExpectedInput),
            Some(token) => match Term::from(App::try_from(self)?).reduce(stack) {
                (Some(_lpar), _) => Err(Error::UnclosedLPar),
                (None, tm) => Ok((tm, Some(token))),
            },
        }
    }
}

impl<S> Term<S> {
    pub fn parse2<I>(
        stack: &mut Stack<S>,
        mut token: OToken<S>,
        iter: &mut I,
    ) -> Result<(Self, OToken<S>), Error>
    where
        I: Iterator<Item = Token<S>>,
    {
        while let Some(tok) = token.take() {
            match tok {
                Token::Ident(s1) => match iter.next() {
                    None => return Err(Error::ExpectedInput),
                    Some(Token::Arrow) => {
                        let app = App::new(Term::Symb(Vec::new(), s1));
                        stack.push(Cont::ATerm(ATerm { x: None, app }, Some(Binder::Prod)))
                    }
                    Some(Token::FatArrow) => stack.push(Cont::VarArrow(s1)),
                    Some(Token::Colon) => match iter.next() {
                        None => return Err(Error::ExpectedInput),
                        Some(Token::Ident(s2)) => {
                            let (app, tok) = match iter.next() {
                                None => return Err(Error::ExpectedInput),
                                Some(Token::Dot) => {
                                    let (path, name, tok) = post_dot(s2, iter)?;
                                    (App::new(Term::Symb(path, name)), tok)
                                }
                                Some(tok) => (App::new(Term::Symb(Vec::new(), s2)), Some(tok)),
                            };
                            match (ATerm { x: Some(s1), app }).parse(stack, tok, iter)? {
                                ATm::ATm(atm, token) => return atm.finish(stack, token),
                                ATm::Term(ct) => stack.push(ct),
                            }
                        }
                        Some(Token::LPar) => stack.push(Cont::LPar(LPar {
                            x: Some(s1),
                            app: None,
                        })),
                        Some(_) => return Err(Error::ExpectedIdentOrLPar),
                    },
                    Some(Token::Dot) => {
                        let (path, name, tok) = post_dot(s1, iter)?;
                        let app = App::new(Term::Symb(path, name));
                        match (ATerm { x: None, app }).parse(stack, tok, iter)? {
                            ATm::ATm(atm, token) => return atm.finish(stack, token),
                            ATm::Term(ct) => stack.push(ct),
                        }
                    }
                    Some(Token::Ident(s2)) => {
                        let app = App::new(Term::Symb(Vec::new(), s1));
                        match (ATerm { x: None, app }).parse(stack, Some(Token::Ident(s2)), iter)? {
                            ATm::ATm(atm, token) => return atm.finish(stack, token),
                            ATm::Term(ct) => stack.push(ct),
                        }
                    }
                    Some(Token::LPar) => {
                        let app = Some(App::new(Term::Symb(Vec::new(), s1)));
                        stack.push(Cont::LPar(LPar { x: None, app }));
                    }
                    Some(Token::RPar) => {
                        let app = App::new(Term::Symb(Vec::new(), s1));
                        match (ATerm { x: None, app }).parse(stack, Some(Token::RPar), iter)? {
                            ATm::ATm(atm, token) => return atm.finish(stack, token),
                            ATm::Term(ct) => stack.push(ct),
                        }
                    }
                    Some(other) => match Term::Symb(Vec::new(), s1).reduce(stack) {
                        (Some(_lpar), _) => return Err(Error::UnclosedLPar),
                        (None, tm) => return Ok((tm, Some(other))),
                    },
                },
                Token::LPar => stack.push(Cont::LPar(LPar { x: None, app: None })),
                _ => return Err(Error::ExpectedIdentOrLPar),
            }
            token = iter.next()
        }
        Err(Error::ExpectedInput)
    }
}

impl<'s> Term<&'s str> {
    pub fn parse_str(s: &'s str) -> Result<Self, Error> {
        let mut stack = Stack::default();
        let mut iter = crate::lex(s).chain(core::iter::once(Token::Period));
        let (tm, tok) = Self::parse2(&mut stack, iter.next(), &mut iter)?;
        assert_eq!(iter.next(), None);
        assert_eq!(tok, Some(Token::Period));
        Ok(tm)
    }
}

#[test]
fn positive() -> Result<(), Error> {
    Term::parse_str("x => y : a => z")?;
    Term::parse_str("a -> x : b -> c")?;
    Term::parse_str("(a b) (c d) e")?;
    Term::parse_str("a (b c) (d e)")?;
    Term::parse_str("((a (((b)))))")?;
    Term::parse_str("m1.a m2.b")?;
    Ok(())
}

#[test]
fn negative() {
    use Error::*;
    assert_eq!(Term::parse_str("->").unwrap_err(), ExpectedIdentOrLPar);
    assert_eq!(Term::parse_str("x : ->").unwrap_err(), ExpectedIdentOrLPar);
    assert_eq!(Term::parse_str("(a ").unwrap_err(), UnclosedLPar);
    assert_eq!(Term::parse_str("(a b ").unwrap_err(), UnclosedLPar);
    assert_eq!(Term::parse_str("a b => c").unwrap_err(), AnonymousLambda);
    assert_eq!(Term::parse_str("m1.m2. -> x").unwrap_err(), ExpectedIdent);
}
