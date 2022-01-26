use crate::Token;
use alloc::{boxed::Box, vec::Vec};
use core::convert::TryFrom;
use core::fmt::{self, Display};

#[derive(Clone, Debug)]
pub enum Term<S> {
    // Symbol name, preceded by module path
    Symb(Vec<S>, S),
    BVar(usize),
    // Application
    Appl(Box<Self>, Vec<Self>),
    Bind(Box<Bind<S, Term<S>>>),
}

#[derive(Clone, Debug)]
pub enum Bind<V, Tm> {
    // Abstraction (`x : A => t`)
    Abst(V, Option<Tm>, Tm),
    // Dependent product (`x : A -> t`)
    Prod(Option<V>, Tm, Tm),
}

impl<S> Term<S> {
    pub fn bind(tm: Bind<S, Self>) -> Self {
        Self::Bind(Box::new(tm))
    }
}

impl<S: Display> Display for Term<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Symb(path, s) => {
                path.iter().try_for_each(|p| write!(f, "{}.", p))?;
                s.fmt(f)
            }
            Self::BVar(x) => write!(f, "β{}", x),
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

impl<C: Display, Tm: Display> Display for Bind<C, Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    ExpectedIdent,
    ExpectedIdentOrLPar,
    AnonymousLambda,
    AbstractionWithoutRhs,
    UnclosedLPar,
}

type Result<T> = core::result::Result<T, Error>;

#[derive(Debug)]
struct App<Tm>(Tm, Vec<Tm>);

impl<Tm> App<Tm> {
    fn new(tm: Tm) -> Self {
        Self(tm, Vec::new())
    }

    fn app(mut self, tm: Tm) -> Self {
        self.1.push(tm);
        self
    }
}

impl<S> From<Term<S>> for App<Term<S>> {
    fn from(tm: Term<S>) -> Self {
        match tm {
            Term::Appl(head, args) => App(*head, args),
            _ => App(tm, Vec::new()),
        }
    }
}

impl<S> From<App<Term<S>>> for Term<S> {
    fn from(app: App<Term<S>>) -> Self {
        if app.1.is_empty() {
            app.0
        } else {
            Self::Appl(Box::new(app.0), app.1)
        }
    }
}

/// An application possibly preceded by an abstraction.
#[derive(Debug)]
pub(crate) struct ATerm<S> {
    x: Option<S>,
    app: App<Term<S>>,
}

impl<S> TryFrom<ATerm<S>> for App<Term<S>> {
    type Error = Error;
    fn try_from(atm: ATerm<S>) -> Result<Self> {
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
    app: Option<App<Term<S>>>,
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
pub(crate) enum State<S> {
    /// nothing
    Init,
    /// `s`
    Symb(S),
    /// `s :`
    VarOf(S),

    ATerm(ATerm<S>),

    Term(Term<S>, Token<()>),
}

impl<S> Default for State<S> {
    fn default() -> Self {
        Self::Init
    }
}

#[derive(Debug)]
enum Cont<S> {
    /// `x`, possibly followed by `: ty`, followed by `=>`,
    Abst(S, Option<Term<S>>),
    /// possibly `x :`, followed by `ty ->`,
    Prod(Option<S>, Term<S>),

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
                Cont::Abst(x, ty) => self = Term::bind(Bind::Abst(x, ty, self)),
                Cont::Prod(x, ty) => self = Term::bind(Bind::Prod(x, ty, self)),
                Cont::LPar(lpar) => return (Some(lpar), self),
            }
        }
        (None, self)
    }
}

type OToken<S> = Option<Token<S>>;

fn post_dot<S, I>(mut s: S, iter: &mut I) -> Result<(Vec<S>, S, OToken<S>)>
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

impl<S> ATerm<S> {
    fn parse<I>(
        mut self,
        stack: &mut Stack<S>,
        mut token: OToken<S>,
        iter: &mut I,
    ) -> Result<Loop<State<S>>>
    where
        I: Iterator<Item = Token<S>>,
    {
        while let Some(tok) = token.take() {
            match tok {
                Token::Ident(s) => match iter.next() {
                    None => self.app.1.push(Term::Symb(Vec::new(), s)),
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
                Token::Arrow => {
                    stack.push(Cont::Prod(self.x, Term::from(self.app)));
                    return Ok(Loop::Continue);
                }
                Token::FatArrow => match self.x {
                    None => return Err(Error::AnonymousLambda),
                    Some(x) => {
                        stack.push(Cont::Abst(x, Some(Term::from(self.app))));
                        return Ok(Loop::Continue);
                    }
                },
                Token::LPar => {
                    stack.push(Cont::LPar(LPar::from(self)));
                    return Ok(Loop::Continue);
                }
                Token::RPar => match Term::from(App::try_from(self)?).reduce(stack) {
                    // if we found a matching left parenthesis
                    (Some(lpar), tm) => self = lpar.app(tm),
                    (None, tm) => {
                        let app = App::from(tm);
                        return Ok(Loop::Return(
                            ATerm { x: None, app }.finish(stack, Token::RPar)?,
                        ));
                    }
                },
                tok => return Ok(Loop::Return(self.finish(stack, tok.map(|_| ()))?)),
            }
            token = iter.next();
        }
        Ok(Loop::Return(State::ATerm(self)))
    }

    fn finish(self, stack: &mut Stack<S>, token: Token<()>) -> Result<State<S>> {
        match Term::from(App::try_from(self)?).reduce(stack) {
            (Some(_lpar), _) => Err(Error::UnclosedLPar),
            (None, tm) => Ok(State::Term(tm, token)),
        }
    }
}

enum Loop<T> {
    Return(T),
    Continue,
}

impl<S> State<S> {
    pub fn parse<I>(self, stack: &mut Stack<S>, iter: &mut I) -> Result<Self>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self {
            Self::Init => (),
            Self::Symb(s1) => match Self::ident(s1, stack, iter)? {
                Loop::Continue => (),
                Loop::Return(ret) => return Ok(ret),
            },
            Self::VarOf(s1) => match Self::varof(s1, stack, iter)? {
                Loop::Continue => (),
                Loop::Return(ret) => return Ok(ret),
            },
            Self::ATerm(atm) => match atm.parse(stack, iter.next(), iter)? {
                Loop::Continue => (),
                Loop::Return(ret) => return Ok(ret),
            },
            Self::Term(_, _) => return Ok(self),
        }

        while let Some(token) = iter.next() {
            match token {
                Token::Ident(s1) => match Self::ident(s1, stack, iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(ret),
                },
                Token::LPar => stack.push(Cont::LPar(LPar { x: None, app: None })),
                _ => return Err(Error::ExpectedIdentOrLPar),
            }
        }
        Ok(Self::Init)
    }

    fn ident<I>(s1: S, stack: &mut Stack<S>, iter: &mut I) -> Result<Loop<Self>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match iter.next() {
            None => return Ok(Loop::Return(Self::Symb(s1))),
            Some(Token::Arrow) => stack.push(Cont::Prod(None, Term::Symb(Vec::new(), s1))),
            Some(Token::FatArrow) => stack.push(Cont::Abst(s1, None)),
            Some(Token::Colon) => match Self::varof(s1, stack, iter)? {
                Loop::Continue => (),
                Loop::Return(ret) => return Ok(Loop::Return(ret)),
            },
            Some(Token::Dot) => {
                let (path, name, tok) = post_dot(s1, iter)?;
                let app = App::new(Term::Symb(path, name));
                match (ATerm { x: None, app }).parse(stack, tok, iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(Token::Ident(s2)) => {
                let app = App::new(Term::Symb(Vec::new(), s1));
                match (ATerm { x: None, app }).parse(stack, Some(Token::Ident(s2)), iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(Token::LPar) => {
                let app = Some(App::new(Term::Symb(Vec::new(), s1)));
                stack.push(Cont::LPar(LPar { x: None, app }));
            }
            Some(Token::RPar) => {
                let app = App::new(Term::Symb(Vec::new(), s1));
                match (ATerm { x: None, app }).parse(stack, Some(Token::RPar), iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(other) => match Term::Symb(Vec::new(), s1).reduce(stack) {
                (Some(_lpar), _) => return Err(Error::UnclosedLPar),
                (None, tm) => return Ok(Loop::Return(Self::Term(tm, other.map(|_| ())))),
            },
        }
        Ok(Loop::Continue)
    }

    fn varof<I>(s1: S, stack: &mut Stack<S>, iter: &mut I) -> Result<Loop<Self>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match iter.next() {
            None => Ok(Loop::Return(Self::VarOf(s1))),
            Some(Token::Ident(s2)) => {
                let (app, tok) = match iter.next() {
                    Some(Token::Dot) => {
                        let (path, name, tok) = post_dot(s2, iter)?;
                        (App::new(Term::Symb(path, name)), tok)
                    }
                    tok => (App::new(Term::Symb(Vec::new(), s2)), tok),
                };
                (ATerm { x: Some(s1), app }).parse(stack, tok, iter)
            }
            Some(Token::LPar) => {
                let x = Some(s1);
                stack.push(Cont::LPar(LPar { x, app: None }));
                Ok(Loop::Continue)
            }
            Some(_) => Err(Error::ExpectedIdentOrLPar),
        }
    }
}

impl<S> Term<S> {
    pub fn parse<I>(stack: &mut Stack<S>, iter: &mut I) -> Result<(Self, Token<()>)>
    where
        I: Iterator<Item = Token<S>>,
    {
        match State::Init.parse(stack, iter)? {
            State::Init | State::VarOf(_) => Err(Error::ExpectedIdentOrLPar),
            // TODO: handle this case
            State::Symb(_) | State::ATerm(_) => panic!("expected input"),
            State::Term(tm, tok) => Ok((tm, tok)),
        }
    }
}

impl<'s> Term<&'s str> {
    pub fn parse_str(s: &'s str) -> Result<Self> {
        let mut stack = Stack::default();
        let mut iter = crate::lex(s).chain(core::iter::once(Token::Period));
        let (tm, tok) = Self::parse(&mut stack, &mut iter)?;
        assert_eq!(iter.next(), None);
        assert_eq!(tok, Token::Period);
        Ok(tm)
    }
}

#[test]
fn positive() -> Result<()> {
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
