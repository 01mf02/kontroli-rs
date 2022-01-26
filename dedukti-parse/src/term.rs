use crate::Token;
use alloc::{boxed::Box, vec::Vec};
use core::convert::TryFrom;
use core::fmt::{self, Display};

#[derive(Clone, Debug)]
pub enum Term<C, V> {
    // Symbol name, preceded by module path
    Symb(Vec<C>, C),
    BVar(usize),
    // Application
    Appl(Box<Self>, Vec<Self>),
    Bind(Box<Bind<V, Term<C, V>>>),
}

#[derive(Clone, Debug)]
pub enum Bind<V, Tm> {
    // Abstraction (`x : A => t`)
    Abst(V, Option<Tm>, Tm),
    // Dependent product (`x : A -> t`)
    Prod(Option<V>, Tm, Tm),
}

impl<C, V> Term<C, V> {
    pub fn bind(tm: Bind<V, Self>) -> Self {
        Self::Bind(Box::new(tm))
    }
}

impl<C: Display, V: Display> Display for Term<C, V> {
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

impl<V: Display, Tm: Display> Display for Bind<V, Tm> {
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

impl<C, V> From<Term<C, V>> for App<Term<C, V>> {
    fn from(tm: Term<C, V>) -> Self {
        match tm {
            Term::Appl(head, args) => App(*head, args),
            _ => App(tm, Vec::new()),
        }
    }
}

impl<C, V> From<App<Term<C, V>>> for Term<C, V> {
    fn from(app: App<Term<C, V>>) -> Self {
        if app.1.is_empty() {
            app.0
        } else {
            Self::Appl(Box::new(app.0), app.1)
        }
    }
}

/// An application possibly preceded by an abstraction.
#[derive(Debug)]
pub(crate) struct ATerm<C, V> {
    x: Option<V>,
    app: App<Term<C, V>>,
}

impl<C, V> TryFrom<ATerm<C, V>> for App<Term<C, V>> {
    type Error = Error;
    fn try_from(atm: ATerm<C, V>) -> Result<Self> {
        match atm.x {
            None => Ok(atm.app),
            Some(_) => Err(Error::AbstractionWithoutRhs),
        }
    }
}

/// A left parenthesis possibly preceded by an abstraction and/or an application.
#[derive(Debug)]
struct LPar<C, V> {
    x: Option<V>,
    app: Option<App<Term<C, V>>>,
}

impl<C, V> LPar<C, V> {
    fn app(self, tm: Term<C, V>) -> ATerm<C, V> {
        let app = match self.app {
            None => App::from(tm),
            Some(app) => app.app(tm),
        };
        ATerm { x: self.x, app }
    }
}

impl<C, V> From<ATerm<C, V>> for LPar<C, V> {
    fn from(at: ATerm<C, V>) -> Self {
        Self {
            x: at.x,
            app: Some(at.app),
        }
    }
}

#[derive(Debug)]
pub(crate) enum State<S, C, V> {
    /// nothing
    Init,
    /// `s`
    Symb(S),
    /// `s :`
    VarOf(V),

    ATerm(ATerm<C, V>),

    Term(Term<C, V>, Token<()>),
}

impl<S, C, V> Default for State<S, C, V> {
    fn default() -> Self {
        Self::Init
    }
}

#[derive(Debug)]
enum Cont<C, V> {
    /// `x`, possibly followed by `: ty`, followed by `=>`,
    Abst(V, Option<Term<C, V>>),
    /// possibly `x :`, followed by `ty ->`,
    Prod(Option<V>, Term<C, V>),

    /// possibly `x :`,
    /// possibly followed by `t1 .. tn`,
    /// followed by `(`
    LPar(LPar<C, V>),
}

#[derive(Debug)]
pub struct Ctx<C, V> {
    stack: Vec<Cont<C, V>>,
}

impl<C, V> Default for Ctx<C, V> {
    fn default() -> Self {
        Self { stack: Vec::new() }
    }
}

impl<C, V> Term<C, V> {
    fn reduce(mut self, ctx: &mut Ctx<C, V>) -> (Option<LPar<C, V>>, Self) {
        while let Some(cur) = ctx.stack.pop() {
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

fn post_dot<S: Into<C>, C, I>(mut s: S, iter: &mut I) -> Result<(Vec<C>, C, OToken<S>)>
where
    I: Iterator<Item = Token<S>>,
{
    let mut path = Vec::new();
    while let Some(tok) = iter.next() {
        match tok {
            Token::Ident(s2) => {
                path.push(s.into());
                s = s2;
                match iter.next() {
                    Some(Token::Dot) => continue,
                    tok => return Ok((path, s.into(), tok)),
                }
            }
            _ => break,
        }
    }
    Err(Error::ExpectedIdent)
}

impl<C, V> ATerm<C, V> {
    fn parse<S: Into<C>, I>(
        mut self,
        ctx: &mut Ctx<C, V>,
        mut token: OToken<S>,
        iter: &mut I,
    ) -> Result<Loop<State<S, C, V>>>
    where
        I: Iterator<Item = Token<S>>,
    {
        while let Some(tok) = token.take() {
            match tok {
                Token::Ident(s) => match iter.next() {
                    None => self.app.1.push(Term::Symb(Vec::new(), s.into())),
                    Some(Token::Dot) => {
                        let (path, name, tok) = post_dot(s, iter)?;
                        self.app.1.push(Term::Symb(path, name));
                        token = tok;
                        continue;
                    }
                    Some(other) => {
                        self.app.1.push(Term::Symb(Vec::new(), s.into()));
                        token = Some(other);
                        continue;
                    }
                },
                Token::Arrow => {
                    ctx.stack.push(Cont::Prod(self.x, Term::from(self.app)));
                    return Ok(Loop::Continue);
                }
                Token::FatArrow => match self.x {
                    None => return Err(Error::AnonymousLambda),
                    Some(x) => {
                        ctx.stack.push(Cont::Abst(x, Some(Term::from(self.app))));
                        return Ok(Loop::Continue);
                    }
                },
                Token::LPar => {
                    ctx.stack.push(Cont::LPar(LPar::from(self)));
                    return Ok(Loop::Continue);
                }
                Token::RPar => match Term::from(App::try_from(self)?).reduce(ctx) {
                    // if we found a matching left parenthesis
                    (Some(lpar), tm) => self = lpar.app(tm),
                    (None, tm) => {
                        let app = App::from(tm);
                        let atm = ATerm { x: None, app };
                        return Ok(Loop::Return(atm.finish(ctx, Token::RPar)?));
                    }
                },
                tok => return Ok(Loop::Return(self.finish(ctx, tok.map(|_| ()))?)),
            }
            token = iter.next();
        }
        Ok(Loop::Return(State::ATerm(self)))
    }

    fn finish<S>(self, ctx: &mut Ctx<C, V>, token: Token<()>) -> Result<State<S, C, V>> {
        match Term::from(App::try_from(self)?).reduce(ctx) {
            (Some(_lpar), _) => Err(Error::UnclosedLPar),
            (None, tm) => Ok(State::Term(tm, token)),
        }
    }
}

enum Loop<T> {
    Return(T),
    Continue,
}

impl<S: Into<C> + Into<V>, C, V> State<S, C, V> {
    pub fn parse<I>(self, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Self>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self {
            Self::Init => (),
            Self::Symb(s1) => match Self::ident(s1, ctx, iter)? {
                Loop::Continue => (),
                Loop::Return(ret) => return Ok(ret),
            },
            Self::VarOf(s1) => match Self::varof(s1, ctx, iter)? {
                Loop::Continue => (),
                Loop::Return(ret) => return Ok(ret),
            },
            Self::ATerm(atm) => match atm.parse(ctx, iter.next(), iter)? {
                Loop::Continue => (),
                Loop::Return(ret) => return Ok(ret),
            },
            Self::Term(_, _) => return Ok(self),
        }

        while let Some(token) = iter.next() {
            match token {
                Token::Ident(s1) => match Self::ident(s1, ctx, iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(ret),
                },
                Token::LPar => ctx.stack.push(Cont::LPar(LPar { x: None, app: None })),
                _ => return Err(Error::ExpectedIdentOrLPar),
            }
        }
        Ok(Self::Init)
    }

    fn ident<I>(s1: S, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Loop<Self>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match iter.next() {
            None => return Ok(Loop::Return(Self::Symb(s1))),
            Some(Token::Arrow) => ctx
                .stack
                .push(Cont::Prod(None, Term::Symb(Vec::new(), s1.into()))),
            Some(Token::FatArrow) => ctx.stack.push(Cont::Abst(s1.into(), None)),
            Some(Token::Colon) => match Self::varof(s1.into(), ctx, iter)? {
                Loop::Continue => (),
                Loop::Return(ret) => return Ok(Loop::Return(ret)),
            },
            Some(Token::Dot) => {
                let (path, name, tok) = post_dot(s1, iter)?;
                let app = App::new(Term::Symb(path, name));
                match (ATerm { x: None, app }).parse(ctx, tok, iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(Token::Ident(s2)) => {
                let app = App::new(Term::Symb(Vec::new(), s1.into()));
                match (ATerm { x: None, app }).parse(ctx, Some(Token::Ident(s2)), iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(Token::LPar) => {
                let app = Some(App::new(Term::Symb(Vec::new(), s1.into())));
                ctx.stack.push(Cont::LPar(LPar { x: None, app }));
            }
            Some(Token::RPar) => {
                let app = App::new(Term::Symb(Vec::new(), s1.into()));
                match (ATerm { x: None, app }).parse(ctx, Some(Token::RPar), iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(other) => match Term::Symb(Vec::new(), s1.into()).reduce(ctx) {
                (Some(_lpar), _) => return Err(Error::UnclosedLPar),
                (None, tm) => return Ok(Loop::Return(Self::Term(tm, other.map(|_| ())))),
            },
        }
        Ok(Loop::Continue)
    }

    fn varof<I>(s1: V, ctx: &mut Ctx<C, V>, iter: &mut I) -> Result<Loop<Self>>
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
                    tok => (App::new(Term::Symb(Vec::new(), s2.into())), tok),
                };
                (ATerm { x: Some(s1), app }).parse(ctx, tok, iter)
            }
            Some(Token::LPar) => {
                let x = Some(s1);
                ctx.stack.push(Cont::LPar(LPar { x, app: None }));
                Ok(Loop::Continue)
            }
            Some(_) => Err(Error::ExpectedIdentOrLPar),
        }
    }
}

impl<C, V> Term<C, V> {
    pub fn parse<S: Into<C> + Into<V>, I>(
        ctx: &mut Ctx<C, V>,
        iter: &mut I,
    ) -> Result<(Self, Token<()>)>
    where
        I: Iterator<Item = Token<S>>,
    {
        match State::Init.parse(ctx, iter)? {
            State::Init | State::VarOf(_) => Err(Error::ExpectedIdentOrLPar),
            // TODO: handle this case
            State::Symb(_) | State::ATerm(_) => panic!("expected input"),
            State::Term(tm, tok) => Ok((tm, tok)),
        }
    }
}

impl<'s> Term<&'s str, &'s str> {
    pub fn parse_str(s: &'s str) -> Result<Self> {
        let mut ctx = Ctx::default();
        let mut iter = crate::lex(s).chain(core::iter::once(Token::Period));
        let (tm, tok) = Self::parse(&mut ctx, &mut iter)?;
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
