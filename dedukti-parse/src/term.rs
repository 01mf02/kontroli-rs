use crate::{Symb, Token};
use alloc::{boxed::Box, vec::Vec};
use core::borrow::Borrow;
use core::fmt::{self, Display};

pub type Term<A, V> = App<Term1<A, V>>;

#[derive(Clone, Debug)]
pub struct App<Tm>(pub Tm, pub Vec<Self>);

#[derive(Clone, Debug)]
pub enum Term1<A, V> {
    Atom(A),
    // Abstraction (`x : A => t`)
    Abst(V, Option<Box<App<Self>>>, Box<App<Self>>),
    // Dependent product (`x : A -> t`)
    Prod(Option<V>, Box<App<Self>>, Box<App<Self>>),
}

#[derive(Clone, Debug)]
pub enum Atom<C> {
    Const(C),
    Var(usize),
    Type,
}

impl<Tm: Display> Display for App<Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.1.is_empty() {
            write!(f, "(")?;
        }
        self.0.fmt(f)?;
        self.1.iter().try_for_each(|a| write!(f, " {}", a))?;
        if !self.1.is_empty() {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl<C: Display> Display for Atom<C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(c) => c.fmt(f),
            Self::Var(v) => v.fmt(f),
            Self::Type => "Type".fmt(f),
        }
    }
}

impl<A: Display, V: Display> Display for Term1<A, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Atom(a) => a.fmt(f),
            Self::Abst(x, Some(ty), tm) => write!(f, "({} : {} => {})", x, ty, tm),
            Self::Abst(x, None, tm) => write!(f, "({} => {})", x, tm),
            Self::Prod(None, ty, tm) => write!(f, "({} -> {})", ty, tm),
            Self::Prod(Some(x), ty, tm) => write!(f, "({} : {} -> {})", x, ty, tm),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    ExpectedIdentOrLPar,
    AnonymousLambda,
    AbstWithoutRhs,
    UnclosedLPar,
}

pub type Result<T> = core::result::Result<T, Error>;

impl<Tm> App<Tm> {
    pub fn new(tm: Tm) -> Self {
        Self(tm, Vec::new())
    }
}

/// A left parenthesis possibly preceded by an abstraction and/or an application.
#[derive(Debug)]
struct LPar<A, V> {
    x: Option<V>,
    app: Option<Term<A, V>>,
}

#[derive(Debug)]
pub(crate) enum State<S, A, V> {
    /// nothing
    Init,
    /// `s` (we do not know at this point whether it is a variable or a constant)
    Symb(S),
    /// `s :`
    VarOf(V),
    /// possibly `x :`, followed by `t1 ... tn`.
    ATerm(Option<V>, Term<A, V>),

    Term(Term<A, V>, Token<()>),
}

impl<S, A, V> Default for State<S, A, V> {
    fn default() -> Self {
        Self::Init
    }
}

#[derive(Debug)]
enum Cont<A, V> {
    /// `x`, possibly followed by `: ty`, followed by `=>`,
    Abst(V, Option<Term<A, V>>),
    /// possibly `x :`, followed by `ty ->`,
    Prod(Option<V>, Term<A, V>),

    /// possibly `x :`,
    /// possibly followed by `t1 .. tn`,
    /// followed by `(`
    LPar(LPar<A, V>),
}

#[derive(Debug)]
pub struct Ctx<A, V> {
    bound: Vec<V>,
    stack: Vec<Cont<A, V>>,
}

impl<A, V> Ctx<A, V> {
    pub fn bound_mut(&mut self) -> &mut Vec<V> {
        &mut self.bound
    }

    pub fn find<S: Eq + ?Sized>(&self, s: &S) -> Option<usize>
    where
        V: Borrow<S>,
    {
        let mut i = 0;
        for cont in self.stack.iter().rev() {
            match cont {
                Cont::Abst(x, _) | Cont::Prod(Some(x), _) => {
                    if x.borrow() == s {
                        return Some(i);
                    } else {
                        i += 1;
                    }
                }
                Cont::Prod(None, _) => i += 1,
                Cont::LPar(_) => (),
            }
        }
        for x in self.bound.iter().rev() {
            if x.borrow() == s {
                return Some(i);
            } else {
                i += 1
            }
        }
        None
    }
}

impl<A, V> Default for Ctx<A, V> {
    fn default() -> Self {
        Self {
            bound: Vec::new(),
            stack: Vec::new(),
        }
    }
}

pub trait Scope<S, V>
where
    Self: Sized,
{
    fn scope(symb: Symb<S>, ctx: &Ctx<Self, V>) -> Self;

    fn go(symb: Symb<S>, ctx: &Ctx<Self, V>) -> Term<Self, V> {
        Term::new(Term1::Atom(Self::scope(symb, ctx)))
    }
}

impl<S, V> Scope<S, V> for Symb<S> {
    fn scope(symb: Symb<S>, _: &Ctx<Self, V>) -> Self {
        symb
    }
}

impl<S: Borrow<str> + Into<C> + Eq, C, V: Borrow<str>> Scope<S, V> for Atom<Symb<C>> {
    fn scope(symb: Symb<S>, ctx: &Ctx<Self, V>) -> Self {
        if symb.path.is_empty() {
            if let Some(v) = ctx.find(symb.name.borrow()) {
                return Atom::Var(v);
            } else if symb.name.borrow() == "Type" {
                return Atom::Type;
            }
        }
        Atom::Const(symb.map(|s| s.into()))
    }
}

impl<A, V> Term<A, V> {
    fn reduce(mut self, ctx: &mut Ctx<A, V>) -> (Option<LPar<A, V>>, Self) {
        while let Some(cur) = ctx.stack.pop() {
            match cur {
                Cont::Abst(x, ty) => self = App::new(Term1::Abst(x, ty.map(Box::new), self.into())),
                Cont::Prod(x, ty) => self = App::new(Term1::Prod(x, Box::new(ty), self.into())),
                Cont::LPar(lpar) => return (Some(lpar), self),
            }
        }
        (None, self)
    }
}

enum Loop<T> {
    Return(T),
    Continue,
}

impl<S: Into<V>, A: Scope<S, V>, V> State<S, A, V> {
    pub fn parse<I>(self, ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<Self>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self.resume(ctx, iter)? {
            Loop::Continue => Self::init(ctx, iter),
            Loop::Return(ret) => Ok(ret),
        }
    }

    fn resume<I>(self, ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<Loop<Self>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self {
            Self::Init => Ok(Loop::Continue),
            Self::Symb(s1) => Self::symb(Symb::new(s1), ctx, iter),
            Self::VarOf(v) => Self::varof(v, ctx, iter),
            Self::ATerm(x, app) => Self::aterm(x, app, ctx, iter),
            Self::Term(_, _) => Ok(Loop::Return(self)),
        }
    }

    pub fn init<I>(ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<Self>
    where
        I: Iterator<Item = Token<S>>,
    {
        while let Some(token) = iter.next() {
            match token {
                Token::Symb(s) => match Self::symb(s, ctx, iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(ret),
                },
                Token::LPar => ctx.stack.push(Cont::LPar(LPar { x: None, app: None })),
                _ => return Err(Error::ExpectedIdentOrLPar),
            }
        }
        Ok(Self::Init)
    }

    fn symb<I>(s: Symb<S>, ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<Loop<Self>>
    where
        I: Iterator<Item = Token<S>>,
    {
        if !s.path.is_empty() {
            return Self::aterm(None, A::go(s, ctx), ctx, iter);
        }
        match iter.next() {
            None => Ok(Loop::Return(Self::Symb(s.name))),
            Some(Token::FatArrow) => {
                ctx.stack.push(Cont::Abst(s.name.into(), None));
                Ok(Loop::Continue)
            }
            Some(Token::Colon) => Self::varof(s.name.into(), ctx, iter),
            Some(tok) => {
                let iter = &mut core::iter::once(tok).chain(iter);
                Self::aterm(None, A::go(s, ctx), ctx, iter)
            }
        }
    }

    fn varof<I>(v: V, ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<Loop<Self>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match iter.next() {
            None => Ok(Loop::Return(Self::VarOf(v))),
            Some(Token::Symb(s)) => Self::aterm(Some(v), A::go(s, ctx), ctx, iter),
            Some(Token::LPar) => {
                let x = Some(v);
                ctx.stack.push(Cont::LPar(LPar { x, app: None }));
                Ok(Loop::Continue)
            }
            Some(_) => Err(Error::ExpectedIdentOrLPar),
        }
    }

    fn aterm(
        mut x: Option<V>,
        mut app: Term<A, V>,
        ctx: &mut Ctx<A, V>,
        iter: &mut impl Iterator<Item = Token<S>>,
    ) -> Result<Loop<Self>> {
        for tok in iter {
            match tok {
                Token::Symb(s) => app.1.push(A::go(s, ctx)),
                Token::Arrow => {
                    ctx.stack.push(Cont::Prod(x, app));
                    return Ok(Loop::Continue);
                }
                Token::FatArrow => match x {
                    None => return Err(Error::AnonymousLambda),
                    Some(x) => {
                        ctx.stack.push(Cont::Abst(x, Some(app)));
                        return Ok(Loop::Continue);
                    }
                },
                Token::LPar => {
                    ctx.stack.push(Cont::LPar(LPar { x, app: Some(app) }));
                    return Ok(Loop::Continue);
                }
                _tok if x.is_some() => return Err(Error::AbstWithoutRhs),
                Token::RPar => match app.reduce(ctx) {
                    // if we found a matching left parenthesis
                    (Some(lpar), tm) => {
                        x = lpar.x;
                        app = match lpar.app {
                            None => tm,
                            Some(mut app) => {
                                app.1.push(tm);
                                app
                            }
                        }
                    }
                    (None, tm) => return Ok(Loop::Return(State::Term(tm, Token::RPar))),
                },
                tok => match app.reduce(ctx) {
                    (Some(_lpar), _) => return Err(Error::UnclosedLPar),
                    (None, tm) => return Ok(Loop::Return(State::Term(tm, tok.map(|_| ())))),
                },
            }
        }
        Ok(Loop::Return(State::ATerm(x, app)))
    }
}

impl<A, V> Term<A, V> {
    pub fn parse<S: Into<V>, I>(ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<(Self, Token<()>)>
    where
        I: Iterator<Item = Token<S>>,
        A: Scope<S, V>,
        V: Borrow<S>,
    {
        match State::init(ctx, iter)? {
            State::Init | State::VarOf(_) => Err(Error::ExpectedIdentOrLPar),
            // TODO: handle this case
            State::Symb(_) | State::ATerm(..) => panic!("expected input"),
            State::Term(tm, tok) => Ok((tm, tok)),
        }
    }
}

impl<'s> Term<Atom<Symb<&'s str>>, &'s str> {
    pub fn parse_str(s: &'s str) -> Result<Self> {
        use crate::Lex;
        let mut ctx = Ctx::default();
        let mut iter = Token::lexer(s).chain(core::iter::once(Token::Dot));
        let (tm, tok) = Self::parse(&mut ctx, &mut iter)?;
        assert_eq!(iter.next(), None);
        assert_eq!(tok, Token::Dot);
        Ok(tm)
    }
}

#[test]
fn positive() -> Result<()> {
    Term::parse_str("x => y : a => z")?;
    Term::parse_str("a -> x : b -> c")?;
    Term::parse_str("(a)")?;
    Term::parse_str("(a b)")?;
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
    assert_eq!(Term::parse_str("(a : b)").unwrap_err(), AbstWithoutRhs);
    assert_eq!(Term::parse_str("a : b.").unwrap_err(), AbstWithoutRhs);
}
