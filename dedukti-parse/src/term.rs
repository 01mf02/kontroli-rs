use crate::{Symb, Token};
use alloc::{boxed::Box, vec::Vec};
use core::borrow::Borrow;
use core::fmt::{self, Display};

pub type Term<A, V> = App<AppH<A, V>>;

/// Application of applications to a head.
#[derive(Clone, Debug)]
pub struct App<H>(pub H, pub Vec<Self>);

/// Head of an application.
#[derive(Clone, Debug)]
pub enum AppH<A, V> {
    /// atom (constants, variables)
    Atom(A),
    /// abstraction (`x : A => t`)
    Abst(V, Option<Box<App<Self>>>, Box<App<Self>>),
    /// dependent product (`x : A -> t`)
    Prod(Option<V>, Box<App<Self>>, Box<App<Self>>),
}

/// Term without subterms.
#[derive(Clone, Debug)]
pub enum Atom<C> {
    Const(C),
    Var(usize),
    Type,
}

impl<H: Display> Display for App<H> {
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

impl<A: Display, V: Display> Display for AppH<A, V> {
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

impl<H> App<H> {
    /// Create an application with a head and no applied arguments.
    pub fn new(head: H) -> Self {
        Self(head, Vec::new())
    }
}

/// A left parenthesis possibly preceded by an abstraction and/or an application.
#[derive(Debug)]
struct LPar<A, V> {
    x: Option<V>,
    app: Option<Term<A, V>>,
}

/// State of the term parser.
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
}

impl<S, A, V> State<S, A, V> {
    pub fn map_symb<T>(self, f: impl FnOnce(S) -> T) -> State<T, A, V> {
        match self {
            Self::Init => State::Init,
            Self::Symb(s) => State::Symb(f(s)),
            Self::VarOf(v) => State::VarOf(v),
            Self::ATerm(v, tm) => State::ATerm(v, tm),
        }
    }
}

impl<S, A, V> Default for State<S, A, V> {
    fn default() -> Self {
        Self::Init
    }
}

/// Unfinished term that surrounds the currently parsed term.
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
    /// variables that were bound outside the term
    bound: Vec<V>,
    stack: Vec<Cont<A, V>>,
}

impl<A, V> Ctx<A, V> {
    pub fn bound_mut(&mut self) -> &mut Vec<V> {
        &mut self.bound
    }

    /// If a symbol is bound in a superterm, return its Bruijn variable.
    ///
    /// This implementation is not particularly efficient,
    /// because it traverses all superterms in the worst case,
    /// including superterms that can not even bind variables.
    /// However, because we commit to not cloning symbols,
    /// this is one of the best things we can do.
    ///
    /// Previously, I kept all bound variables in `bound`,
    /// removing them from the stack.
    /// However, when we reduce the stack, we need to obtain the variables,
    /// and this is quite error-prone and ugly.
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

/// Convert a symbol to a given type of atoms.
pub trait Scope<S, V>
where
    Self: Sized,
{
    /// Convert a symbol to a given type of atoms.
    fn scope(symb: Symb<S>, ctx: &Ctx<Self, V>) -> Self;

    /// Convenience function to convert a symbol to a term.
    fn go(symb: Symb<S>, ctx: &Ctx<Self, V>) -> Term<Self, V> {
        Term::new(AppH::Atom(Self::scope(symb, ctx)))
    }
}

/// The identity scoper.
impl<S, V> Scope<S, V> for Symb<S> {
    fn scope(symb: Symb<S>, _: &Ctx<Self, V>) -> Self {
        symb
    }
}

/// Distinguish symbols into constants and variables.
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
                Cont::Abst(x, ty) => self = App::new(AppH::Abst(x, ty.map(Box::new), self.into())),
                Cont::Prod(x, ty) => self = App::new(AppH::Prod(x, Box::new(ty), self.into())),
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

type OTok<S> = Option<Token<S>>;

impl<S: Into<V>, A: Scope<S, V>, V> State<S, A, V> {
    pub fn parse<I>(self, ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<(Self, OTok<S>)>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self.resume(ctx, iter)? {
            Loop::Continue => Self::init(ctx, iter),
            Loop::Return(ret) => Ok(ret),
        }
    }

    fn resume<I>(self, ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<Loop<(Self, OTok<S>)>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match self {
            Self::Init => Ok(Loop::Continue),
            Self::Symb(s1) => Self::symb(Symb::new(s1), ctx, iter),
            Self::VarOf(v) => Self::varof(v, ctx, iter),
            Self::ATerm(x, app) => Self::aterm(x, app, ctx, iter),
        }
    }

    pub fn init<I>(ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<(Self, OTok<S>)>
    where
        I: Iterator<Item = Token<S>>,
    {
        loop {
            match iter.next() {
                tok @ (None | Some(Token::Comment(_))) => return Ok((Self::Init, tok)),
                Some(Token::Symb(s)) => match Self::symb(s, ctx, iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(ret),
                },
                Some(Token::LPar) => ctx.stack.push(Cont::LPar(LPar { x: None, app: None })),
                _ => return Err(Error::ExpectedIdentOrLPar),
            }
        }
    }

    fn symb<I>(s: Symb<S>, ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<Loop<(Self, OTok<S>)>>
    where
        I: Iterator<Item = Token<S>>,
    {
        if !s.path.is_empty() {
            return Self::aterm(None, A::go(s, ctx), ctx, iter);
        }
        match iter.next() {
            tok @ (None | Some(Token::Comment(_))) => Ok(Loop::Return((Self::Symb(s.name), tok))),
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

    fn varof<I>(v: V, ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<Loop<(Self, OTok<S>)>>
    where
        I: Iterator<Item = Token<S>>,
    {
        match iter.next() {
            tok @ (None | Some(Token::Comment(_))) => Ok(Loop::Return((Self::VarOf(v), tok))),
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
    ) -> Result<Loop<(Self, OTok<S>)>> {
        loop {
            match iter.next() {
                tok @ (None | Some(Token::Comment(_))) => {
                    return Ok(Loop::Return((State::ATerm(x, app), tok)))
                }
                Some(Token::Symb(s)) => app.1.push(A::go(s, ctx)),
                Some(Token::Arrow) => {
                    ctx.stack.push(Cont::Prod(x, app));
                    return Ok(Loop::Continue);
                }
                Some(Token::FatArrow) => match x {
                    None => return Err(Error::AnonymousLambda),
                    Some(x) => {
                        ctx.stack.push(Cont::Abst(x, Some(app)));
                        return Ok(Loop::Continue);
                    }
                },
                Some(Token::LPar) => {
                    ctx.stack.push(Cont::LPar(LPar { x, app: Some(app) }));
                    return Ok(Loop::Continue);
                }
                Some(_) if x.is_some() => return Err(Error::AbstWithoutRhs),
                Some(tok) => match app.reduce(ctx) {
                    // if we found a matching left parenthesis
                    (Some(lpar), tm) if matches!(tok, Token::RPar) => {
                        x = lpar.x;
                        app = match lpar.app {
                            None => tm,
                            Some(mut app) => {
                                app.1.push(tm);
                                app
                            }
                        }
                    }
                    (Some(_lpar), _) => return Err(Error::UnclosedLPar),
                    (None, tm) => return Ok(Loop::Return((State::ATerm(x, tm), Some(tok)))),
                },
            }
        }
    }
}

impl<A, V> Term<A, V> {
    pub fn parse<S: Into<V>, I>(ctx: &mut Ctx<A, V>, iter: &mut I) -> Result<(Self, Token<S>)>
    where
        I: Iterator<Item = Token<S>>,
        A: Scope<S, V>,
        V: Borrow<S>,
    {
        match State::init(ctx, iter)? {
            (State::ATerm(None, tm), Some(tok)) => Ok((tm, tok)),
            (State::Init | State::VarOf(_), _) => Err(Error::ExpectedIdentOrLPar),
            (State::Symb(_) | State::ATerm(..), _) => panic!("expected input"),
        }
    }
}

impl<'s> Term<Atom<Symb<&'s str>>, &'s str> {
    pub fn parse_str(s: &'s str) -> Result<Self> {
        use logos::Logos;
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
