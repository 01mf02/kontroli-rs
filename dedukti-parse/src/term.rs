use crate::Token;
use alloc::{boxed::Box, vec::Vec};
use core::borrow::Borrow;
use core::fmt::{self, Display};

#[derive(Clone, Debug)]
pub struct App<Tm>(pub Tm, pub Vec<Self>);

#[derive(Clone, Debug)]
pub enum Term1<C, V> {
    // Symbol name, preceded by module path
    Const(Vec<C>, C),
    Var(usize),
    // Abstraction (`x : A => t`)
    Abst(V, Option<Box<App<Self>>>, Box<App<Self>>),
    // Dependent product (`x : A -> t`)
    Prod(Option<V>, Box<App<Self>>, Box<App<Self>>),
}

pub type Term<C, V> = App<Term1<C, V>>;

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

impl<C: Display, V: Display> Display for Term1<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Const(path, name) => {
                path.iter().try_for_each(|p| write!(f, "{}.", p))?;
                name.fmt(f)
            }
            Self::Var(v) => v.fmt(f),
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

impl<Tm> App<Tm> {
    pub fn new(tm: Tm) -> Self {
        Self(tm, Vec::new())
    }
}

/// A left parenthesis possibly preceded by an abstraction and/or an application.
#[derive(Debug)]
struct LPar<C, V> {
    x: Option<V>,
    app: Option<Term<C, V>>,
}

#[derive(Debug)]
pub(crate) enum State<S, C, V> {
    /// nothing
    Init,
    /// `s` (we do not know at this point whether it is a variable or a constant)
    Symb(S),
    /// `s :`
    VarOf(V),
    /// possibly `x :`, followed by `t1 ... tn`.
    ATerm(Option<V>, Term<C, V>),

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
    bound: Vec<V>,
    stack: Vec<Cont<C, V>>,
}

impl<C, V> Ctx<C, V> {
    pub fn bound_mut(&mut self) -> &mut Vec<V> {
        &mut self.bound
    }

    fn find<S: Eq>(&self, s: &S) -> Option<usize>
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

impl<C, V> Default for Ctx<C, V> {
    fn default() -> Self {
        Self {
            bound: Vec::new(),
            stack: Vec::new(),
        }
    }
}

impl<C, V> Term<C, V> {
    fn reduce(mut self, ctx: &mut Ctx<C, V>) -> (Option<LPar<C, V>>, Self) {
        while let Some(cur) = ctx.stack.pop() {
            match cur {
                Cont::Abst(x, ty) => {
                    self = App::new(Term1::Abst(x, ty.map(Box::new), Box::new(self)))
                }
                Cont::Prod(x, ty) => self = App::new(Term1::Prod(x, Box::new(ty), Box::new(self))),
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
            Self::ATerm(x, app) => match Self::aterm(x, app, ctx, iter.next(), iter)? {
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
            Some(Token::Arrow) => ctx.stack.push(Cont::Prod(
                None,
                App::new(Term1::Const(Vec::new(), s1.into())),
            )),
            Some(Token::FatArrow) => ctx.stack.push(Cont::Abst(s1.into(), None)),
            Some(Token::Colon) => match Self::varof(s1.into(), ctx, iter)? {
                Loop::Continue => (),
                Loop::Return(ret) => return Ok(Loop::Return(ret)),
            },
            Some(Token::Dot) => {
                let (path, name, tok) = post_dot(s1, iter)?;
                let app = App::new(Term1::Const(path, name));
                match Self::aterm(None, app, ctx, tok, iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(Token::Ident(s2)) => {
                let app = App::new(Term1::Const(Vec::new(), s1.into()));
                match Self::aterm(None, app, ctx, Some(Token::Ident(s2)), iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(Token::LPar) => {
                let app = Some(App::new(Term1::Const(Vec::new(), s1.into())));
                ctx.stack.push(Cont::LPar(LPar { x: None, app }));
            }
            Some(Token::RPar) => {
                let app = App::new(Term1::Const(Vec::new(), s1.into()));
                match Self::aterm(None, app, ctx, Some(Token::RPar), iter)? {
                    Loop::Continue => (),
                    Loop::Return(ret) => return Ok(Loop::Return(ret)),
                }
            }
            Some(other) => match App::new(Term1::Const(Vec::new(), s1.into())).reduce(ctx) {
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
                        (Term1::Const(path, name), tok)
                    }
                    tok => (Term1::Const(Vec::new(), s2.into()), tok),
                };
                Self::aterm(Some(s1), App::new(app), ctx, tok, iter)
            }
            Some(Token::LPar) => {
                let x = Some(s1);
                ctx.stack.push(Cont::LPar(LPar { x, app: None }));
                Ok(Loop::Continue)
            }
            Some(_) => Err(Error::ExpectedIdentOrLPar),
        }
    }

    fn aterm(
        mut x: Option<V>,
        mut app: Term<C, V>,
        ctx: &mut Ctx<C, V>,
        mut token: OToken<S>,
        iter: &mut impl Iterator<Item = Token<S>>,
    ) -> Result<Loop<State<S, C, V>>> {
        while let Some(tok) = token.take() {
            match tok {
                Token::Ident(s) => match iter.next() {
                    None => app.1.push(App::new(Term1::Const(Vec::new(), s.into()))),
                    Some(Token::Dot) => {
                        let (path, name, tok) = post_dot(s, iter)?;
                        app.1.push(App::new(Term1::Const(path, name)));
                        token = tok;
                        continue;
                    }
                    Some(other) => {
                        app.1.push(App::new(Term1::Const(Vec::new(), s.into())));
                        token = Some(other);
                        continue;
                    }
                },
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
                _tok if x.is_some() => return Err(Error::AbstractionWithoutRhs),
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
            token = iter.next();
        }
        Ok(Loop::Return(State::ATerm(x, app)))
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
            State::Symb(_) | State::ATerm(..) => panic!("expected input"),
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
