use crate::Token;
use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Command of a Dedukti theory.
#[derive(Clone, Debug)]
pub enum Command<C, V, Tm> {
    // Introduce a new symbol with arguments
    Intro(C, Vec<(V, Tm)>, Intro<Tm>),
    // Add rewrite rules
    Rules(Vec<Rule<V, Tm>>),
}

/// A command that introduces a new constant.
#[derive(Clone, Debug)]
pub enum Intro<Ty, Tm = Ty> {
    Definition(Option<Ty>, Option<Tm>),
    Theorem(Ty, Tm),
    Declaration(Ty),
}

/// A command that introduces a set of rewrite rules.
#[derive(Clone, Debug)]
pub struct Rule<V, Tm> {
    /// context (bound variables)
    pub ctx: Vec<(V, Option<Tm>)>,
    /// left-hand side (pattern to match with)
    pub lhs: Tm,
    /// right-hand side (term to replace with)
    pub rhs: Tm,
}

impl<C, V, Tm> Command<C, V, Tm> {
    /// Apply a function to the constant introduced by an introduction.
    pub fn map_const<C2>(self, f: impl FnOnce(C) -> C2) -> Command<C2, V, Tm> {
        match self {
            Self::Intro(x, args, it) => Command::Intro(f(x), args, it),
            Self::Rules(rules) => Command::Rules(rules),
        }
    }
}

impl<Ty, Tm> Intro<Ty, Tm> {
    /// Only constants introduced by definitions are rewritable.
    pub fn rewritable(&self) -> bool {
        match self {
            Self::Definition(..) => true,
            Self::Declaration(_) | Self::Theorem(..) => false,
        }
    }

    /// Apply a function to the type of the introduced constant, if given.
    pub fn map_type<U>(self, f: impl FnOnce(Ty) -> U) -> Intro<U, Tm> {
        match self {
            Self::Definition(ty, tm) => Intro::Definition(ty.map(f), tm),
            Self::Theorem(ty, tm) => Intro::Theorem(f(ty), tm),
            Self::Declaration(ty) => Intro::Declaration(f(ty)),
        }
    }

    /// Apply a function to the term of the introduced constant, if given.
    pub fn map_term<U>(self, f: impl FnOnce(Tm) -> U) -> Intro<Ty, U> {
        match self {
            Self::Definition(ty, tm) => Intro::Definition(ty, tm.map(f)),
            Self::Theorem(ty, tm) => Intro::Theorem(ty, f(tm)),
            Self::Declaration(ty) => Intro::Declaration(ty),
        }
    }
}

impl<C: Display, V: Display, Tm: Display> Display for Command<C, V, Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Intro(x, args, it) => {
                match it {
                    Intro::Theorem(_, _) => write!(f, "thm ")?,
                    Intro::Definition(_, _) => write!(f, "def ")?,
                    Intro::Declaration(_) => (),
                };
                write!(f, "{}", x)?;
                args.iter()
                    .try_for_each(|(x, ty)| write!(f, " ({} : {})", x, ty))?;
                match it {
                    Intro::Definition(ty, tm) => {
                        ty.iter().try_for_each(|ty| write!(f, " : {}", ty))?;
                        tm.iter().try_for_each(|tm| write!(f, " := {}", tm))
                    }
                    Intro::Theorem(ty, tm) => write!(f, " : {} := {}", ty, tm),
                    Intro::Declaration(ty) => write!(f, " : {}", ty),
                }
            }
            Self::Rules(rules) => rules.iter().try_for_each(|rule| rule.fmt(f)),
        }
    }
}

impl<V: Display, Tm: Display> Display for Rule<V, Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        let mut ctx = self.ctx.iter().peekable();
        while let Some((x, ty)) = ctx.next() {
            write!(f, "{}", x)?;
            ty.iter().try_for_each(|ty| write!(f, ": {}", ty))?;
            ctx.peek().iter().try_for_each(|_| write!(f, ", "))?;
        }
        write!(f, "] {} --> {}", self.lhs, self.rhs)
    }
}

#[derive(Debug)]
pub(crate) enum DefThm {
    Def,
    Thm,
}

#[derive(Debug)]
pub(crate) enum OfEq {
    Of,
    Eq,
}

#[derive(Debug)]
pub struct RuleCtx<V, Tm> {
    rules: Vec<Rule<V, Tm>>,
    vars: Vec<Option<Tm>>,
}

impl<V, Tm> Default for RuleCtx<V, Tm> {
    fn default() -> Self {
        Self {
            rules: Default::default(),
            vars: Default::default(),
        }
    }
}

impl<V, Tm> RuleCtx<V, Tm> {
    fn add(mut self, bound: &mut Vec<V>, lhs: Tm, rhs: Tm) -> Self {
        let ctx = core::mem::take(&mut self.vars);
        let ctx = bound.drain(..).zip(ctx).collect();
        self.rules.push(Rule { ctx, lhs, rhs });
        self
    }
}

/// Error occurring during the parsing of a command.
#[derive(Debug, PartialEq)]
pub enum Error {
    ExpectedColon,
    ExpectedColonEq,
    ExpectedColonOrColonEq,
    ExpectedLongArrow,
    ExpectedIdent,
    ExpectedCommaOrRBrk,
    ExpectedRPar,
    ExpectedCmd,
    UnexpectedPath,
    UnexpectedToken,
}

type Result<T> = core::result::Result<T, Error>;

/// State of the command parser.
#[derive(Debug)]
pub(crate) enum State<C, V, Tm> {
    /// nothing
    Init,

    /// `def`
    Def,
    /// `thm`
    Thm,

    /// `s` followed by `:` if true
    Decl(C, bool),

    /// `def/thm s (x1 : t1) .. (xn : tn)`
    Args(DefThm, C, Vec<Tm>),
    /// `def/thm s (x1 : t1) .. (`
    /// followed by `x`   if `Some((x, false))`
    /// followed by `x :` if `Some((x,  true))`
    ArgsIn(DefThm, C, Vec<Tm>, Option<(V, bool)>),
    /// `def/thm s (x1 : t1) ... (xn: tn) : ty :=`
    DefThmEq(DefThm, C, Vec<Tm>, Tm),

    /// `def s (x1 : t1) .. (xn : tn)` followed by `:` or `:=`
    DefOfEq(C, Vec<Tm>, OfEq),
    /// `thm s (x1 : t1) .. (xn : tn)` followed by `:`
    ThmOf(C, Vec<Tm>),

    /// `[x1 : t1, ..,`
    /// followed by `x`   if `Some((x, false))`
    /// followed by `x :` if `Some((x,  true))`
    RuleCtx(RuleCtx<V, Tm>, Option<(V, bool)>),

    /// `[x1 : t1, .., xn : tn]`
    RuleL(RuleCtx<V, Tm>),
    /// `[x1 : t1, .., xn : tn] tm -->`
    RuleR(RuleCtx<V, Tm>, Tm),

    /// `#`
    Pragma,

    Command(Command<C, V, Tm>),
}

impl<C, V, Tm> Default for State<C, V, Tm> {
    fn default() -> Self {
        Self::Init
    }
}

pub trait Joker {
    fn joker() -> Self;
}

impl<'s> Joker for &'s str {
    fn joker() -> Self {
        "_"
    }
}

impl Joker for alloc::string::String {
    fn joker() -> Self {
        "_".into()
    }
}

impl<C, V: Joker, Tm> State<C, V, Tm> {
    pub fn parse<S>(self, bound: &mut Vec<V>, token: Token<S>) -> Result<State<C, V, Tm>>
    where
        S: Into<C> + Into<V>,
    {
        match (self, token) {
            (State::Pragma, Token::Dot) => Ok(State::Init),
            (State::Pragma, _) => Ok(State::Pragma),

            (state, Token::Symb(s)) if s.path.is_empty() => state.apply_name(s.name),
            (_, Token::Symb(_)) => Err(Error::UnexpectedPath),

            // starting commands
            (State::Init, Token::Def) => Ok(State::Def),
            (State::Init, Token::Thm) => Ok(State::Thm),
            (State::Init, Token::LBrk) => Ok(State::RuleCtx(Default::default(), None)),
            (State::Init, Token::Hash) => Ok(State::Pragma),
            (State::Init, _) => Err(Error::ExpectedCmd),

            // s + :
            (State::Decl(s, false), Token::Colon) => Ok(State::Decl(s, true)),
            (State::Decl(_, false), _) => Err(Error::ExpectedColon),

            (State::Def | State::Thm, _) => Err(Error::ExpectedIdent),

            // def/thm s + (
            (State::Args(dt, s, ctx), Token::LPar) => Ok(State::ArgsIn(dt, s, ctx, None)),

            // thm s (..) + :
            (State::Args(DefThm::Thm, s, c), Token::Colon) => Ok(State::ThmOf(s, c)),
            // def s (..) + :
            (State::Args(DefThm::Def, s, c), Token::Colon) => Ok(State::DefOfEq(s, c, OfEq::Of)),
            // def s (..) + :=
            (State::Args(DefThm::Def, s, c), Token::ColonEq) => Ok(State::DefOfEq(s, c, OfEq::Eq)),
            (State::Args(DefThm::Thm, _, _), _) => Err(Error::ExpectedColon),
            (State::Args(DefThm::Def, _, _), _) => Err(Error::ExpectedColonOrColonEq),

            (State::ArgsIn(_, _, _, None), _) => Err(Error::ExpectedIdent),

            // def s (x + :
            (State::ArgsIn(dt, s, c, Some((x, false))), Token::Colon) => {
                Ok(State::ArgsIn(dt, s, c, Some((x, true))))
            }
            (State::ArgsIn(_, _, _, Some((_, false))), _) => Err(Error::ExpectedColon),

            // [x1 : t1, .., + ]
            (State::RuleCtx(c, None), Token::RBrk) => Ok(Self::rulel(bound, c)),
            (State::RuleCtx(_, None), _) => Err(Error::ExpectedCommaOrRBrk),

            // [x1 : t1, .., x + :
            (State::RuleCtx(c, Some((x, false))), Token::Colon) => {
                Ok(State::RuleCtx(c, Some((x, true))))
            }
            // [x1 : t1, .., x + ,
            (State::RuleCtx(mut c, Some((s, false))), Token::Comma) => {
                bound.push(s);
                c.vars.push(None);
                Ok(State::RuleCtx(c, None))
            }
            // [x1 : t1, .., x + ]
            (State::RuleCtx(mut c, Some((s, false))), Token::RBrk) => {
                bound.push(s);
                c.vars.push(None);
                Ok(Self::rulel(bound, c))
            }
            // TODO: comma, rbrk, OR colon!
            (State::RuleCtx(_, Some((_, false))), _) => Err(Error::ExpectedCommaOrRBrk),

            _ => Err(Error::UnexpectedToken),
        }
    }

    pub fn apply_name(self, name: impl Into<C> + Into<V>) -> Result<State<C, V, Tm>> {
        match self {
            State::Init => Ok(State::Decl(name.into(), false)),
            // def/thm + s
            State::Def => Ok(State::Args(DefThm::Def, name.into(), Vec::new())),
            State::Thm => Ok(State::Args(DefThm::Thm, name.into(), Vec::new())),
            // def/thm s ( + x
            State::ArgsIn(dt, s, c, None) => {
                Ok(State::ArgsIn(dt, s, c, Some((name.into(), false))))
            }
            // [x1 : t1, .., + x
            State::RuleCtx(c, None) => Ok(State::RuleCtx(c, Some((name.into(), false)))),
            _ => Err(Error::UnexpectedToken),
        }
    }

    pub fn expects_term(&self) -> bool {
        matches!(
            self,
            State::Decl(_, true)
                | State::ThmOf(..)
                | State::DefOfEq(..)
                | State::DefThmEq(..)
                | State::ArgsIn(.., Some((_, true)))
                | State::RuleCtx(_, Some((_, true)))
                | State::RuleL(_)
                | State::RuleR(..)
        )
    }

    pub fn rulel(bound: &mut Vec<V>, ctx: RuleCtx<V, Tm>) -> Self {
        bound.insert(0, V::joker());
        Self::RuleL(ctx)
    }

    pub fn apply<S>(self, bound: &mut Vec<V>, tm: Tm, token: Token<S>) -> Result<State<C, V, Tm>> {
        match (self, token) {
            (State::ArgsIn(dt, s, mut ctx, Some((x, true))), Token::RPar) => {
                bound.push(x);
                ctx.push(tm);
                Ok(State::Args(dt, s, ctx))
            }

            (State::RuleL(ctx), Token::LongArrow) => {
                // kill the joker
                bound.remove(0);
                Ok(State::RuleR(ctx, tm))
            }
            (State::RuleL(..), _) => Err(Error::ExpectedLongArrow),
            (State::RuleR(ctx, lhs), Token::LBrk) => {
                Ok(State::RuleCtx(ctx.add(bound, lhs, tm), None))
            }
            (State::RuleCtx(mut ctx, Some((s, true))), Token::RBrk) => {
                bound.push(s);
                ctx.vars.push(Some(tm));
                Ok(Self::rulel(bound, ctx))
            }
            (State::RuleCtx(mut ctx, Some((s, true))), Token::Comma) => {
                bound.push(s);
                ctx.vars.push(Some(tm));
                Ok(State::RuleCtx(ctx, None))
            }
            (State::RuleCtx(..), _) => Err(Error::ExpectedCommaOrRBrk),
            (State::ThmOf(x, ctx), Token::ColonEq) => Ok(State::DefThmEq(DefThm::Thm, x, ctx, tm)),
            (State::DefOfEq(x, ctx, OfEq::Of), Token::ColonEq) => {
                Ok(State::DefThmEq(DefThm::Def, x, ctx, tm))
            }
            (State::ThmOf(..), _) => Err(Error::ExpectedColonEq),

            (State::ArgsIn(_, _, _, Some((_, true))), _) => Err(Error::ExpectedRPar),

            (cur, Token::Dot) => Ok(State::Command(cur.close(bound, tm)?)),
            _ => Err(Error::UnexpectedToken),
        }
    }

    fn close(self, bound: &mut Vec<V>, tm: Tm) -> Result<Command<C, V, Tm>> {
        match self {
            State::Decl(x, true) => Ok(Command::Intro(x, Vec::new(), Intro::Declaration(tm))),
            State::DefOfEq(x, ctx, ofeq) => {
                let it = match ofeq {
                    OfEq::Eq => Intro::Definition(None, Some(tm)),
                    OfEq::Of => Intro::Definition(Some(tm), None),
                };
                let ctx = bound.drain(..).zip(ctx).collect();
                Ok(Command::Intro(x, ctx, it))
            }
            State::DefThmEq(dt, x, ctx, ty) => {
                let it = match dt {
                    DefThm::Thm => Intro::Theorem(ty, tm),
                    DefThm::Def => Intro::Definition(Some(ty), Some(tm)),
                };
                let ctx = bound.drain(..).zip(ctx).collect();
                Ok(Command::Intro(x, ctx, it))
            }
            State::RuleR(ctx, lhs) => Ok(Command::Rules(ctx.add(bound, lhs, tm).rules)),
            _ => Err(Error::ExpectedCmd),
        }
    }
}
