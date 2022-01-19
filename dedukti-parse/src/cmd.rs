use crate::{Bound, CmdIter, Term, Token};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt::{self, Display};

#[derive(Clone, Debug)]
pub enum Command<Tm> {
    // Introduce a new symbol with arguments
    Intro(String, Vec<(String, Tm)>, Intro<Tm>),
    // Add rewrite rules
    Rules(Vec<Rule<Tm>>),
}

#[derive(Clone, Debug)]
pub enum Intro<Ty, Tm = Ty> {
    Definition(Option<Ty>, Option<Tm>),
    Theorem(Ty, Tm),
    Declaration(Ty),
}

#[derive(Clone, Debug)]
pub struct Rule<Tm> {
    /// context (bound variables)
    pub ctx: Vec<(String, Option<Tm>)>,
    /// left-hand side (pattern to match with)
    pub lhs: Tm,
    /// right-hand side (term to replace with)
    pub rhs: Tm,
}

impl<S: Display> Display for Command<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
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

impl<Tm: Display> Display for Rule<Tm> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
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
pub struct RuleCtx<Tm> {
    rules: Vec<Rule<Tm>>,
    vars: Vec<Option<Tm>>,
}

impl<Tm> Default for RuleCtx<Tm> {
    fn default() -> Self {
        Self {
            rules: Default::default(),
            vars: Default::default(),
        }
    }
}

impl<Tm> RuleCtx<Tm> {
    fn add(mut self, bound: &mut Bound, lhs: Tm, rhs: Tm) -> Self {
        let ctx = core::mem::take(&mut self.vars);
        let ctx = bound.drain(1..).zip(ctx).collect();
        self.rules.push(Rule { ctx, lhs, rhs });
        self
    }
}

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
    UnexpectedToken,
}

type RState<S, Tm> = Result<State<S, Tm>, Error>;

#[derive(Debug)]
pub(crate) enum State<S, Tm = Term<S>> {
    /// nothing
    Init,

    /// `def`
    Def,
    /// `thm`
    Thm,

    /// `s` followed by `:` if true
    Decl(String, bool),

    /// `def/thm s (x1 : t1) .. (xn : tn)`
    Args(DefThm, String, Vec<Tm>),
    /// `def/thm s (x1 : t1) .. (`
    /// followed by `x`   if `Some((x, false))`
    /// followed by `x :` if `Some((x,  true))`
    ArgsIn(DefThm, String, Vec<Tm>, Option<(S, bool)>),
    /// `def/thm s (x1 : t1) ... (xn: tn) : ty :=`
    DefThmEq(DefThm, String, Vec<Tm>, Tm),

    /// `def s (x1 : t1) .. (xn : tn)` followed by `:` or `:=`
    DefOfEq(String, Vec<Tm>, OfEq),
    /// `thm s (x1 : t1) .. (xn : tn)` followed by `:`
    ThmOf(String, Vec<Tm>),

    /// `[x1 : t1, ..,`
    /// followed by `x`   if `Some((x, false))`
    /// followed by `x :` if `Some((x,  true))`
    RuleCtx(RuleCtx<Tm>, Option<(S, bool)>),

    /// `[x1 : t1, .., xn : tn]`
    RuleL(RuleCtx<Tm>),
    /// `[x1 : t1, .., xn : tn] tm -->`
    RuleR(RuleCtx<Tm>, Tm),

    Command(Command<Tm>),
}

impl<S: Into<String>, Tm> State<S, Tm> {
    pub fn parse(self, bound: &mut Bound, token: Token<S>) -> RState<S, Tm> {
        match (self, token) {
            // starting commands
            (State::Init, Token::Ident(s)) => Ok(State::Decl(s.into(), false)),
            (State::Init, Token::Def) => Ok(State::Def),
            (State::Init, Token::Thm) => Ok(State::Thm),
            (State::Init, Token::LBrk) => {
                bound.push("_".to_string());
                Ok(State::RuleCtx(Default::default(), None))
            }
            (State::Init, _) => Err(Error::ExpectedCmd),

            // s + :
            (State::Decl(s, false), Token::Colon) => Ok(State::Decl(s, true)),
            (State::Decl(_, false), _) => Err(Error::ExpectedColon),

            // def/thm + s
            (State::Def, Token::Ident(s)) => Ok(State::Args(DefThm::Def, s.into(), Vec::new())),
            (State::Thm, Token::Ident(s)) => Ok(State::Args(DefThm::Thm, s.into(), Vec::new())),
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

            // def/thm s ( + x
            (State::ArgsIn(dt, s, c, None), Token::Ident(x)) => {
                Ok(State::ArgsIn(dt, s, c, Some((x, false))))
            }
            (State::ArgsIn(_, _, _, None), _) => Err(Error::ExpectedIdent),

            // def s (x + :
            (State::ArgsIn(dt, s, c, Some((x, false))), Token::Colon) => {
                Ok(State::ArgsIn(dt, s, c, Some((x, true))))
            }
            (State::ArgsIn(_, _, _, Some((_, false))), _) => Err(Error::ExpectedColon),

            // [x1 : t1, .., + ]
            (State::RuleCtx(c, None), Token::RBrk) => Ok(State::RuleL(c)),
            // [x1 : t1, .., + x
            (State::RuleCtx(c, None), Token::Ident(s)) => Ok(State::RuleCtx(c, Some((s, false)))),
            (State::RuleCtx(_, None), _) => Err(Error::ExpectedCommaOrRBrk),

            // [x1 : t1, .., x + :
            (State::RuleCtx(c, Some((x, false))), Token::Colon) => {
                Ok(State::RuleCtx(c, Some((x, true))))
            }
            // [x1 : t1, .., x + ,
            (State::RuleCtx(mut c, Some((s, false))), Token::Comma) => {
                bound.push(s.into());
                c.vars.push(None);
                Ok(State::RuleCtx(c, None))
            }
            // [x1 : t1, .., x + ]
            (State::RuleCtx(mut c, Some((s, false))), Token::RBrk) => {
                bound.push(s.into());
                c.vars.push(None);
                Ok(State::RuleL(c))
            }
            // TODO: comma, rbrk, OR colon!
            (State::RuleCtx(_, Some((_, false))), _) => Err(Error::ExpectedCommaOrRBrk),

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
                | State::ArgsIn(_, _, _, Some((_, true)))
                | State::RuleCtx(_, Some((_, true)))
                | State::RuleL(_)
                | State::RuleR(..)
        )
    }

    pub fn apply(self, bound: &mut Bound, tm: Tm, token: Token<S>) -> RState<S, Tm> {
        match (self, token) {
            (State::ArgsIn(dt, s, mut ctx, Some((x, true))), Token::RPar) => {
                bound.push(x.into());
                ctx.push(tm);
                Ok(State::Args(dt, s, ctx))
            }

            (State::RuleL(ctx), Token::LongArrow) => Ok(State::RuleR(ctx, tm)),
            (State::RuleL(..), _) => Err(Error::ExpectedLongArrow),
            (State::RuleR(ctx, lhs), Token::LBrk) => {
                Ok(State::RuleCtx(ctx.add(bound, lhs, tm), None))
            }
            (State::RuleCtx(mut ctx, Some((s, true))), Token::RBrk) => {
                bound.push(s.into());
                ctx.vars.push(Some(tm));
                Ok(State::RuleL(ctx))
            }
            (State::RuleCtx(mut ctx, Some((s, true))), Token::Comma) => {
                bound.push(s.into());
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

            (cur, Token::Period) => Ok(State::Command(cur.close(bound, tm)?)),
            _ => Err(Error::UnexpectedToken),
        }
    }

    fn close(self, bound: &mut Bound, tm: Tm) -> Result<Command<Tm>, Error> {
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
            State::RuleR(ctx, lhs) => {
                let rules = ctx.add(bound, lhs, tm).rules;
                bound.pop();
                Ok(Command::Rules(rules))
            }
            _ => Err(Error::ExpectedCmd),
        }
    }
}

impl<'s> Command<Term<&'s str>> {
    pub fn parse_str(s: &'s str) -> Result<Self, crate::Error> {
        let err = Err(crate::Error::ExpectedInput);
        CmdIter::new(s).next().unwrap_or(err)
    }
}

#[test]
fn positive() -> Result<(), crate::Error> {
    Command::parse_str("prop : Type.")?;
    Command::parse_str("imp: prop -> prop -> prop.")?;
    Command::parse_str("def prf: prop -> Type.")?;
    Command::parse_str("[x, y] prf (imp x y) --> prf x -> prf y.")?;
    Command::parse_str("thm imp_refl (x: prop) : prf (imp x x) := p: prf x => p.")?;
    Ok(())
}

#[test]
fn negative() {
    use Error::*;
    let parse_err = |s: &str| match Command::parse_str(s) {
        Err(crate::Error::Command(e)) => e,
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
