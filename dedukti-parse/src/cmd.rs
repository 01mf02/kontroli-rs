use crate::Token;
use crate::{Command, Intro, Rule, Term};
use alloc::vec::Vec;

#[derive(Debug)]
pub enum DefThm {
    Def,
    Thm,
}

#[derive(Debug)]
pub enum OfEq {
    Of,
    Eq,
}

#[derive(Debug)]
pub struct RuleCtx<S, Tm> {
    rules: Vec<Rule<S, Tm>>,
    vars: Vec<(S, Option<Tm>)>,
}

impl<S, Tm> Default for RuleCtx<S, Tm> {
    fn default() -> Self {
        Self {
            rules: Default::default(),
            vars: Default::default(),
        }
    }
}

impl<S, Tm> RuleCtx<S, Tm> {
    fn add(mut self, lhs: Tm, rhs: Tm) -> Self {
        let ctx = core::mem::take(&mut self.vars);
        self.rules.push(Rule { ctx, lhs, rhs });
        self
    }
}

#[derive(Debug)]
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
    Term(crate::term::Error),
}

pub type RState<S, Tm> = Result<State<S, Tm>, Error>;

#[derive(Debug)]
pub enum State<S, Tm = Term<S>> {
    /// nothing
    Init,

    /// `def`
    Def,
    /// `thm`
    Thm,

    /// `s` followed by `:` if true
    Decl(S, bool),

    /// `def/thm s (x1 : t1) .. (xn : tn)`
    Args(DefThm, S, Vec<(S, Tm)>),
    /// `def/thm s (x1 : t1) .. (`
    /// followed by `x`   if `Some((x, false))`
    /// followed by `x :` if `Some((x,  true))`
    ArgsIn(DefThm, S, Vec<(S, Tm)>, Option<(S, bool)>),
    /// `def/thm s (x1 : t1) ... (xn: tn) : ty :=`
    DefThmEq(DefThm, S, Vec<(S, Tm)>, Tm),

    /// `def s (x1 : t1) .. (xn : tn)` followed by `:` or `:=`
    DefOfEq(S, Vec<(S, Tm)>, OfEq),
    /// `thm s (x1 : t1) .. (xn : tn)` followed by `:`
    ThmOf(S, Vec<(S, Tm)>),

    /// `[x1 : t1, ..,`
    /// followed by `x`   if `Some((x, false))`
    /// followed by `x :` if `Some((x,  true))`
    RuleCtx(RuleCtx<S, Tm>, Option<(S, bool)>),

    /// `[x1 : t1, .., xn : tn]`
    RuleL(RuleCtx<S, Tm>),
    /// `[x1 : t1, .., xn : tn] tm -->`
    RuleR(RuleCtx<S, Tm>, Tm),

    Command(Command<S, Tm>),
}

impl<S, Tm> State<S, Tm> {
    pub fn parse<I, F>(self, f: &mut F, token: Token<S>, iter: &mut I) -> RState<S, Tm>
    where
        I: Iterator<Item = Token<S>>,
        F: FnMut(Token<S>, &mut I) -> Result<(Tm, Option<Token<S>>), crate::term::Error>,
    {
        match (self, token) {
            // starting commands
            (State::Init, Token::Ident(s)) => Ok(State::Decl(s, false)),
            (State::Init, Token::Def) => Ok(State::Def),
            (State::Init, Token::Thm) => Ok(State::Thm),
            (State::Init, Token::LBrk) => Ok(State::RuleCtx(Default::default(), None)),
            (State::Init, _) => Err(Error::ExpectedCmd),

            // s + :
            (State::Decl(s, false), Token::Colon) => Ok(State::Decl(s, true)),

            // def/thm + s
            (State::Def, Token::Ident(s)) => Ok(State::Args(DefThm::Def, s, Vec::new())),
            (State::Thm, Token::Ident(s)) => Ok(State::Args(DefThm::Thm, s, Vec::new())),
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
                c.vars.push((s, None));
                Ok(State::RuleCtx(c, None))
            }
            // [x1 : t1, .., x + ]
            (State::RuleCtx(mut c, Some((s, false))), Token::RBrk) => {
                c.vars.push((s, None));
                Ok(State::RuleL(c))
            }
            // TODO: comma, rbrk, OR colon!
            (State::RuleCtx(_, Some((_, false))), _) => Err(Error::ExpectedCommaOrRBrk),

            (cur, token) => match f(token, iter).map_err(Error::Term)? {
                (tm, Some(tok2)) => cur.close(tm, tok2),
                // TODO: is this correct?
                (_tm, None) => Ok(cur),
            },
        }
    }

    fn close(self, tm: Tm, token: Token<S>) -> RState<S, Tm> {
        match (self, token) {
            (State::ArgsIn(dt, s, mut ctx, Some((x, true))), Token::RPar) => {
                ctx.push((x, tm));
                Ok(State::Args(dt, s, ctx))
            }

            (State::RuleL(ctx), Token::LongArrow) => Ok(State::RuleR(ctx, tm)),
            (State::RuleL(..), _) => Err(Error::ExpectedLongArrow),
            (State::RuleR(ctx, lhs), Token::LBrk) => Ok(State::RuleCtx(ctx.add(lhs, tm), None)),
            (State::RuleCtx(mut ctx, Some((s, true))), Token::RBrk) => {
                ctx.vars.push((s, Some(tm)));
                Ok(State::RuleL(ctx))
            }
            (State::RuleCtx(mut ctx, Some((s, true))), Token::Comma) => {
                ctx.vars.push((s, Some(tm)));
                Ok(State::RuleCtx(ctx, None))
            }
            (State::RuleCtx(..), _) => Err(Error::ExpectedCommaOrRBrk),
            (State::ThmOf(x, ctx), Token::ColonEq) => Ok(State::DefThmEq(DefThm::Thm, x, ctx, tm)),
            (State::DefOfEq(x, ctx, OfEq::Of), Token::ColonEq) => {
                Ok(State::DefThmEq(DefThm::Def, x, ctx, tm))
            }
            (State::ThmOf(..), _) => Err(Error::ExpectedColonEq),

            (State::ArgsIn(_, _, _, Some((_, true))), _) => Err(Error::ExpectedRPar),

            (cur, Token::Period) => Ok(State::Command(cur.close_command(tm)?)),
            (_, _) => Err(Error::UnexpectedToken),
        }
    }

    fn close_command(self, tm: Tm) -> Result<Command<S, Tm>, Error> {
        match self {
            State::Decl(x, true) => Ok(Command::Intro(x, Vec::new(), Intro::Declaration(tm))),
            State::DefOfEq(x, ctx, ofeq) => {
                let it = match ofeq {
                    OfEq::Eq => Intro::Definition(None, Some(tm)),
                    OfEq::Of => Intro::Definition(Some(tm), None),
                };
                Ok(Command::Intro(x, ctx, it))
            }
            State::DefThmEq(dt, x, ctx, ty) => {
                let it = match dt {
                    DefThm::Thm => Intro::Theorem(ty, tm),
                    DefThm::Def => Intro::Definition(Some(ty), Some(tm)),
                };
                Ok(Command::Intro(x, ctx, it))
            }
            State::RuleR(ctx, lhs) => Ok(Command::Rules(ctx.add(lhs, tm).rules)),
            _ => Err(Error::ExpectedCmd),
        }
    }
}
