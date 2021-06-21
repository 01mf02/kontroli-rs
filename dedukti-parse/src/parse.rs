use crate::Token;
use alloc::{boxed::Box, vec::Vec};
use core::iter::Peekable;

#[derive(Clone, Debug)]
pub enum Term<S> {
    // Symbol name, preceded by module path
    Symb(Vec<S>, S),
    // Application
    Appl(Box<Term<S>>, Vec<Term<S>>),
    // Abstraction (`x : A => t`)
    Abst(S, Option<Box<Term<S>>>, Box<Term<S>>),
    // Dependent product (`x : A -> t`)
    Prod(Option<S>, Box<Term<S>>, Box<Term<S>>),
}

#[derive(Debug)]
pub enum Intro<Ty, Tm = Ty> {
    Definition(Option<Ty>, Option<Tm>),
    Theorem(Ty, Tm),
    Declaration(Ty),
}

#[derive(Debug)]
pub struct Rule<S, Tm = Term<S>> {
    /// context (bound variables)
    pub ctx: Vec<(S, Option<Tm>)>,
    /// left-hand side (pattern to match with)
    pub lhs: Tm,
    /// right-hand side (term to replace with)
    pub rhs: Tm,
}

#[derive(Debug)]
pub enum Command<S, Tm = Term<S>> {
    // Introduce a new symbol with arguments
    Intro(S, Vec<(S, Tm)>, Intro<Tm>),
    // Add rewrite rules
    Rules(Vec<Rule<S, Tm>>),
}

#[derive(Debug)]
pub enum Error {
    ExpectedDot,
    ExpectedColon,
    ExpectedColonEq,
    ExpectedColonEqOrEoc,
    ExpectedColonOrColonEq,
    ExpectedRuleOrEoc,
    ExpectedArrow,
    ExpectedLongArrow,
    ExpectedTerm,
    ExpectedIdent,
    ExpectedIdentOrRBrk,
    ExpectedCommaOrRBrk,
    ExpectedRPar,
    ExpectedCmd,
}

pub fn yield_command<'s>(
    tokens: &mut impl Iterator<Item = Token<'s>>,
) -> Option<Result<Vec<Token<'s>>, Error>> {
    let mut cmd = Vec::new();
    loop {
        match tokens.next() {
            None if cmd.is_empty() => return None,
            None => return Some(Err(Error::ExpectedDot)),
            Some(Token::Dot) => match tokens.next() {
                // a command is always terminated by a dot,
                // followed by either some space or EOF
                Some(Token::Space) | None => return Some(Ok(cmd)),
                Some(t2) => {
                    cmd.push(Token::Dot);
                    cmd.push(t2);
                }
            },
            // throw away space
            Some(Token::Space) => (),
            Some(t1) => cmd.push(t1),
        }
    }
}

impl<'s, S: From<&'s str>> Command<S> {
    pub fn parse<I>(iter: &mut Peekable<I>) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        match iter.next() {
            Some(Token::Ident(id)) => Ok(Self::Intro(
                id.into(),
                Self::parse_args(iter)?,
                Intro::Declaration(Self::parse_type(iter)?),
            )),
            Some(Token::Def) => {
                let id = Term::parse_ident(iter)?;
                let args = Self::parse_args(iter)?;
                let (ty, tm) = match iter.next() {
                    Some(Token::Colon) => {
                        let ty = Term::parse(iter)?;
                        let tm = match iter.next() {
                            Some(Token::ColonEq) => Some(Term::parse(iter)?),
                            None => None,
                            _ => return Err(Error::ExpectedColonEqOrEoc),
                        };
                        (Some(ty), tm)
                    }
                    Some(Token::ColonEq) => (None, Some(Term::parse(iter)?)),
                    _ => return Err(Error::ExpectedColonOrColonEq),
                };
                Ok(Self::Intro(id, args, Intro::Definition(ty, tm)))
            }
            Some(Token::Thm) => {
                let id = Term::parse_ident(iter)?;
                let args = Self::parse_args(iter)?;
                let ty = Self::parse_type(iter)?;
                let tm = match iter.next() {
                    Some(Token::ColonEq) => Term::parse(iter)?,
                    _ => return Err(Error::ExpectedColonEq),
                };
                Ok(Self::Intro(id, args, Intro::Theorem(ty, tm)))
            }
            Some(Token::LBrk) => {
                let mut rules = Vec::new();
                rules.push(Rule::parse(iter)?);
                loop {
                    match iter.next() {
                        Some(Token::LBrk) => rules.push(Rule::parse(iter)?),
                        None => return Ok(Self::Rules(rules)),
                        _ => return Err(Error::ExpectedRuleOrEoc),
                    }
                }
            }
            _ => Err(Error::ExpectedCmd),
        }
    }

    fn parse_args<I>(iter: &mut Peekable<I>) -> Result<Vec<(S, Term<S>)>, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        let mut args = Vec::new();
        while let Some(Token::LPar) = iter.peek() {
            iter.next();
            args.push((Term::parse_ident(iter)?, Self::parse_type(iter)?));
            if iter.next() != Some(Token::RPar) {
                return Err(Error::ExpectedRPar);
            }
        }
        Ok(args)
    }

    fn parse_type<I>(iter: &mut Peekable<I>) -> Result<Term<S>, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        match iter.next() {
            Some(Token::Colon) => Term::parse(iter),
            _ => Err(Error::ExpectedColon),
        }
    }
}

impl<'s, S: From<&'s str>> Rule<S> {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Rule<S, Term<S>>, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        let mut ctx = Vec::new();
        match iter.next() {
            Some(Token::Ident(id)) => {
                let ty = Self::parse_type0(iter)?;
                ctx.push((id.into(), ty));
                loop {
                    match iter.next() {
                        Some(Token::Comma) => {
                            ctx.push((Term::parse_ident(iter)?, Self::parse_type0(iter)?))
                        }
                        Some(Token::RBrk) => break,
                        _ => return Err(Error::ExpectedCommaOrRBrk),
                    }
                }
            }
            Some(Token::RBrk) => (),
            _ => return Err(Error::ExpectedIdentOrRBrk),
        };
        let lhs = Term::parse(iter)?;
        if iter.next() != Some(Token::LongArrow) {
            return Err(Error::ExpectedLongArrow);
        };
        let rhs = Term::parse(iter)?;
        Ok(Rule { ctx, lhs, rhs })
    }

    fn parse_type0<I>(iter: &mut Peekable<I>) -> Result<Option<Term<S>>, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        match iter.peek() {
            Some(Token::Colon) => {
                iter.next();
                Ok(Some(Term::parse(iter)?))
            }
            _ => Ok(None),
        }
    }
}

impl<'s, S: From<&'s str>> Term<S> {
    pub fn parse<I>(iter: &mut Peekable<I>) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        let tm = Self::parse_non_lr(iter)?;
        match iter.peek() {
            Some(Token::Arrow) => {
                iter.next();
                let tm2 = Self::parse(iter)?;
                Ok(Self::Prod(None, Box::new(tm), Box::new(tm2)))
            }
            _ => Ok(tm),
        }
    }

    fn parse_non_lr<I>(iter: &mut Peekable<I>) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        match iter.next() {
            Some(Token::Ident(s)) => match iter.peek() {
                // `s`
                None => {
                    iter.next();
                    Ok(Term::Symb(Vec::new(), s.into()))
                }
                // `x : A -> B` or `x : A => t`
                Some(Token::Colon) => {
                    iter.next();
                    Self::binder(s.into(), iter)
                }
                // `x => t`
                Some(Token::FatArrow) => {
                    iter.next();
                    Ok(Term::Abst(s.into(), None, Box::new(Self::parse(iter)?)))
                }
                // `s t1 ... tn`
                Some(_) => Ok(Self::parse_appl(Self::symb(s, iter)?, iter)?),
            },
            // `(t) t1 ... tn`
            Some(Token::LPar) => Self::parse_appl(Self::parse_and_rpar(iter)?, iter),
            _ => Err(Error::ExpectedTerm),
        }
    }

    fn symb<I>(head: &'s str, iter: &mut Peekable<I>) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        let mut last = head.into();
        let mut before = Vec::new();
        while let Some(Token::Dot) = iter.peek() {
            iter.next();
            before.push(core::mem::replace(&mut last, Self::parse_ident(iter)?));
        }
        Ok(Self::Symb(before, last))
    }

    fn parse_ident(iter: &mut impl Iterator<Item = Token<'s>>) -> Result<S, Error> {
        match iter.next() {
            Some(Token::Ident(id)) => Ok(id.into()),
            _ => Err(Error::ExpectedIdent),
        }
    }

    /// Parse a term followed by a closing parenthesis ')'.
    fn parse_and_rpar<I>(iter: &mut Peekable<I>) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        let tm = Self::parse(iter)?;
        if iter.next() == Some(Token::RPar) {
            Ok(tm)
        } else {
            Err(Error::ExpectedRPar)
        }
    }

    // aterm
    fn parse_a<I>(iter: &mut Peekable<I>) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        Self::parse_appl(Self::parse_m1(iter)?, iter)
    }

    fn binder<I>(head: S, iter: &mut Peekable<I>) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        let ty = Box::new(Self::parse_a(iter)?);
        match iter.next() {
            Some(Token::FatArrow) => Ok(Term::Abst(head, Some(ty), Box::new(Self::parse(iter)?))),
            Some(Token::Arrow) => Ok(Term::Prod(Some(head), ty, Box::new(Self::parse(iter)?))),
            _ => Err(Error::ExpectedArrow),
        }
    }

    // sterm
    fn parse_m0<I>(iter: &mut Peekable<I>) -> Result<Option<Self>, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        match iter.peek() {
            Some(Token::Ident(s)) => {
                let s = *s;
                iter.next();
                Self::symb(s, iter).map(Some)
            }
            Some(Token::LPar) => {
                iter.next();
                Self::parse_and_rpar(iter).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn parse_m1<I>(iter: &mut Peekable<I>) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        match iter.next() {
            Some(Token::Ident(s)) => Self::symb(s, iter),
            Some(Token::LPar) => Self::parse_and_rpar(iter),
            _ => Err(Error::ExpectedTerm),
        }
    }

    fn parse_appl<I>(self, iter: &mut Peekable<I>) -> Result<Self, Error>
    where
        I: Iterator<Item = Token<'s>>,
    {
        let mut args = Vec::new();
        while let Some(tm) = Self::parse_m0(iter)? {
            args.push(tm)
        }
        if args.is_empty() {
            Ok(self)
        } else {
            // TODO: handle case where self is Appl?
            Ok(Term::Appl(Box::new(self), args))
        }
    }
}
