use nom::{
    branch::alt,
    bytes::streaming::{tag, take_until, take_while1},
    character::is_alphanumeric,
    character::streaming::{char, multispace0},
    combinator::{map, map_opt, opt, value},
    error::VerboseError,
    multi::{many0, separated_list},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use crate::precommand::{GDCommand, Precommand};
use crate::preterm::{Binder, Prearg, Preterm};
use std::convert::TryFrom;

type Parse<'a, A> = IResult<&'a [u8], A, VerboseError<&'a [u8]>>;

fn comment(i: &[u8]) -> Parse<&[u8]> {
    delimited(tag("(;"), take_until(";)"), tag(";)"))(i)
}

fn space(i: &[u8]) -> Parse<Vec<&[u8]>> {
    preceded(multispace0, many0(terminated(comment, multispace0)))(i)
}

fn lexeme<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    preceded(opt(space), inner)
}

fn parens<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    delimited(char('('), lexeme(inner), lexeme(char(')')))
}

fn phrase<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    terminated(inner, lexeme(char('.')))
}

pub fn string_from_u8(i: &[u8]) -> String {
    std::str::from_utf8(i).map(String::from).unwrap()
}

fn bracket_string(s: String) -> String {
    // this is fast because of:
    // <https://github.com/hoodie/concatenation_benchmarks-rs>
    let mut result = String::with_capacity(s.len() + 4);
    result.push_str("{|");
    result.push_str(&s);
    result.push_str("|}");
    result
}

fn bracket_ident(i: &[u8]) -> Parse<String> {
    map(delimited(tag("{|"), take_until("|}"), tag("|}")), |id| {
        bracket_string(string_from_u8(id))
    })(i)
}

fn normal_ident(i: &[u8]) -> Parse<String> {
    // 0x27 is: '
    // 0x5F is: _
    map(
        take_while1(|c| is_alphanumeric(c) || c == 0x5F || c == 0x27),
        string_from_u8,
    )(i)
}

fn ident(i: &[u8]) -> Parse<String> {
    alt((bracket_ident, normal_ident))(i)
}

fn maybe_ident(i: &[u8]) -> Parse<Option<String>> {
    alt((map(ident, Some), value(None, char('_'))))(i)
}

impl Prearg {
    fn parse(i: &[u8]) -> Parse<Self> {
        map(pair(maybe_ident, opt(lexeme(Preterm::of))), |(id, ty)| {
            Self { id, ty }
        })(i)
    }
}

impl Binder {
    fn pre(i: &[u8]) -> Parse<Self> {
        alt((value(Self::Lam, char('\\')), value(Self::Pi, char('!'))))(i)
    }

    fn post(i: &[u8]) -> Parse<Self> {
        alt((value(Self::Lam, tag("=>")), value(Self::Pi, tag("->"))))(i)
    }

    fn parse_unnamed<'a, O1, O2, F, G>(f: F, g: G) -> impl Fn(&'a [u8]) -> Parse<(O1, Self, O2)>
    where
        F: Fn(&'a [u8]) -> Parse<'a, O1>,
        G: Fn(&'a [u8]) -> Parse<'a, O2>,
    {
        preceded(char(':'), tuple((lexeme(f), lexeme(Self::post), lexeme(g))))
    }

    fn parse_named<'a, O1, O2, F, G>(f: F, g: G) -> impl Fn(&'a [u8]) -> Parse<(Self, O1, O2)>
    where
        F: Fn(&'a [u8]) -> Parse<'a, O1>,
        G: Fn(&'a [u8]) -> Parse<'a, O2>,
    {
        map_opt(
            tuple((Self::pre, lexeme(f), lexeme(Self::post), lexeme(g))),
            |(bnd, x, arr, y)| {
                if bnd == arr {
                    Some((bnd, x, y))
                } else {
                    None
                }
            },
        )
    }
}

impl Preterm {
    fn of(i: &[u8]) -> Parse<Box<Self>> {
        preceded(char(':'), map(lexeme(Self::parse), Box::new))(i)
    }

    fn is(i: &[u8]) -> Parse<Box<Self>> {
        preceded(tag(":="), map(lexeme(Self::parse), Box::new))(i)
    }

    fn sterm(i: &[u8]) -> Parse<Self> {
        alt((
            parens(Self::parse),
            value(Self::Type, tag("Type")),
            map(ident, Self::Symb),
        ))(i)
    }

    fn appl(i: &[u8]) -> Parse<Self> {
        map(
            pair(Self::sterm, many0(lexeme(Self::sterm))),
            |(head, tail)| Self::apply(head, tail),
        )(i)
    }

    fn bind_unnamed(i: &[u8]) -> Parse<Self> {
        map(
            Binder::parse_unnamed(Self::appl, Self::parse),
            |(ty, bnd, tm)| {
                let ty = Some(Box::new(ty));
                Self::Bind(bnd, Prearg { id: None, ty }, Box::new(tm))
            },
        )(i)
    }

    fn bind_named(i: &[u8]) -> Parse<Self> {
        map(
            Binder::parse_named(Prearg::parse, Self::parse),
            |(bnd, arg, tm)| Self::Bind(bnd, arg, Box::new(tm)),
        )(i)
    }

    pub fn parse(i: &[u8]) -> Parse<Self> {
        alt((Self::bind_named, Self::appl, Self::bind_unnamed))(i)
    }
}

impl Precommand {
    fn definition(i: &[u8]) -> Parse<Self> {
        preceded(
            tag("def"),
            map(
                tuple((
                    lexeme(ident),
                    many0(lexeme(parens(Prearg::parse))),
                    opt(lexeme(Preterm::of)),
                    opt(lexeme(Preterm::is)),
                )),
                |(id, params, ty, tm)| Self::DCmd(id, params, GDCommand::Definition(ty, tm)),
            ),
        )(i)
    }

    fn theorem(i: &[u8]) -> Parse<Self> {
        preceded(
            tag("thm"),
            map(
                tuple((
                    lexeme(ident),
                    many0(lexeme(parens(Prearg::parse))),
                    lexeme(Preterm::of),
                    lexeme(Preterm::is),
                )),
                |(id, params, ty, tm)| Self::DCmd(id, params, GDCommand::Theorem(ty, tm)),
            ),
        )(i)
    }

    fn declaration(i: &[u8]) -> Parse<Self> {
        map(
            tuple((
                ident,
                many0(lexeme(parens(Prearg::parse))),
                lexeme(Preterm::of),
            )),
            |(id, params, ty)| Self::DCmd(id, params, GDCommand::Declaration(ty)),
        )(i)
    }

    fn dcmd(i: &[u8]) -> Parse<Self> {
        alt((Self::definition, Self::theorem, Self::declaration))(i)
    }

    fn rule(i: &[u8]) -> Parse<Self> {
        map(
            tuple((
                preceded(
                    char('['),
                    terminated(
                        separated_list(lexeme(char(',')), lexeme(ident)),
                        lexeme(char(']')),
                    ),
                ),
                lexeme(Preterm::parse),
                lexeme(tag("-->")),
                lexeme(Preterm::parse),
            )),
            |(vars, lhs, _, rhs)| Self::Rule(vars, Box::new(lhs), Box::new(rhs)),
        )(i)
    }

    pub fn parse(i: &[u8]) -> Parse<Self> {
        alt((Self::dcmd, Self::rule))(i)
    }
}

impl<'a> TryFrom<&'a str> for Preterm {
    type Error = nom::Err<VerboseError<&'a [u8]>>;

    fn try_from(i: &'a str) -> Result<Self, Self::Error> {
        phrase(Self::parse)(i.as_bytes()).map(|(i, o)| o)
    }
}


// parse whitespace or commands
pub fn parse_toplevel(i: &[u8]) -> Parse<Option<Precommand>> {
    alt((
        value(None, nom::character::complete::multispace1),
        value(None, comment),
        map(phrase(Precommand::parse), Some),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn terms() {
        let pt = phrase(Preterm::parse);
        assert!(pt(b"x.").is_ok());
        assert!(pt(b":x -> x.").is_ok());
        assert!(pt(b":vec n -> vec (succ n).").is_ok());
        assert!(pt(b"! x -> x.").is_ok());
        assert!(pt(br"\ x => x.").is_ok());
        assert!(pt(b"! A : eta {|prop|type|} -> eps ({|Pure.eq|const|} {|prop|type|} ({|Pure.prop|const|} A) A).").is_ok());
    }

    #[test]
    fn commands() {
        let pc = phrase(Precommand::parse);
        assert!(pc(b"thm {|Pure.prop_def|thm|} : A := A.").is_ok());
        assert!(pc(r"def x : (;test;)(Type {|y|} {|💖!\|}).".as_bytes()).is_ok());
        assert!(pc(br"def x := \ x : Type Type => {|x|}.").is_ok());
    }
}
