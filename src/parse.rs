//! Parsers for prestructures.

use nom::{
    branch::alt,
    bytes::streaming::{is_not, tag, take_until, take_while1},
    character::is_alphanumeric,
    character::streaming::{char, multispace0, one_of},
    combinator::{map, map_opt, opt, recognize, value},
    error::VerboseError,
    multi::{many0, separated_list},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use crate::precommand::{GDCommand, Precommand};
use crate::prerule::Prerule;
use crate::preterm::{Binder, Prearg, Preterm};

pub type Parse<'a, A> = IResult<&'a [u8], A, VerboseError<&'a [u8]>>;

pub trait Parser: Sized {
    fn parse(i: &[u8]) -> Parse<Self>;
}

/// Parse arbitrary nesting of strings delimited by non-empty start and end tags.
///
/// ~~~
/// # use kontroli::parse::nested;
/// let c_style = nested(b"/*", b"*/");
/// assert!(c_style(b"/* here /* more */ */").is_ok());
///
/// // all nestings have to be closed
/// assert!(c_style(b"/* here /* more */").is_err());
/// ~~~
///
/// To allow nested bracketed idents
/// (which slows down parsing by about 10% compared to unnested ones):
///
/// ~~~
/// # use nom::combinator::map;
/// # use kontroli::parse::{nested, string_from_u8};
/// let bid = map(nested(b"{|", b"|}"), string_from_u8);
/// assert!(bid(b"{|n|}").is_ok());
/// assert!(bid(b"{|quoting {|n|} is fun|}").is_ok());
/// ~~~
pub fn nested<'a>(start: &'a [u8], end: &'a [u8]) -> impl Fn(&'a [u8]) -> Parse<&'a [u8]> {
    recognize(pair(tag(start), nested_post(start, end)))
}

fn nested_post<'a>(start: &'a [u8], end: &'a [u8]) -> impl Fn(&'a [u8]) -> Parse<&'a [u8]> {
    let begins = [start[0], end[0]];

    move |i: &'a [u8]| {
        recognize(pair(
            // first, we read until we see either the begin of start or end
            opt(is_not(begins)),
            alt((
                // if we then recognize the end, we are done
                tag(end),
                // otherwise, we assume that we got either
                // a new nesting or a stray character corresponding to a beginning
                recognize(pair(
                    alt((nested(start, end), recognize(one_of(begins)))),
                    nested_post(start, end),
                )),
            )),
        ))(i)
    }
}

/// Parse a (potentially nested) comment.
fn comment(i: &[u8]) -> Parse<&[u8]> {
    nested(b"(;", b";)")(i)
}

fn space(i: &[u8]) -> Parse<Vec<&[u8]>> {
    preceded(multispace0, many0(terminated(comment, multispace0)))(i)
}

/// Parse whitespace/comments to `None` and given function to `Some`.
pub fn opt_lexeme<'a, O1: Clone, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<Option<O1>>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    alt((
        value(None, nom::character::complete::multispace1),
        value(None, comment),
        map(inner, Some),
    ))
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

pub fn phrase<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    terminated(inner, lexeme(char('.')))
}

pub fn string_from_u8(i: &[u8]) -> String {
    std::str::from_utf8(i).map(String::from).unwrap()
}

fn bracket_ident(i: &[u8]) -> Parse<String> {
    map(
        recognize(delimited(tag("{|"), take_until("|}"), tag("|}"))),
        string_from_u8,
    )(i)
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

impl Parser for Prearg {
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
}

impl Parser for Preterm {
    fn parse(i: &[u8]) -> Parse<Self> {
        alt((Self::bind_named, Self::appl, Self::bind_unnamed))(i)
    }
}

impl Parser for Prerule {
    fn parse(i: &[u8]) -> Parse<Self> {
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
            |(ctx, lhs, _, rhs)| Prerule { ctx, lhs, rhs },
        )(i)
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
}

impl Parser for Precommand {
    fn parse(i: &[u8]) -> Parse<Self> {
        alt((Self::dcmd, map(Prerule::parse, Self::Rule)))(i)
    }
}

pub fn parse<'a, P: Parser>(i: &'a str) -> Result<P, nom::Err<VerboseError<&'a [u8]>>> {
    phrase(P::parse)(i.as_bytes()).map(|(_i, o)| o)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comments() {
        assert!(comment(b"(; ;)").is_ok());
        assert!(comment(b"(; ; ;; ) ); ;)").is_ok());
        assert!(comment(b"(; a normal comment ;)").is_ok());
        assert!(comment(r"(;💖;)".as_bytes()).is_ok());
        assert!(comment(b"(;;)").is_ok());

        assert!(comment(b"(; ").is_err());
        assert!(comment(b" ;)").is_err());
    }

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
