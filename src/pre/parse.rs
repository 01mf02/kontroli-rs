//! Parsers for prestructures.
//!
//! All basic parsers operate on byte slices (`&[u8]`) instead of strings (`&str`).
//! This enables efficient parsing using a circular buffer.
//!
//! The parsers for all primitives in this file assume that
//! they are being given input that has been lexed;
//! that is, leading whitespace has been removed.
//! This has a few practical implications:
//!
//! * If a parser consists of a sequence of other parsers,
//!   then it has to lex every parser in that sequence except the first.
//!   (We could also lex the first parser, but then, we would duplicate work,
//!   because we know by assumption that the whole parser is lexed.)
//! * If a parser consists of an alternative of parsers `alt(p1, ..., pn)`,
//!   then its lexed version should be
//!   the lexed alternative of parsers `lex(alt(p1, ..., pn))` instead of
//!   the alternative of lexed parsers `alt(lex(p1), ..., lex(pn))`.
//!   This avoids redoing the lexing for all alternatives.

use nom::{
    branch::alt,
    bytes::streaming::{is_not, tag, take_until, take_while1},
    character::is_alphanumeric,
    character::streaming::{char, multispace0, one_of},
    combinator::{map, map_opt, map_res, opt, recognize, value},
    error::VerboseError,
    multi::{many0, separated_list},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use super::command::{Command, GIntroType};
use super::term::{Arg, Binder, Term};
use super::Rule;
use alloc::{boxed::Box, string::String, vec::Vec};

/// Result of a parser.
pub type Parse<'a, A> = IResult<&'a [u8], A, VerboseError<&'a [u8]>>;

/// A trait similar to `FromStr`, but for byte slices instead of strings.
pub trait Parser: Sized {
    fn parse(i: &[u8]) -> Parse<Self>;
}

/// Parse a string phrase and discard remaining input.
///
/// ~~~
/// # use kontroli::Error;
/// # use kontroli::pre::Term;
/// # use kontroli::pre::parse::parse;
/// # use Term::{Symb, Appl};
/// let preterm = parse::<Term>("fst x y. Nothing to see here, move along.")?;
/// let head = Symb("fst".to_string());
/// let args = vec![Symb("x".to_string()), Symb("y".to_string())];
/// assert_eq!(preterm, Appl(Box::new(head), args));
/// # Ok::<(), Error>(())
/// ~~~
pub fn parse<'a, P: Parser>(i: &'a str) -> Result<P, nom::Err<VerboseError<&'a [u8]>>> {
    phrase(P::parse)(i.as_bytes()).map(|(_i, o)| o)
}

/// Parse arbitrary nesting of strings delimited by non-empty start and end tags.
///
/// ~~~
/// # use kontroli::pre::parse::nested;
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
/// # use kontroli::pre::parse::nested;
/// let bid = nested(b"{|", b"|}");
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
///
/// ~~~
/// # use kontroli::pre::parse::comment;
/// assert!(comment(b"(; ;)").is_ok());
/// assert!(comment(b"(; ; ;; ) ); ;)").is_ok());
/// assert!(comment(b"(; a normal comment ;)").is_ok());
/// assert!(comment(r"(;💖;)".as_bytes()).is_ok());
/// assert!(comment(b"(;;)").is_ok());
///
/// assert!(comment(b"(; ").is_err());
/// assert!(comment(b" ;)").is_err());
/// ~~~
pub fn comment(i: &[u8]) -> Parse<&[u8]> {
    nested(b"(;", b";)")(i)
}

fn space(i: &[u8]) -> Parse<Vec<&[u8]>> {
    preceded(multispace0, many0(terminated(comment, multispace0)))(i)
}

/// Parse whitespace/comments to `None` and given function to `Some`.
pub fn opt_lex<'a, O1: Clone, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<Option<O1>>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    alt((
        value(None, nom::character::complete::multispace1),
        value(None, comment),
        map(inner, Some),
    ))
}

/// Strip away space before parsing with the given function.
fn lex<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    preceded(opt(space), inner)
}

fn parens<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    delimited(char('('), lex(inner), lex(char(')')))
}

/// Parse a phrase, i.e. a given function terminated by a dot.
///
/// For example, this line is a phrase.
pub fn phrase<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    terminated(inner, lex(char('.')))
}

/// Parse bracket-surrounded identifier, like `{| anything \o/ goes |}`.
fn bracket_ident(i: &[u8]) -> Parse<&[u8]> {
    recognize(delimited(tag("{|"), take_until("|}"), tag("|}")))(i)
}

fn normal_ident(i: &[u8]) -> Parse<&[u8]> {
    // 0x27 is: '
    // 0x5F is: _
    take_while1(|c| is_alphanumeric(c) || c == 0x5F || c == 0x27)(i)
}

fn ident(i: &[u8]) -> Parse<String> {
    let ident_u8 = alt((bracket_ident, normal_ident));
    map_res(ident_u8, |i| alloc::str::from_utf8(i).map(String::from))(i)
}

impl Parser for Arg {
    fn parse(i: &[u8]) -> Parse<Self> {
        let of = preceded(char(':'), map(lex(Term::appl), Box::new));
        map(pair(ident, opt(lex(of))), |(id, ty)| Self { id, ty })(i)
    }
}

impl Binder {
    fn pre(i: &[u8]) -> Parse<Self> {
        alt((value(Self::Lam, char('\\')), value(Self::Pi, char('!'))))(i)
    }

    fn post(i: &[u8]) -> Parse<Self> {
        alt((value(Self::Lam, tag("=>")), value(Self::Pi, tag("->"))))(i)
    }

    fn parse_named<'a, O1, O2, F, G>(f: F, g: G) -> impl Fn(&'a [u8]) -> Parse<(Self, O1, O2)>
    where
        F: Fn(&'a [u8]) -> Parse<'a, O1>,
        G: Fn(&'a [u8]) -> Parse<'a, O2>,
    {
        let check = |(bnd, x, arr, y)| {
            if bnd == arr {
                Some((bnd, x, y))
            } else {
                None
            }
        };
        map_opt(tuple((Self::pre, lex(f), lex(Self::post), lex(g))), check)
    }
}

impl Term {
    fn of(i: &[u8]) -> Parse<Box<Self>> {
        preceded(char(':'), map(lex(Self::parse), Box::new))(i)
    }

    fn is(i: &[u8]) -> Parse<Box<Self>> {
        preceded(tag(":="), map(lex(Self::parse), Box::new))(i)
    }

    fn sterm(i: &[u8]) -> Parse<Self> {
        alt((parens(Self::parse), map(ident, Self::Symb)))(i)
    }

    fn appl(i: &[u8]) -> Parse<Self> {
        let app = |(head, tail)| Self::apply(head, tail);
        map(pair(Self::sterm, many0(lex(Self::sterm))), app)(i)
    }

    fn appl_or_bind_unnamed(i: &[u8]) -> Parse<Self> {
        let bind = pair(lex(Binder::post), lex(Self::parse));
        map(pair(Self::appl, opt(bind)), |(app, bind)| match bind {
            None => app,
            Some((binder, bound)) => Self::Bind(binder, Arg::from(app), Box::new(bound)),
        })(i)
    }

    fn bind_named(i: &[u8]) -> Parse<Self> {
        let bind = |(bnd, arg, tm)| Self::Bind(bnd, arg, Box::new(tm));
        map(Binder::parse_named(Arg::parse, Self::parse), bind)(i)
    }
}

impl Parser for Term {
    /// ~~~
    /// # use kontroli::pre::parse::{Parser, phrase};
    /// # use kontroli::pre::Term;
    /// let pt = phrase(Term::parse);
    /// assert!(pt(b"x.").is_ok());
    /// assert!(pt(b"x -> x.").is_ok());
    /// assert!(pt(b"N -> N -> N.").is_ok());
    /// assert!(pt(b"vec n -> vec (succ n).").is_ok());
    /// assert!(pt(b"! x -> x.").is_ok());
    /// assert!(pt(br"\ x => x.").is_ok());
    /// assert!(pt(b"! A : eta {|prop|type|} -> eps ({|Pure.eq|const|} {|prop|type|} ({|Pure.prop|const|} A) A).").is_ok());
    /// ~~~
    fn parse(i: &[u8]) -> Parse<Self> {
        alt((Self::bind_named, Self::appl_or_bind_unnamed))(i)
    }
}

/// Parse a (potentially empty) list of comma-separated identifiers.
fn idents(i: &[u8]) -> Parse<Vec<String>> {
    separated_list(lex(char(',')), lex(ident))(i)
}

impl Parser for Rule {
    fn parse(i: &[u8]) -> Parse<Self> {
        map(
            tuple((
                delimited(char('['), idents, lex(char(']'))),
                lex(Term::parse),
                lex(tag("-->")),
                lex(Term::parse),
            )),
            |(ctx, lhs, _, rhs)| Rule { ctx, lhs, rhs },
        )(i)
    }
}

/// Parse an identifier followed by an arbitrary number of arguments.
fn ident_args(i: &[u8]) -> Parse<(String, Vec<Arg>)> {
    pair(ident, many0(lex(parens(Arg::parse))))(i)
}

impl Command {
    fn definition(i: &[u8]) -> Parse<Self> {
        preceded(
            tag("def"),
            map(
                tuple((lex(ident_args), opt(lex(Term::of)), opt(lex(Term::is)))),
                |((id, args), ty, tm)| Self::Intro(id, args, GIntroType::Definition(ty, tm)),
            ),
        )(i)
    }

    fn theorem(i: &[u8]) -> Parse<Self> {
        preceded(
            tag("thm"),
            map(
                tuple((lex(ident_args), lex(Term::of), lex(Term::is))),
                |((id, args), ty, tm)| Self::Intro(id, args, GIntroType::Theorem(ty, tm)),
            ),
        )(i)
    }

    fn declaration(i: &[u8]) -> Parse<Self> {
        map(tuple((ident_args, lex(Term::of))), |((id, args), ty)| {
            Self::Intro(id, args, GIntroType::Declaration(ty))
        })(i)
    }

    fn intro(i: &[u8]) -> Parse<Self> {
        alt((Self::definition, Self::theorem, Self::declaration))(i)
    }
}

impl Parser for Command {
    /// ~~~
    /// # use kontroli::pre::parse::{Parser, phrase};
    /// # use kontroli::pre::Command;
    /// let pc = phrase(Command::parse);
    /// assert!(pc(b"imp : prop -> prop -> prop.").is_ok());
    /// assert!(pc(b"thm {|Pure.prop_def|thm|} : A := A.").is_ok());
    /// assert!(pc(r"def x : (;test;)(Type {|y|} {|💖!\|}).".as_bytes()).is_ok());
    /// assert!(pc(br"def x := \ x : Type Type => {|x|}.").is_ok());
    /// ~~~
    fn parse(i: &[u8]) -> Parse<Self> {
        alt((Self::intro, map(Rule::parse, Self::Rule)))(i)
    }
}
