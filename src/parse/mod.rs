//! Parsing to unshared, reference-free data structures.
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

mod intro;
mod pattern;
mod symbol;
pub mod term;

pub use intro::Intro;
pub use pattern::Pattern;
pub use symbol::Symbol;
pub use term::Term;

/// Rewrite rules with arguments as bound variables,
/// and preterms as left- and right-hand sides.
///
/// This is a vast overapproximation of rules, because
/// not every preterm is a valid rule left-hand side.
/// Scoping takes care to separate the wheat from the chaff.
pub type Rule = crate::Rule<OptArg, Term, Term>;

/// Signature-changing command.
pub type Command = crate::Command<String, Intro, Rule>;

use nom::{
    branch::alt,
    bytes::streaming::{is_not, tag, take_until, take_while1},
    character::is_alphanumeric,
    character::streaming::{char, one_of},
    combinator::{map, map_opt, map_res, opt, recognize, value},
    error::VerboseError,
    multi::{many0, many1, separated_list, separated_nonempty_list},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use alloc::{boxed::Box, string::String, vec::Vec};
use term::{Arg, Binder, OptArg};

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
/// # use kontroli::parse::{Symbol, Term, parse};
/// # use Term::{Symb, Appl};
/// let preterm = parse::<Term>("fst x y. Nothing to see here, move along.")?;
/// let head = Symb(Symbol::from("fst"));
/// let args = vec![Symb(Symbol::from("x")), Symb(Symbol::from("y"))];
/// assert_eq!(preterm, Appl(Box::new(head), args));
/// # Ok::<(), Error>(())
/// ~~~
pub fn parse<'a, P: Parser>(i: &'a str) -> Result<P, nom::Err<VerboseError<&'a [u8]>>> {
    phrase(P::parse)(i.as_bytes()).map(|(_i, o)| o)
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
/// # use kontroli::parse::nested;
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
/// # use kontroli::parse::comment;
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

fn space0(i: &[u8]) -> Parse<Vec<&[u8]>> {
    use nom::character::streaming::multispace0;
    preceded(multispace0, many0(terminated(comment, multispace0)))(i)
}

fn space1(i: &[u8]) -> Parse<Vec<&[u8]>> {
    use nom::character::streaming::multispace1;
    many1(alt((multispace1, comment)))(i)
}

/// Parse whitespace/comments to `None` and given function to `Some`.
pub fn opt_lex<'a, O1: Clone, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<Option<O1>>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    // attention: we are using the complete version of multispace1 here!
    use nom::character::complete::multispace1;
    alt((value(None, alt((multispace1, comment))), map(inner, Some)))
}

/// Strip away optional space before parsing with the given function.
fn lex<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
    F: Fn(&'a [u8]) -> Parse<'a, O1>,
{
    preceded(space0, inner)
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

impl Parser for Symbol {
    fn parse(i: &[u8]) -> Parse<Self> {
        map_opt(separated_nonempty_list(char('.'), ident), |mut path| {
            // this should always succeed, because the parsed list must be non-empty
            let name = path.pop()?;
            Some(Self { path, name })
        })(i)
    }
}

impl Parser for Arg {
    fn parse(i: &[u8]) -> Parse<Self> {
        map(pair(ident, lex(Term::of)), |(id, ty)| Self { id, ty })(i)
    }
}

impl Parser for OptArg {
    fn parse(i: &[u8]) -> Parse<Self> {
        map(pair(ident, lex(opt(Term::of))), |(id, ty)| Self { id, ty })(i)
    }
}

impl Binder {
    fn lam(i: &[u8]) -> Parse<Self> {
        value(Self::Lam, tag("=>"))(i)
    }

    fn pi(i: &[u8]) -> Parse<Self> {
        value(Self::Pi, tag("->"))(i)
    }
}

impl Parser for Binder {
    fn parse(i: &[u8]) -> Parse<Self> {
        alt((Self::lam, Self::pi))(i)
    }
}

impl Term {
    fn of(i: &[u8]) -> Parse<Box<Self>> {
        preceded(char(':'), map(lex(Self::parse), Box::new))(i)
    }

    fn of_appl(i: &[u8]) -> Parse<Box<Self>> {
        preceded(char(':'), map(lex(Self::appl), Box::new))(i)
    }

    fn is(i: &[u8]) -> Parse<Box<Self>> {
        preceded(tag(":="), map(lex(Self::parse), Box::new))(i)
    }

    fn sterm(i: &[u8]) -> Parse<Self> {
        alt((parens(Self::parse), map(Symbol::parse, Self::Symb)))(i)
    }

    fn appl(i: &[u8]) -> Parse<Self> {
        let app = |(head, tail)| Self::apply(head, tail);
        map(pair(Self::sterm, many0(lex(Self::sterm))), app)(i)
    }

    fn appl_or_bind_unnamed(i: &[u8]) -> Parse<Self> {
        let bind = preceded(Binder::pi, lex(Self::parse));
        map(pair(Self::appl, opt(lex(bind))), |(app, bind)| match bind {
            None => app,
            Some(bound) => Self::Prod(Arg::from(app), Box::new(bound)),
        })(i)
    }

    fn bind_named(i: &[u8]) -> Parse<Self> {
        let untyped = value(None, Binder::lam);
        let typed = map(pair(Term::of_appl, lex(Binder::parse)), Some);
        let binder = alt((untyped, typed));
        let bind = |(id, binder, tm)| match binder {
            None => Self::Abst(OptArg { id, ty: None }, Box::new(tm)),
            Some((ty, Binder::Lam)) => Self::Abst(OptArg { id, ty: Some(ty) }, Box::new(tm)),
            Some((ty, Binder::Pi)) => Self::Prod(Arg { id, ty }, Box::new(tm)),
        };
        map(tuple((ident, lex(binder), lex(Self::parse))), bind)(i)
    }
}

impl Parser for Term {
    /// ~~~
    /// # use kontroli::parse::{Parser, Term, phrase};
    /// let pt = phrase(Term::parse);
    /// assert!(pt(b"x.\n").is_ok());
    /// assert!(pt(b"x -> x.\n").is_ok());
    /// assert!(pt(b"N -> N -> N.\n").is_ok());
    /// assert!(pt(b"vec n -> vec (succ n).\n").is_ok());
    /// assert!(pt(b"x -> x.\n").is_ok());
    /// assert!(pt(br"x => x.\n").is_ok());
    /// assert!(pt(b"A : eta T -> A.\n").is_ok());
    /// assert!(pt(b"A : eta {|prop|type|} -> eps ({|Pure.eq|const|} {|prop|type|} ({|Pure.prop|const|} A) A).\n").is_ok());
    /// ~~~
    fn parse(i: &[u8]) -> Parse<Self> {
        alt((Self::bind_named, Self::appl_or_bind_unnamed))(i)
    }
}

impl Parser for Rule {
    /// ~~~
    /// # use kontroli::parse::{Parser, Rule, phrase};
    /// let pr = phrase(Rule::parse);
    /// assert!(pr(b"[x] id x --> x.\n").is_ok());
    /// assert!(pr(b"[x : A, y : B] fst x y --> x.\n").is_ok());
    /// ~~~
    fn parse(i: &[u8]) -> Parse<Self> {
        let args = separated_list(lex(char(',')), lex(OptArg::parse));
        let ctxt = delimited(char('['), args, lex(char(']')));
        map(
            tuple((ctxt, lex(Term::parse), lex(tag("-->")), lex(Term::parse))),
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
            terminated(tag("def"), space1),
            map(
                tuple((ident_args, opt(lex(Term::of)), opt(lex(Term::is)))),
                |((id, args), ty, tm)| Self::Intro(id, Intro::Definition(ty, tm).parametrise(args)),
            ),
        )(i)
    }

    fn theorem(i: &[u8]) -> Parse<Self> {
        preceded(
            terminated(tag("thm"), space1),
            map(
                tuple((ident_args, lex(Term::of), lex(Term::is))),
                |((id, args), ty, tm)| Self::Intro(id, Intro::Theorem(ty, tm).parametrise(args)),
            ),
        )(i)
    }

    fn declaration(i: &[u8]) -> Parse<Self> {
        map(tuple((ident_args, lex(Term::of))), |((id, args), ty)| {
            Self::Intro(id, Intro::Declaration(ty).parametrise(args))
        })(i)
    }

    fn intro(i: &[u8]) -> Parse<Self> {
        alt((Self::definition, Self::theorem, Self::declaration))(i)
    }
}

impl Parser for Command {
    /// ~~~
    /// # use kontroli::parse::{Command, Parser, phrase};
    /// let pc = phrase(Command::parse);
    /// assert!(pc(b"imp : prop -> prop -> prop.\n").is_ok());
    /// assert!(pc(b"thm {|Pure.prop_def|thm|} : A := A.\n").is_ok());
    /// assert!(pc(r"def x : (;test;)(Type {|y|} {|💖!\|}).\n".as_bytes()).is_ok());
    /// assert!(pc(br"def x := x : Type Type => {|x|}.\n").is_ok());
    /// assert!(pc(br"[X] id X --> X.\n").is_ok());
    /// assert!(pc(br"[X] pred S(X) --> X [] pred 0 --> 0.\n").is_ok());
    /// ~~~
    fn parse(i: &[u8]) -> Parse<Self> {
        alt((Self::intro, map(many1(lex(Rule::parse)), Self::Rules)))(i)
    }
}
