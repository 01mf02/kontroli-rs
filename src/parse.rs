use nom::{
    branch::alt,
    bytes::streaming::{tag, take_until, take_while1},
    character::is_alphanumeric,
    character::streaming::{char, multispace0},
    combinator::{map, opt, value},
    error::VerboseError,
    multi::{many0, separated_list},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use crate::command::*;
use crate::term::*;

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

fn bracket_ident(i: &[u8]) -> Parse<&[u8]> {
    delimited(tag("{|"), take_until("|}"), tag("|}"))(i)
}

fn normal_ident(i: &[u8]) -> Parse<&[u8]> {
    // 0x5F is '_'
    take_while1(|c| is_alphanumeric(c) || c == 0x5F)(i)
}

fn ident(i: &[u8]) -> Parse<String> {
    map(alt((bracket_ident, normal_ident)), |id| {
        std::str::from_utf8(id).map(String::from).unwrap()
    })(i)
}

fn sterm(i: &[u8]) -> Parse<Term> {
    alt((
        parens(term),
        value(Term::Type, tag("Type")),
        map(ident, Term::Symb),
    ))(i)
}

fn appl(i: &[u8]) -> Parse<Term> {
    map(pair(sterm, many0(lexeme(sterm))), |(head, tail)| {
        Term::Appl(Box::new(head), tail.into_iter().map(Box::new).collect())
    })(i)
}

fn maybe_ident(i: &[u8]) -> Parse<Option<String>> {
    alt((map(ident, Some), value(None, char('_'))))(i)
}

fn of_term(i: &[u8]) -> Parse<BTerm> {
    preceded(char(':'), map(lexeme(term), Box::new))(i)
}

fn is_term(i: &[u8]) -> Parse<BTerm> {
    preceded(tag(":="), map(lexeme(term), Box::new))(i)
}

fn arg(i: &[u8]) -> Parse<Arg> {
    map(pair(maybe_ident, opt(lexeme(of_term))), |(id, ty)| Arg {
        id,
        ty,
    })(i)
}

fn abst(i: &[u8]) -> Parse<Term> {
    map(
        pair(
            preceded(char('\\'), terminated(lexeme(arg), lexeme(tag("=>")))),
            lexeme(term),
        ),
        |(arg, tm)| Term::Abst(arg, Box::new(tm)),
    )(i)
}

fn prod(i: &[u8]) -> Parse<Term> {
    map(
        pair(
            preceded(char('!'), terminated(lexeme(arg), lexeme(tag("->")))),
            lexeme(term),
        ),
        |(arg, tm)| Term::Prod(arg, Box::new(tm)),
    )(i)
}

fn term(i: &[u8]) -> Parse<Term> {
    alt((abst, prod, appl))(i)
}

fn dcommand(i: &[u8]) -> Parse<Command> {
    use Command::DCmd;
    use DCommand::*;

    alt((
        preceded(
            tag("def"),
            map(
                tuple((
                    lexeme(ident),
                    many0(lexeme(parens(arg))),
                    opt(lexeme(of_term)),
                    opt(lexeme(is_term)),
                )),
                |(id, params, ty, tm)| DCmd(id, params, Definition(ty, tm)),
            ),
        ),
        preceded(
            tag("thm"),
            map(
                tuple((
                    lexeme(ident),
                    many0(lexeme(parens(arg))),
                    lexeme(of_term),
                    lexeme(is_term),
                )),
                |(id, params, ty, tm)| DCmd(id, params, Theorem(ty, tm)),
            ),
        ),
        map(
            tuple((ident, many0(lexeme(parens(arg))), lexeme(of_term))),
            |(id, params, ty)| DCmd(id, params, Declaration(ty)),
        ),
    ))(i)
}

fn rule(i: &[u8]) -> Parse<Command> {
    map(
        tuple((
            preceded(
                char('['),
                terminated(
                    separated_list(lexeme(char(',')), lexeme(ident)),
                    lexeme(char(']')),
                ),
            ),
            lexeme(term),
            lexeme(tag("-->")),
            lexeme(term),
        )),
        |(vars, lhs, _, rhs)| Command::Rule(vars, Box::new(lhs), Box::new(rhs)),
    )(i)
}

fn command(i: &[u8]) -> Parse<Command> {
    alt((dcommand, rule))(i)
}

// parse whitespace or commands
pub fn parse_toplevel(i: &[u8]) -> Parse<Option<Command>> {
    alt((
        value(None, nom::character::complete::multispace1),
        value(None, comment),
        map(terminated(command, lexeme(char('.'))), Some),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn terms() {
        let pt = terminated(term, lexeme(char('.')));
        assert!(pt(b"x.").is_ok());
        assert!(pt(b"! x -> x.").is_ok());
        assert!(pt(br"\ x => x.").is_ok());
        assert!(pt(b"! A : eta {|prop|type|} -> eps ({|Pure.eq|const|} {|prop|type|} ({|Pure.prop|const|} A) A).").is_ok());
    }

    #[test]
    fn commands() {
        let pc = terminated(command, lexeme(char('.')));
        assert!(pc(b"thm {|Pure.prop_def|thm|} : A := A.").is_ok());
        assert!(pc(r"def x : (;test;)(Type {|y|} {|💖!\|}).".as_bytes()).is_ok());
        assert!(pc(br"def x := \ x : Type Type => {|x|}.").is_ok());
    }
}
