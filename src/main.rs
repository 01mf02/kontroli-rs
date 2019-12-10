extern crate nom;
extern crate circular;

use fnv::FnvHashMap;
use std::io::Read;
use std::iter::Iterator;

use nom::{
  branch::alt,
  bytes::streaming::{tag, take_until, take_while1},
  character::{is_alphanumeric},
  character::streaming::{char, multispace0},
  combinator::{flat_map, map, opt, value},
  error::VerboseError,
  multi::{fold_many0, many0},
  sequence::{preceded, terminated, delimited, pair, tuple},
  Err,
  IResult,
  Offset,
};


type Arg = (Option<String>, Option<BTerm>);
type BTerm = Box<Term>;

type DeBruijn = usize;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
  Type,
  Symb(String),
  BVar(DeBruijn),
  Appl(BTerm, BTerm),
  Abst(Arg, BTerm),
  Prod(Arg, BTerm),
}

impl Term {
  fn absts(self, args: Vec<Arg>) -> Term {
    args.into_iter().rev().fold(self, |acc, arg| Term::Abst(arg, Box::new(acc)))
  }
  fn prods(self, args: Vec<Arg>) -> Term {
    args.into_iter().rev().fold(self, |acc, arg| Term::Prod(arg, Box::new(acc)))
  }
}

type Command = (String, Vec<Arg>, DCommand);

#[derive(Debug, Clone)]
pub enum DCommand {
  Definition(Option<BTerm>, Option<BTerm>),
  Theorem(BTerm, BTerm),
  Declaration(BTerm),
}

impl DCommand {
  fn map_type<F>(self, f: F) -> DCommand
  where
    F: FnOnce(BTerm) -> BTerm
  {
    match self {
      Self::Definition(ty, tm) => Self::Definition(ty.map(f), tm),
      Self::Theorem(ty, tm) => Self::Theorem(f(ty), tm),
      Self::Declaration(ty) => Self::Declaration(f(ty)),
    }
  }
  fn map_term<F>(self, f: F) -> DCommand
  where
    F: FnOnce(BTerm) -> BTerm
  {
    match self {
      Self::Definition(ty, tm) => Self::Definition(ty, tm.map(f)),
      Self::Theorem(ty, tm) => Self::Theorem(ty, f(tm)),
      Self::Declaration(ty) => Self::Declaration(ty),
    }
  }

  fn parametrise(self, args: Vec<Arg>) -> DCommand {
    self
      .map_type(|tm| Box::new(tm.prods(args.clone())))
      .map_term(|tm| Box::new(tm.absts(args)))
  }

}


mod parse {

use super::*;

type Parse<'a, A> = IResult<&'a [u8], A, VerboseError<&'a [u8]>>;

fn comment(i: &[u8]) -> Parse<&[u8]> {
  delimited(tag("(;"), take_until(";)"), tag(";)"))(i)
}

fn space(i: &[u8]) -> Parse<Vec<&[u8]>> {
  preceded(multispace0, many0(terminated(comment, multispace0)))(i)
}

fn lexeme<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
  F: Fn(&'a [u8]) -> IResult<&'a [u8], O1, VerboseError<&'a [u8]>>,
{
  preceded(opt(space), inner)
}

fn parens<'a, O1, F>(inner: F) -> impl Fn(&'a [u8]) -> Parse<O1>
where
  F: Fn(&'a [u8]) -> IResult<&'a [u8], O1, VerboseError<&'a [u8]>>,
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
  map(alt((bracket_ident, normal_ident)),
    |id| std::str::from_utf8(id).map(String::from).unwrap()
  )(i)
}

fn sterm(i: &[u8]) -> Parse<Term> {
  alt((
    parens(term),
    value(Term::Type, tag("Type")),
    map(ident, Term::Symb),
  ))(i)
}

fn appl(i: &[u8]) -> Parse<Term> {
  flat_map(sterm, |head|
    fold_many0(lexeme(sterm), head, |acc, x|
      Term::Appl(Box::new(acc), Box::new(x))))(i)
}

fn maybe_ident(i: &[u8]) -> Parse<Option<String>> {
  alt((
    map(ident, Some),
    value(None, char('_'))
  ))(i)
}

fn of_term(i: &[u8]) -> Parse<BTerm> {
  preceded(char(':'), map(lexeme(term), Box::new))(i)
}

fn is_term(i: &[u8]) -> Parse<BTerm> {
  preceded(tag(":="), map(lexeme(term), Box::new))(i)
}

fn arg(i: &[u8]) -> Parse<Arg> {
  pair(maybe_ident, opt(lexeme(of_term)))(i)
}

fn abst(i: &[u8]) -> Parse<Term> {
  map(
    pair(
      preceded(char('\\'), terminated(lexeme(arg), lexeme(tag("=>")))),
      lexeme(term)
    ), |(arg, tm)| Term::Abst(arg, Box::new(tm)))(i)
}

fn prod(i: &[u8]) -> Parse<Term> {
  map(
    pair(
      preceded(char('!'), terminated(lexeme(arg), lexeme(tag("->")))),
      lexeme(term)
    ), |(arg, tm)| Term::Prod(arg, Box::new(tm)))(i)
}

fn term(i: &[u8]) -> Parse<Term> {
  alt((
    abst,
    prod,
    appl,
  ))(i)
}

fn command(i: &[u8]) -> Parse<Command> {
  alt((
    preceded(tag("def"),
      map(tuple((
        lexeme(ident),
        many0(lexeme(parens(arg))),
        opt(lexeme(of_term)),
        opt(lexeme(is_term)))),
        |(id, params, ty, tm)| (id, params, DCommand::Definition(ty, tm)))),
    preceded(tag("thm"),
      map(tuple((
        lexeme(ident),
        many0(lexeme(parens(arg))),
        lexeme(of_term),
        lexeme(is_term))),
        |(id, params, ty, tm)| (id, params, DCommand::Theorem(ty, tm)))),
    map(tuple((
      ident,
      many0(lexeme(parens(arg))),
      lexeme(of_term))),
      |(id, params, ty)| (id, params, DCommand::Declaration(ty))),
  ))
  (i)
}

// parse whitespace or commands
pub fn parse_toplevel(i: &[u8]) -> Parse<Option<Command>> {
  alt((
    value(None, nom::character::complete::multispace1),
    value(None, comment),
    map(terminated(command, lexeme(char('.'))), Some)
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

}


mod scope {

use super::*;

pub type SymTable = FnvHashMap<String, ()>;
type Bound = Vec<String>;

fn bind<A, F>(bnd: &mut Bound, arg: Option<String>, f: F) -> A
where
  F: FnOnce(&mut Bound) -> A
{
  match arg {
    Some(id) => {
      bnd.push(id);
      let x = f(bnd);
      bnd.pop();
      x
    },
    None => f(bnd)
  }
}

fn term(sym: &SymTable, bnd: &mut Bound, tm: Term) -> Term {
  use super::Term::*;
  match tm {
    Type => Type,
    Symb(s) => {
      match bnd.iter().rev().position(|id| *id == s) {
        Some(idx) => BVar(idx),
        None =>
          if sym.contains_key(&s) { Symb(s) }
          else { panic!("undeclared symbol") }
      }
    },
    Appl(t1, t2) => Appl(
      Box::new(term(sym, bnd, *t1)),
      Box::new(term(sym, bnd, *t2))),
    Abst(arg, tm) => {
      let arg = argument(sym, bnd, arg);
      bind(bnd, arg.0.clone(), |bnd| Abst(arg, Box::new(term(sym, bnd, *tm))))
    },
    Prod(arg, tm) => {
      let arg = argument(sym, bnd, arg);
      bind(bnd, arg.0.clone(), |bnd| Prod(arg, Box::new(term(sym, bnd, *tm))))
    }
    BVar(_) => panic!("found bound variable during scoping")
  }
}

fn argument(sym: &SymTable, bnd: &mut Bound, (id, tm): Arg) -> Arg {
  (id, tm.map(|tm| Box::new(term(sym, bnd, *tm))))
}

pub fn dcommand(sym: &SymTable, bnd: &mut Bound, dcmd: DCommand) -> DCommand {
  dcmd
    .map_type(|tm| Box::new(term(sym, bnd, *tm)))
    .map_term(|tm| Box::new(term(sym, bnd, *tm)))
}

}


fn run(filename: &str) -> std::io::Result<()> {
  let mut file = std::fs::File::open(filename)?;

  // circular::Buffer is a ring buffer abstraction that separates reading and consuming data
  // it can grow its internal buffer and move data around if we reached the end of that buffer
  let mut b = circular::Buffer::with_capacity(64 * 1024 * 1024); 
  let mut symbols: scope::SymTable = FnvHashMap::default();

  loop {
    let consumed = match parse::parse_toplevel(b.data()) {
      Err(Err::Incomplete(_)) => {
        // ensure that we have some space available in the buffer
        if b.available_space() == 0 {
          if b.position() == 0 {
            //println!("growing buffer capacity");

            // double buffer capacity
            b.grow(b.capacity() * 2);
          }
          else {
            b.shift();
          }
        }

        // read from file to free space of buffer
        let read_bytes = file.read(b.space()).expect("should write");
        b.fill(read_bytes);
        //println!("Read {} bytes from file", read_bytes);

        if b.available_data() == 0 {
          // no more data to read or parse, stopping the reading loop
          break;
        }
        else if read_bytes == 0 {
          panic!("incomplete parse at end of file");
        }

        // we did not consume anything
        0
      },

      Err(e) => {
        panic!("parse error: {:#?}", e);
      },

      Ok((remaining, toplevel)) => {
        if let Some((id, args, dcmd)) = toplevel {
          println!("{}", id);
          let dcmd = dcmd.parametrise(args);
          let dcmd = scope::dcommand(&symbols, &mut Vec::new(), dcmd);
          if symbols.insert(id, ()).is_some() {
            panic!("symbol redeclaration");
          };
        }

        b.data().offset(remaining)
      }
    };
    b.consume(consumed);
  }
  Ok (())
}


fn main() {
  let mut args = std::env::args();
  let _ = args.next().expect("first arg is program path");
  let filename = args.next().expect("please pass a file path as first argument");
  run(&filename).expect("should parse file correctly");
}
