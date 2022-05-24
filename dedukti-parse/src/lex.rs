use crate::Symb;
use core::fmt::{self, Display};
use logos::{Filter, Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Eq)]
#[logos(type S = &str)]
pub enum Token<S> {
    #[token("def")]
    Def,

    #[token("thm")]
    Thm,

    #[token("[")]
    LBrk,

    #[token("]")]
    RBrk,

    #[token("(")]
    LPar,

    #[token(")")]
    RPar,

    #[token(":")]
    Colon,

    #[token(":=")]
    ColonEq,

    #[token("->")]
    Arrow,

    #[token("=>")]
    FatArrow,

    #[token("-->")]
    LongArrow,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[regex("[a-zA-Z0-9_!?][a-zA-Z0-9_!?']*", symb)]
    #[token("{|", moustache)]
    Symb(Symb<S>),

    #[token("(;", comment1)]
    Comment(usize),

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[error]
    Error,
}

impl<S: Display> Display for Token<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Def => "def".fmt(f),
            Self::Thm => "thm".fmt(f),
            Self::LBrk => "[".fmt(f),
            Self::RBrk => "]".fmt(f),
            Self::LPar => "(".fmt(f),
            Self::RPar => ")".fmt(f),
            Self::Colon => ":".fmt(f),
            Self::ColonEq => ":=".fmt(f),
            Self::Arrow => "->".fmt(f),
            Self::FatArrow => "=>".fmt(f),
            Self::LongArrow => "-->".fmt(f),
            Self::Comma => ",".fmt(f),
            Self::Dot => ".".fmt(f),
            Self::Symb(s) => s.fmt(f),
            Self::Comment(_) => " ".fmt(f),
            Self::Error => Err(Default::default()),
        }
    }
}

fn symb<'s>(lex: &mut Lexer<'s, Token<&'s str>>) -> Option<Symb<&'s str>> {
    let mut symb = Symb::new(lex.slice());

    // regular expressions for head and tail of identifiers
    let ih = |c| matches!(c, 'a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9' | '_' | '!' | '?');
    let it = |c| matches!(c, 'a' ..= 'z' | 'A' ..= 'Z' | '0' ..= '9' | '_' | '!' | '?' | '\'');

    while let Some(after_dot) = lex.remainder().strip_prefix('.') {
        let len = if let Some(tail) = after_dot.strip_prefix(ih) {
            1 + tail.find(|c| !it(c)).unwrap_or(tail.len())
        } else if let Some(after_moustache) = after_dot.strip_prefix("{|") {
            2 + after_moustache.find("|}")? + 2
        } else {
            break;
        };

        symb.push(&after_dot[..len]);
        lex.bump(1 + len); // eat the dot
    }

    Some(symb)
}

fn moustache<'s>(lex: &mut Lexer<'s, Token<&'s str>>) -> Option<Symb<&'s str>> {
    let len = lex.remainder().find("|}")?;
    lex.bump(len + 2); // include len of `|}`
    symb(lex)
}

fn comment1<'s>(lex: &mut Lexer<'s, Token<&'s str>>) -> Filter<usize> {
    match comment(lex, 1) {
        0 => Filter::Skip,
        n => Filter::Emit(n),
    }
}

/// Lex inside a comment until end of comment or input, return number of open comments.
pub fn comment<'s>(lex: &mut Lexer<'s, Token<&'s str>>, mut open: usize) -> usize {
    let prefix: &[_] = &['(', ';'];
    while open > 0 {
        // go to first occurrence of either ';' or '('
        match lex.remainder().find(prefix) {
            Some(offset) => lex.bump(offset),
            None => {
                lex.bump(lex.remainder().len());
                return open;
            }
        };
        if lex.remainder().starts_with("(;") {
            open += 1;
            lex.bump(2);
        } else if lex.remainder().starts_with(";)") {
            open -= 1;
            lex.bump(2);
        } else {
            lex.bump(1);
        }
    }
    0
}

#[test]
fn comment_open() {
    let s = "opening .. (; closing .. ;) still one open ..";
    assert!(matches!(comment1(&mut Token::lexer(s)), Filter::Emit(1)));

    let s = "closing .. ;) none open";
    assert!(matches!(comment1(&mut Token::lexer(s)), Filter::Skip));
}
