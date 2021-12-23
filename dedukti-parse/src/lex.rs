use core::fmt::{self, Display};
use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq)]
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

    Period,

    #[regex("[a-zA-Z0-9_!?][a-zA-Z0-9_!?']*")]
    #[token("{|", ident)]
    Ident(S),

    #[regex(r"[ \t\n\f]+")]
    #[token("(;", comment)]
    Space,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    Error,
}

impl<S: Display> Display for Token<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
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
            Self::Dot | Self::Period => ".".fmt(f),
            Self::Ident(s) => s.fmt(f),
            Self::Space => " ".fmt(f),
            Self::Error => return Err(Default::default()),
        }
    }
}

fn ident<'s>(lex: &mut Lexer<'s, Token<&'s str>>) -> Option<&'s str> {
    let len = lex.remainder().find("|}")?;
    lex.bump(len + 2); // include len of `|}`
    Some(lex.slice())
}

fn comment<'s>(lex: &mut Lexer<'s, Token<&'s str>>) -> Option<()> {
    // number of open comments
    let mut open = 1;
    let prefix: &[_] = &['(', ';'];
    while open > 0 {
        // go to first occurrence of either ';' or '('
        lex.bump(lex.remainder().find(prefix)?);
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
    Some(())
}
