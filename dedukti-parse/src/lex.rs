use core::fmt::{self, Display};
use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq)]
pub enum Token<'s> {
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

    #[regex("[a-zA-Z0-9_!?][a-zA-Z0-9_!?']*")]
    #[token("{|", ident)]
    Ident(&'s str),

    #[regex(r"[ \t\n\f]+")]
    #[token("(;", comment)]
    Space,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    Error,
}

impl<'s> Display for Token<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let s = match self {
            Self::Def => "def",
            Self::Thm => "thm",
            Self::LBrk => "[",
            Self::RBrk => "]",
            Self::LPar => "(",
            Self::RPar => ")",
            Self::Colon => ":",
            Self::ColonEq => ":=",
            Self::Arrow => "->",
            Self::FatArrow => "=>",
            Self::LongArrow => "-->",
            Self::Comma => ",",
            Self::Dot => ".",
            Self::Ident(s) => s,
            Self::Space => " ",
            Self::Error => return Err(Default::default()),
        };
        s.fmt(f)
    }
}

fn ident<'s>(lex: &mut Lexer<'s, Token<'s>>) -> Option<&'s str> {
    let len = lex.remainder().find("|}")?;
    lex.bump(len + 2); // include len of `|}`
    Some(lex.slice())
}

fn comment<'s>(lex: &mut Lexer<'s, Token<'s>>) -> Option<()> {
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
