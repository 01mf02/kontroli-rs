use core::fmt::{self, Display};
use logos::{Filter, Lexer, Logos};

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
    Space,

    #[token("(;", comment1)]
    Comment(usize),

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    Error,
}

impl<S> Token<S> {
    pub fn map<T>(self, f: impl Fn(S) -> T) -> Token<T> {
        use Token::*;
        match self {
            Def => Def,
            Thm => Thm,
            LBrk => LBrk,
            RBrk => RBrk,
            LPar => LPar,
            RPar => RPar,
            Colon => Colon,
            ColonEq => ColonEq,
            Arrow => Arrow,
            FatArrow => FatArrow,
            LongArrow => LongArrow,
            Comma => Comma,
            Dot => Dot,
            Period => Period,
            Ident(s) => Ident(f(s)),
            Space => Space,
            Comment(o) => Comment(o),
            Error => Error,
        }
    }
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
            Self::Dot | Self::Period => ".".fmt(f),
            Self::Ident(s) => s.fmt(f),
            Self::Space | Self::Comment(_) => " ".fmt(f),
            Self::Error => Err(Default::default()),
        }
    }
}

fn ident<'s>(lex: &mut Lexer<'s, Token<&'s str>>) -> Option<&'s str> {
    let len = lex.remainder().find("|}")?;
    lex.bump(len + 2); // include len of `|}`
    Some(lex.slice())
}

fn comment1<'s>(lex: &mut Lexer<'s, Token<&'s str>>) -> Filter<usize> {
    comment(lex, 1)
}

/// Lex inside a comment until end of comment or input, return number of open comments.
pub fn comment<'s>(lex: &mut Lexer<'s, Token<&'s str>>, mut open: usize) -> Filter<usize> {
    let prefix: &[_] = &['(', ';'];
    while open > 0 {
        // go to first occurrence of either ';' or '('
        match lex.remainder().find(prefix) {
            Some(offset) => lex.bump(offset),
            None => {
                lex.bump(lex.remainder().len());
                return Filter::Emit(open);
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
    Filter::Skip
}

#[test]
fn comment_open() {
    let s = "opening .. (; closing .. ;) still one open ..";
    assert_eq!(comment1(&mut Token::lexer(s)), 1);

    let s = "closing .. ;) none open";
    assert_eq!(comment1(&mut Token::lexer(s)), 0);
}
