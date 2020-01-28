use super::*;

type Miller = usize;

pub enum Pattern {
    MVar(Miller, Vec<DeBruijn>),
    Abst(Option<String>, Box<Pattern>),
    Symb(String, Vec<Pattern>),
    BVar(DeBruijn, Vec<Pattern>),
}

impl Pattern {
    pub fn is_de_bruijn(&self) -> Option<DeBruijn> {
        match self {
            Pattern::BVar(idx, args) if args.is_empty() => Some(*idx),
            _ => None,
        }
    }
}

impl From<Term> for Pattern {
    fn from(tm: Term) -> Self {
        use Term::*;
        match tm {
            Appl(head, mut args) => match *head {
                Symb(s) => Pattern::Symb(s, args.into_iter().map(Self::from).collect()),
                Appl(head2, mut args2) => {
                    args2.append(&mut args);
                    Self::from(Appl(head2, args2))
                }
                _ => unimplemented!(),
            },
            Symb(s) => Pattern::Symb(s, Vec::new()),
            // TODO: warn if arg.type given?
            Abst(arg, tm) => Pattern::Abst(arg.id, Box::new(Self::from(*tm))),
            _ => unimplemented!(),
        }
    }
}

pub struct Rule {
    pub ctx: Vec<String>,
    pub pat: Pattern,
    pub rhs: Term,
}
