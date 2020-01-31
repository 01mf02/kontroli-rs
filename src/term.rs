#[derive(Clone, Debug, PartialEq)]
pub struct Arg {
    pub id: Option<String>,
    pub ty: Option<BTerm>,
}

pub type BTerm = Box<Term>;

pub type DeBruijn = usize;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Kind,
    Type,
    Symb(String),
    BVar(DeBruijn),
    Appl(BTerm, Vec<Term>),
    Abst(Arg, BTerm),
    Prod(Arg, BTerm),
}

impl Term {
    pub fn absts(self, args: Vec<Arg>) -> Term {
        args.into_iter()
            .rev()
            .fold(self, |acc, arg| Term::Abst(arg, Box::new(acc)))
    }
    pub fn prods(self, args: Vec<Arg>) -> Term {
        args.into_iter()
            .rev()
            .fold(self, |acc, arg| Term::Prod(arg, Box::new(acc)))
    }

    pub fn appl(self, args: Vec<Term>) -> Self {
        if args.is_empty() {
            self
        } else {
            Term::Appl(Box::new(self), args)
        }
    }

    pub fn apply(mut self, mut args: Vec<Term>) -> Term {
        if args.is_empty() {
            self
        } else {
            match self {
                Self::Appl(_, ref mut args1) => {
                    args1.append(&mut args);
                    self
                }
                _ => Self::Appl(Box::new(self), args),
            }
        }
    }
}
