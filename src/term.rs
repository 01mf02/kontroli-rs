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

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.id.as_ref().unwrap_or(&"_".to_string()))?;
        for ty in self.ty.as_ref() {
            write!(f, " : {}", ty)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Kind => write!(f, "Kind"),
            Self::Type => write!(f, "Type"),
            Self::Symb(s) => write!(f, "{}", s),
            Self::BVar(x) => write!(f, "𝕍{}", x),
            Self::Appl(head, tail) => {
                let parens = !tail.is_empty();
                if parens {write!(f, "(")?; };
                write!(f, "{}", head)?;
                for t in tail {
                    write!(f, " {}", t)?;
                }
                if parens {write!(f, ")")?; };
                Ok(())
            }
            Self::Abst(arg, tm) => write!(f, "(λ {}. {})", arg, tm),
            Self::Prod(arg, tm) => write!(f, "(Π {}. {})", arg, tm),
        }
    }
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

    pub fn get_symb_appl(self) -> Option<(String, Vec<Term>)> {
        match self {
            Self::Symb(s) => Some((s, Vec::new())),
            Self::Appl(head, args) => match *head {
                Self::Symb(s) => Some((s, args)),
                _ => None,
            },
            _ => None,
        }
    }
}
