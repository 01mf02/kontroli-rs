use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Arg {
    pub id: Option<String>,
    pub ty: Option<BTerm>,
}

pub type BTerm = Box<Term>;

pub type DeBruijn = usize;

#[derive(Debug, Clone)]
pub enum Term {
    Kind,
    Type,
    Symb(Rc<String>),
    BVar(DeBruijn),
    Appl(BTerm, Vec<Term>),
    Abst(Arg, BTerm),
    Prod(Arg, BTerm),
}

impl Default for Term {
    fn default() -> Self {
        Self::Type
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        use Term::*;
        match (self, other) {
            (Kind, Kind) | (Type, Type) => true,
            (Symb(s1), Symb(s2)) => Rc::ptr_eq(s1, s2),
            (BVar(x1), BVar(x2)) => x1 == x2,
            (Appl(f1, args1), Appl(f2, args2)) => {
                f1 == f2
                    && args1.len() == args2.len()
                    && args1.iter().zip(args2).all(|(t1, t2)| t1 == t2)
            }
            (Abst(_, tm1), Abst(_, tm2)) => tm1 == tm2,
            (Prod(arg1, tm1), Prod(arg2, tm2)) => arg1.ty == arg2.ty && tm1 == tm2,
            (_, _) => false,
        }
    }
}
impl Eq for Term {}

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.id.as_ref().unwrap_or(&"_".to_string()))?;
        if let Some(ty) = self.ty.as_ref() {
            write!(f, " : {}", ty)?;
        }
        Ok(())
    }
}

pub fn fmt_appl<H, T>(head: &H, tail: &[T], f: &mut std::fmt::Formatter) -> std::fmt::Result
where
    H: std::fmt::Display,
    T: std::fmt::Display,
{
    let parens = !tail.is_empty();
    if parens {
        write!(f, "(")?;
    };
    write!(f, "{}", head)?;
    for t in tail {
        write!(f, " {}", t)?;
    }
    if parens {
        write!(f, ")")?;
    };
    Ok(())
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Kind => write!(f, "Kind"),
            Self::Type => write!(f, "Type"),
            Self::Symb(s) => write!(f, "{}", s),
            Self::BVar(x) => write!(f, "β{}", x),
            Self::Appl(head, tail) => fmt_appl(head, tail, f),
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

    pub fn get_symb_appl(self) -> Option<(Rc<String>, Vec<Term>)> {
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
