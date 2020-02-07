#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Binder {
    Lam,
    Pi,
}

#[derive(Debug, Clone)]
pub struct GArg<T> {
    pub id: Option<String>,
    pub ty: Option<T>,
}

pub type BPreterm = Box<Preterm>;

pub type Prearg = GArg<BPreterm>;

#[derive(Debug, Clone)]
pub enum Preterm {
    Type,
    Symb(String),
    Appl(BPreterm, Vec<Preterm>),
    Bind(Binder, GArg<BPreterm>, BPreterm),
}

impl Preterm {
    fn bind_many(self, binders: Vec<(Binder, Prearg)>) -> Self {
        binders.into_iter().rev().fold(self, |acc, (binder, arg)| {
            Self::Bind(binder, arg, Box::new(acc))
        })
    }

    pub fn absts(self, args: Vec<Prearg>) -> Self {
        self.bind_many(args.into_iter().map(|arg| (Binder::Lam, arg)).collect())
    }
    pub fn prods(self, args: Vec<Prearg>) -> Self {
        self.bind_many(args.into_iter().map(|arg| (Binder::Pi, arg)).collect())
    }

    pub fn apply(mut self, mut args: Vec<Self>) -> Self {
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
