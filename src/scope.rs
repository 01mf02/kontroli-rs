use fnv::FnvHashMap;

use super::*;

pub type SymTable = FnvHashMap<String, ()>;
type Bound = Vec<String>;

pub fn bind<X, A, F>(bnd: &mut Vec<X>, arg: Option<X>, f: F) -> A
where
    F: FnOnce(&mut Vec<X>) -> A,
{
    match arg {
        Some(id) => {
            bnd.push(id);
            let x = f(bnd);
            bnd.pop();
            x
        }
        None => f(bnd),
    }
}

impl Term {
    fn scope(self, sym: &SymTable, bnd: &mut Bound) -> Self {
        use super::Term::*;
        match self {
            Kind => Kind,
            Type => Type,
            Symb(s) => match bnd.iter().rev().position(|id| *id == s) {
                Some(idx) => BVar(idx),
                None => {
                    if sym.contains_key(&s) {
                        Symb(s)
                    } else {
                        panic!("undeclared symbol")
                    }
                }
            },
            Appl(head, tail) => Appl(
                Box::new(head.scope(sym, bnd)),
                tail.into_iter()
                    .map(|tm| Box::new(tm.scope(sym, bnd)))
                    .collect(),
            ),
            Abst(arg, tm) => {
                let arg = arg.scope(sym, bnd);
                bind(bnd, arg.id.clone(), |bnd| {
                    Abst(arg, Box::new(tm.scope(sym, bnd)))
                })
            }
            Prod(arg, tm) => {
                let arg = arg.scope(sym, bnd);
                bind(bnd, arg.id.clone(), |bnd| {
                    Prod(arg, Box::new(tm.scope(sym, bnd)))
                })
            }
            BVar(_) => panic!("found bound variable during scoping"),
        }
    }
}

impl Arg {
    fn scope(self, sym: &SymTable, bnd: &mut Bound) -> Arg {
        Arg {
            id: self.id,
            ty: self.ty.map(|ty| Box::new(ty.scope(sym, bnd))),
        }
    }
}

impl DCommand {
    pub fn scope(self, sym: &SymTable, bnd: &mut Bound) -> Self {
        self.map_type(|tm| Box::new(tm.scope(sym, bnd)))
            .map_term(|tm| Box::new(tm.scope(sym, bnd)))
    }
}
