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
                let arg = argument(sym, bnd, arg);
                bind(bnd, arg.0.clone(), |bnd| {
                    Abst(arg, Box::new(tm.scope(sym, bnd)))
                })
            }
            Prod(arg, tm) => {
                let arg = argument(sym, bnd, arg);
                bind(bnd, arg.0.clone(), |bnd| {
                    Prod(arg, Box::new(tm.scope(sym, bnd)))
                })
            }
            BVar(_) => panic!("found bound variable during scoping"),
        }
    }
}

fn argument(sym: &SymTable, bnd: &mut Bound, (id, tm): Arg) -> Arg {
    (id, tm.map(|tm| Box::new(tm.scope(sym, bnd))))
}

impl DCommand {
    pub fn scope(self, sym: &SymTable, bnd: &mut Bound) -> Self {
        self.map_type(|tm| Box::new(tm.scope(sym, bnd)))
            .map_term(|tm| Box::new(tm.scope(sym, bnd)))
    }
}
