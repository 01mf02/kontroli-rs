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

fn term(sym: &SymTable, bnd: &mut Bound, tm: Term) -> Term {
    use super::Term::*;
    match tm {
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
            Box::new(term(sym, bnd, *head)),
            tail.into_iter()
                .map(|tm| Box::new(term(sym, bnd, *tm)))
                .collect(),
        ),
        Abst(arg, tm) => {
            let arg = argument(sym, bnd, arg);
            bind(bnd, arg.0.clone(), |bnd| {
                Abst(arg, Box::new(term(sym, bnd, *tm)))
            })
        }
        Prod(arg, tm) => {
            let arg = argument(sym, bnd, arg);
            bind(bnd, arg.0.clone(), |bnd| {
                Prod(arg, Box::new(term(sym, bnd, *tm)))
            })
        }
        BVar(_) => panic!("found bound variable during scoping"),
    }
}

fn argument(sym: &SymTable, bnd: &mut Bound, (id, tm): Arg) -> Arg {
    (id, tm.map(|tm| Box::new(term(sym, bnd, *tm))))
}

pub fn dcommand(sym: &SymTable, bnd: &mut Bound, dcmd: DCommand) -> DCommand {
    dcmd.map_type(|tm| Box::new(term(sym, bnd, *tm)))
        .map_term(|tm| Box::new(term(sym, bnd, *tm)))
}
