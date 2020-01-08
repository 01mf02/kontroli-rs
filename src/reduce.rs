use super::*;
use lazy_st::{lazy, Lazy};
use std::rc::Rc;

// symbol -> type
pub type Signature = FnvHashMap<String, Term>;

type Context = Vec<Rc<Lazy<Term>>>;
type Stack = Vec<Lazy<Term>>;

pub struct State(Context, Term, Stack);

impl State {
    pub fn new(tm: Term) -> Self {
        State(Vec::new(), tm, Vec::new())
    }

    pub fn whnf(self, sig: &Signature) -> State {
        use Term::*;
        let State(mut ctx, mut tm, mut stack) = self;
        loop {
            match tm {
                Type | Kind | Prod(_, _) => break,
                BVar(x) => {
                    tm = if x < ctx.len() {
                        (**ctx.get(x).expect("get")).clone()
                    } else {
                        BVar(x - ctx.len())
                    };
                    ctx.clear();
                }
                Abst(a, t) => match stack.pop() {
                    None => {
                        tm = Abst(a, t);
                        break;
                    }
                    Some(p) => {
                        tm = *t;
                        ctx.push(Rc::new(p));
                    }
                },
                Appl(head, tail) => {
                    tm = *head;
                    for t in tail.into_iter().rev() {
                        let st = State(ctx.clone(), *t, Vec::new());
                        stack.push(lazy!(Term::from(st)))
                    }
                }
                Symb(s) => unimplemented!(),
            }
        }

        State(ctx, tm, stack)
    }
}

impl From<State> for Term {
    fn from(State(ctx, tm, stack): State) -> Self {
        let mut t = if ctx.is_empty() { tm } else { unimplemented!() };
        let args = stack.into_iter().map(|la| Box::new(la.unwrap())).collect();
        t.apply(args);
        t
    }
}

fn conversion_step(cn: (Term, Term), cns: &mut Vec<(Term, Term)>) -> bool {
    use Term::*;

    match cn {
        (Kind, Kind) | (Type, Type) => true,
        (Symb(s1), Symb(s2)) if s1 == s2 => true,
        (BVar(v1), BVar(v2)) if v1 == v2 => true,
        (Abst(_, t1), Abst(_, t2)) => {
            cns.push((*t1, *t2));
            true
        }
        (Prod((_, Some(ty1)), tm1), Prod((_, Some(ty2)), tm2)) => {
            cns.push((*ty1, *ty2));
            cns.push((*tm1, *tm2));
            true
        }
        // TODO: only check this if eta-equivalence is enabled
        (a, Abst(_, b)) | (Abst(_, b), a) => {
            let mut shifted = a << 1;
            shifted.apply(vec![Box::new(BVar(0))]);
            cns.push((*b, shifted));
            true
        }
        (Appl(f1, args1), Appl(f2, args2)) => {
            if args1.len() == args2.len() {
                let unbox = |a: Vec<BTerm>| a.into_iter().map(|a| *a);
                cns.push((*f1, *f2));
                cns.extend(unbox(args1).zip(unbox(args2)));
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

pub fn convertible(sig: &Signature, tm1: Term, tm2: Term) -> bool {
    let mut cns = vec![(tm1, tm2)];
    loop {
        match cns.pop() {
            Some((tm1, tm2)) => {
                if tm1 != tm2 {
                    let cn = (tm1.whnf(sig), tm2.whnf(sig));
                    if !conversion_step(cn, &mut cns) {
                        break false;
                    }
                }
            }
            None => break true,
        }
    }
}
