use super::*;
use crate::signature::Signature;
use lazy_st::{lazy, Lazy};
use std::rc::Rc;

type Context = Vec<Rc<Lazy<Term>>>;
pub type Stack = Vec<Lazy<Term>>;

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
                    tm = match ctx.get(x) {
                        Some(tm) => (**tm).clone(),
                        None => BVar(x - ctx.len()),
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
                        let st = State(ctx.clone(), t, Vec::new());
                        stack.push(lazy!(Term::from(st)))
                    }
                }
                Symb(s) => {
                    let rules = &sig.get(&s).expect("symbol info").rules;
                    match rules.iter().filter_map(|r| r.match_stack(&stack, sig)).next() {
                        None => (),
                        Some(subst) => unimplemented!()
                    }
                }
            }
        }

        State(ctx, tm, stack)
    }
}

impl Rule {
    pub fn match_stack(&self, stack: &Stack, sig: &Signature) -> Option<rule::Subst> {
        unimplemented!()
    }

}

impl From<State> for Term {
    fn from(State(ctx, tm, stack): State) -> Self {
        let t = if ctx.is_empty() { tm } else { tm.psubst(&ctx) };
        let args = stack.into_iter().map(|la| la.unwrap()).collect();
        t.apply(args)
    }
}

impl Term {
    pub fn whnf(self, sig: &Signature) -> Self {
        Term::from(State::new(self).whnf(sig))
    }
}

const ETA: bool = true;

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
        (Prod(Arg { ty: Some(ty1), .. }, tm1), Prod(Arg { ty: Some(ty2), .. }, tm2)) => {
            cns.push((*ty1, *ty2));
            cns.push((*tm1, *tm2));
            true
        }
        (a, Abst(_, b)) | (Abst(_, b), a) if ETA => {
            cns.push((*b, (a << 1).apply(vec![BVar(0)])));
            true
        }
        (Appl(f1, args1), Appl(f2, args2)) => {
            if args1.len() == args2.len() {
                cns.push((*f1, *f2));
                cns.extend(args1.into_iter().zip(args2));
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
