use crate::pattern::Miller;
use crate::rule::Rule;
use crate::signature::Signature;
use crate::stack;
use crate::term::{Arg, Term};
use lazy_st::Thunk;
use std::cell::RefCell;
use std::rc::Rc;

pub type RState = Rc<RefCell<State>>;

// DB -> term
type Context = stack::Stack<Rc<Thunk<RState, Term>>>;
type Stack = stack::Stack<RState>;

#[derive(Clone, Default)]
pub struct State(Context, Term, Stack);

impl State {
    pub fn new(tm: Term) -> Self {
        State(Context::new(), tm, Stack::new())
    }

    pub fn whnf(self, sig: &Signature) -> State {
        use Term::*;
        let State(mut ctx, mut tm, mut stack) = self;
        loop {
            trace!("whnf: {}", tm);
            match tm {
                Type | Kind | Prod(_, _) => break,
                BVar(x) => match ctx.get(x) {
                    Some(ctm) => {
                        tm = (**ctm).clone();
                        ctx.clear()
                    }
                    None => {
                        tm = BVar(x - ctx.len());
                        ctx.clear();
                        break;
                    }
                },
                Abst(a, t) => match stack.pop() {
                    None => {
                        tm = Abst(a, t);
                        break;
                    }
                    Some(p) => {
                        tm = *t;
                        ctx.push(Rc::new(Thunk::new(p)));
                    }
                },
                Appl(head, tail) => {
                    tm = *head;
                    for t in tail.into_iter().rev() {
                        let st = State(ctx.clone(), t, Stack::new());
                        stack.push(Rc::new(RefCell::new(st)))
                    }
                }
                Symb(s) => {
                    let rules = &sig.get(&s).expect("symbol info").rules;
                    match rules
                        .iter()
                        .filter_map(|r| Some((r.match_stack(&stack, sig)?, r.args.len())))
                        .next()
                    {
                        None => {
                            tm = Symb(s);
                            break;
                        }
                        Some((rhs, consumed)) => {
                            trace!("rewrite: {} ... ⟶ {}", s, rhs);
                            tm = rhs;
                            stack.pop_many(consumed)
                        }
                    }
                }
            }
        }

        State(ctx, tm, stack)
    }
}

impl Term {
    pub fn psubst(self, args: &Context) -> Term {
        self.apply_subst(&psubst(args), 0)
    }

    pub fn psubst2(self, args: Vec<&Term>) -> Term {
        self.apply_subst(&psubst2(args), 0)
    }
}

fn psubst(args: &Context) -> impl Fn(usize, usize) -> Term + '_ {
    move |n: usize, k: usize| {
        match args.get(n - k) {
            // TODO: if shifting turns out to be a performance bottleneck,
            // switch to a shift-memoised version as in Dedukti
            Some(arg) => (**arg).clone() << k,
            None => Term::BVar(n - args.len()),
        }
    }
}

fn psubst2(args: Vec<&Term>) -> impl Fn(usize, usize) -> Term + '_ {
    move |n: usize, k: usize| {
        match args.get(n - k) {
            // TODO: if shifting turns out to be a performance bottleneck,
            // switch to a shift-memoised version as in Dedukti
            Some(arg) => (*arg).clone() << k,
            None => Term::BVar(n - args.len()),
        }
    }
}

impl Rule {
    pub fn match_stack(&self, stack: &Stack, sig: &Signature) -> Option<Term> {
        let mut subst = std::collections::HashMap::new();
        for (pat, rstate) in self.args.iter().zip(stack.iter()) {
            rstate.replace_with(|state| std::mem::take(state).whnf(sig));
            // TODO: match pattern with state, not term!
            pat.match_term(Term::from(rstate.borrow().clone()), sig, &mut subst)?;
        }
        let subst: Option<_> = (0..self.ctx.len())
            .map(|i| subst.get(&Miller(i)))
            .rev()
            .collect();
        Some(self.rhs.clone().psubst2(subst?))
    }
}
impl lazy_st::Evaluate<Term> for RState {
    fn evaluate(self) -> Term {
        Term::from(self)
    }
}

impl From<RState> for Term {
    fn from(s: RState) -> Self {
        Term::from(s.borrow().clone())
    }
}

impl From<State> for Term {
    fn from(State(ctx, tm, stack): State) -> Self {
        let t = if ctx.is_empty() { tm } else { tm.psubst(&ctx) };
        let args = stack.into_iter().map(Term::from).collect();
        t.apply(args)
    }
}

impl Term {
    pub fn whnf(self, sig: &Signature) -> Self {
        trace!("whnf of {}", self);
        Term::from(State::new(self).whnf(sig))
    }
}

const ETA: bool = true;

fn conversion_step(cn: (Term, Term), cns: &mut Vec<(Term, Term)>) -> bool {
    use Term::*;

    match cn {
        (Kind, Kind) | (Type, Type) => true,
        (Symb(s1), Symb(s2)) => s1 == s2,
        (BVar(v1), BVar(v2)) => v1 == v2,
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
                trace!("convertible: {} ~? {}", tm1, tm2);
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
