use crate::pattern::{Miller, Pattern};
use crate::rule::Rule;
use crate::signature::Signature;
use crate::stack;
use crate::term::{Arg, RTerm, Term};
use lazy_st::Thunk;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone)]
pub struct RTTerm(Rc<Thunk<RState, RTerm>>);
pub type RState = Rc<RefCell<State>>;

impl RTTerm {
    fn new(st: RState) -> Self {
        Self(Rc::new(Thunk::new(st)))
    }
}

impl std::ops::Deref for RTTerm {
    type Target = RTerm;

    fn deref(&self) -> &Self::Target {
        &**self.0
    }
}

// DB -> term
type Context = stack::Stack<RTTerm>;
type Stack = stack::Stack<RState>;

#[derive(Clone, Default)]
pub struct State {
    ctx: Context,
    term: RTerm,
    stack: Stack,
}

impl State {
    pub fn new(term: RTerm) -> Self {
        State {
            ctx: Context::new(),
            term,
            stack: Stack::new(),
        }
    }

    pub fn whnf(self, sig: &Signature) -> Self {
        use Term::*;
        let State {
            mut ctx,
            mut term,
            mut stack,
        } = self;
        loop {
            trace!("whnf: {}", term);
            match &*term {
                Type | Kind | Prod(_, _) => break,
                BVar(x) => match ctx.get(*x) {
                    Some(ctm) => {
                        term = (**ctm).clone();
                        ctx.clear()
                    }
                    None => {
                        if !ctx.is_empty() {
                            term = RTerm::new(BVar(x - ctx.len()));
                            ctx.clear();
                        }
                        break;
                    }
                },
                Abst(a, t) => match stack.pop() {
                    None => break,
                    Some(p) => {
                        term = t.clone();
                        ctx.push(RTTerm::new(p));
                    }
                },
                Appl(head, tail) => {
                    for t in tail.iter().rev() {
                        let st = State {
                            ctx: ctx.clone(),
                            term: t.clone(),
                            stack: Stack::new(),
                        };
                        stack.push(Rc::new(RefCell::new(st)))
                    }
                    term = head.clone();
                }
                Symb(s) => {
                    let rules = &sig.get(&s).expect("symbol info").rules;
                    match rules
                        .iter()
                        .filter_map(|r| Some((r.match_stack(&stack, sig)?, r)))
                        .next()
                    {
                        None => break,
                        Some((subst, rule)) => {
                            trace!("rewrite: {} ... ⟶ {}", s, rule);
                            term = rule.rhs.clone();
                            for i in subst.into_iter().rev() {
                                ctx.push(i)
                            }
                            stack.pop_many(rule.args.len());
                        }
                    }
                }
            }
        }

        State { ctx, term, stack }
    }
}

impl RTerm {
    pub fn psubst(self, args: &Context) -> Self {
        if args.is_empty() {
            self
        } else {
            self.apply_subst(&psubst(args), 0)
        }
    }
}

// TODO: find out how often k is 0,
// and if it is often 0, then make the closure return RTerm
fn psubst(args: &Context) -> impl Fn(usize, usize) -> Term + '_ {
    move |n: usize, k: usize| match args.get(n - k) {
        Some(arg) => (***arg).clone() << k,
        None => Term::BVar(n - args.len()),
    }
}

// TODO: move to "matching.rs"?

impl Pattern {
    fn match_state<'a>(
        &'a self,
        rstate: RState,
        sig: &'a Signature,
    ) -> Box<dyn Iterator<Item = Option<(Miller, RState)>> + 'a> {
        match self {
            Self::Symb(sp, pats) => {
                rstate.replace_with(|state| std::mem::take(state).whnf(sig));
                let state = rstate.borrow();
                match &*state.term {
                    Term::Symb(st) => {
                        if sp == st && state.stack.len() >= pats.len() {
                            Box::new(
                                pats.iter()
                                    .zip(state.stack.clone())
                                    .map(move |(pat, rst)| pat.match_state(rst, sig))
                                    .flatten(),
                            )
                        } else {
                            Box::new(std::iter::once(None))
                        }
                    }
                    _ => Box::new(std::iter::once(None)),
                }
            }
            Self::MVar(m, dbs) => {
                if dbs.is_empty() {
                    Box::new(std::iter::once(Some((*m, rstate))))
                } else {
                    todo!()
                }
            }
            // TODO: really empty?
            Self::Joker => Box::new(std::iter::empty()),
            _ => todo!(),
        }
    }
}

fn nonlinearity(s: Vec<RTTerm>, sig: &Signature) -> Option<RTTerm> {
    let mut iter = s.into_iter();
    // assure that at least one term was matched
    let st1 = iter.next()?;
    // nonlinearity
    for stn in iter {
        if !convertible(&sig, (*st1).clone(), (*stn).clone()) {
            return None;
        }
    }
    Some(st1)
}

impl Rule {
    pub fn match_stack(&self, stack: &Stack, sig: &Signature) -> Option<Vec<RTTerm>> {
        let iter = self
            .args
            .iter()
            .zip(stack.iter())
            .map(|(pat, rstate)| pat.match_state(rstate.clone(), sig))
            .flatten();
        let mut subst = vec![vec![]; self.ctx.len()];
        for i in iter {
            let (m, st1) = i?;
            subst.get_mut(m.0).expect("subst").push(RTTerm::new(st1))
        }
        subst.into_iter().map(|s| nonlinearity(s, sig)).collect()
    }
}

impl lazy_st::Evaluate<RTerm> for RState {
    fn evaluate(self) -> RTerm {
        RTerm::from(self)
    }
}

impl From<RState> for RTerm {
    fn from(s: RState) -> Self {
        RTerm::from(s.borrow().clone())
    }
}

impl From<State> for RTerm {
    fn from(state: State) -> Self {
        state
            .term
            .psubst(&state.ctx)
            .apply(state.stack.into_iter().map(Self::from).collect())
    }
}

impl RTerm {
    pub fn whnf(self, sig: &Signature) -> Self {
        trace!("whnf of {}", self);
        Self::from(State::new(self).whnf(sig))
    }
}

const ETA: bool = true;

fn conversion_step(cn: (RTerm, RTerm), cns: &mut Vec<(RTerm, RTerm)>) -> bool {
    use Term::*;

    let (cn1, cn2) = cn;
    match (&*cn1, &*cn2) {
        (Kind, Kind) | (Type, Type) => true,
        (Symb(s1), Symb(s2)) => s1 == s2,
        (BVar(v1), BVar(v2)) => v1 == v2,
        (Abst(_, t1), Abst(_, t2)) => {
            cns.push((t1.clone(), t2.clone()));
            true
        }
        (Prod(Arg { ty: Some(ty1), .. }, tm1), Prod(Arg { ty: Some(ty2), .. }, tm2)) => {
            cns.push((ty1.clone(), ty2.clone()));
            cns.push((tm1.clone(), tm2.clone()));
            true
        }
        // TODO: make this nicer
        (a, Abst(_, b)) | (Abst(_, b), a) if ETA => {
            cns.push((
                b.clone(),
                (RTerm::new(a.clone()) << 1).apply(vec![RTerm::new(BVar(0))]),
            ));
            true
        }
        (Appl(f1, args1), Appl(f2, args2)) => {
            if args1.len() == args2.len() {
                cns.push((f1.clone(), f2.clone()));
                cns.extend(args1.clone().into_iter().zip(args2.clone()));
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

pub fn convertible(sig: &Signature, tm1: RTerm, tm2: RTerm) -> bool {
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
