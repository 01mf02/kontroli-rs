//! Reduction to weak head normal form (WHNF), including rewriting.

use crate::pattern::{Miller, Pattern, TopPattern};
use crate::rule::Rule;
use crate::signature::Signature;
use crate::stack;
use crate::term::{Arg, RTerm, Term};
use lazy_st::Thunk;
use std::cell::RefCell;
use std::rc::Rc;

/// A shared lazy term constructed from a state.
#[derive(Clone)]
pub struct RTTerm(Rc<Thunk<RState, RTerm>>);

/// A shared mutable state.
///
/// We use `RefCell` instead of `Thunk` here
/// because evaluation requires a signature and
/// because we sometimes wish to access the original state.
pub type RState = Rc<RefCell<WState>>;

/// A version of `State` that tracks whether it was reduced to WHNF yet.
pub struct WState {
    state: State,
    whnfed: bool,
}

impl WState {
    fn new(state: State) -> WState {
        let whnfed = false;
        Self { state, whnfed }
    }

    /// Replace the state with its WHNF if it was not in WHNF before.
    fn whnf(&mut self, sig: &Signature) {
        if self.whnfed {
            return;
        }

        let state = std::mem::take(&mut self.state);
        self.state = state.whnf(sig);
        self.whnfed = true
    }
}

impl RTTerm {
    fn new(st: RState) -> Self {
        Self(Rc::new(Thunk::new(st)))
    }

    pub fn force(&self) -> &RTerm {
        &**self.0
    }

    /// For a list of terms, return its first term
    /// if it is convertible with all others.
    ///
    /// This is used for checking nonlinear pattern matches, because there
    /// we want to ensure that all terms that were
    /// matched with the same variable are convertible.
    pub fn all_convertible(s: Vec<Self>, sig: &Signature) -> Option<Self> {
        let mut iter = s.into_iter();
        // assure that we have at least one term
        let tm1 = iter.next()?;
        for tmn in iter {
            // the first term only gets evaluated if we have some other terms
            if !convertible(&sig, tm1.force().clone(), tmn.force().clone()) {
                return None;
            }
        }
        Some(tm1)
    }
}

// DB -> term
type Context = stack::Stack<RTTerm>;
type Stack = stack::Stack<RState>;

/// An abstract machine representing arguments applied to a substituted term.
///
/// This representation allows for the lazy evaluation of terms.
///
/// See section 5.1 of the following reference:
/// Asperti, A.; Ricciotti, W.; Sacerdoti Coen, C.; Tassi, E. (2009).
/// "A compact kernel for the calculus of inductive constructions".
/// *Sadhana*. **34**: 71–144.
/// doi: [10.1007/s12046-009-0003-3](https://doi.org/10.1007%2Fs12046-009-0003-3).
///
#[derive(Clone, Default)]
pub struct State {
    pub ctx: Context,
    pub term: RTerm,
    pub stack: Stack,
}

impl State {
    /// Construct a new state from a reference to a term.
    ///
    /// This does not yet evaluate anything.
    pub fn new(term: RTerm) -> Self {
        Self {
            ctx: Context::new(),
            term,
            stack: Stack::new(),
        }
    }

    /// Evaluate the state to its weak head normal form.
    pub fn whnf(self, sig: &Signature) -> Self {
        use Term::*;
        let Self {
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
                        term = ctm.force().clone();
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
                        stack.push(Rc::new(RefCell::new(WState::new(st))))
                    }
                    term = head.clone();
                }
                Symb(s) => match &sig.rules.get(&s) {
                    None => break,
                    Some(rules) => {
                        match rules
                            .iter()
                            .filter_map(|r| Some((r.match_stack(&stack, sig)?, r)))
                            .next()
                        {
                            None => break,
                            Some((subst, rule)) => {
                                trace!("rewrite: {} ... ⟶ {}", s, rule);
                                ctx = subst;
                                term = rule.rhs.clone();
                                stack.pop_many(rule.lhs.args.len());
                            }
                        }
                    }
                },
            }
        }

        State { ctx, term, stack }
    }
}

impl RTerm {
    fn psubst(self, args: &Context) -> Self {
        if args.is_empty() {
            self
        } else {
            self.apply_subst(&psubst(args), 0)
        }
    }
}

fn psubst(args: &Context) -> impl Fn(usize, usize) -> RTerm + '_ {
    move |n: usize, k: usize| match args.get(n - k) {
        Some(arg) => arg.force().clone() << k,
        None => RTerm::new(Term::BVar(n - args.len())),
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
                rstate.borrow_mut().whnf(sig);
                let state = &rstate.borrow().state;
                match &*state.term {
                    Term::Symb(st) => {
                        // The stack and pattern length have to be equal,
                        // to exclude pattern matches like `f (g a) ~ f g`.
                        // This is unlike `TopPattern::match_stack`, which
                        // allows matches like `add 0 n ~ add 0`.
                        if sp == st && state.stack.len() == pats.len() {
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
            Self::MVar(m, ctx) => {
                if ctx.depth == 0 {
                    Box::new(std::iter::once(Some((*m, rstate))))
                } else {
                    todo!()
                }
            }
            Self::Joker => Box::new(std::iter::empty()),
            Self::Abst(_, pat) => todo!(),
            Self::BVar(db, pats) => todo!(),
        }
    }
}

impl TopPattern {
    pub fn match_stack<'a>(
        &'a self,
        stack: &'a Stack,
        sig: &'a Signature,
    ) -> Box<dyn Iterator<Item = Option<(Miller, RState)>> + 'a> {
        if stack.len() < self.args.len() {
            // we do not have enough arguments on the stack to match against
            return Box::new(std::iter::once(None));
        }

        Box::new(
            self.args
                .iter()
                .zip(stack.iter())
                .map(move |(pat, rstate)| pat.match_state(rstate.clone(), sig))
                .flatten(),
        )
    }
}

impl Rule {
    /// ~~~
    /// # use kontroli::{Prerule, Preterm, Rule, RTerm, Signature, Symbol, Symbols};
    /// # use kontroli::reduce::State;
    /// # use std::convert::TryFrom;
    /// let syms: Symbols = vec!("id", "f", "a").into_iter().collect();
    /// let sig: Signature = Default::default();
    /// let mk_rule = |s: &str| Rule::try_from(Prerule::try_from(s).unwrap().scope(&syms).unwrap()).unwrap();
    /// let mk_term = |s: &str| Preterm::try_from(s).unwrap().scope_closed(&syms).unwrap();
    /// let rule = mk_rule("[A] id A --> A.");
    /// let term = mk_term("id f a.");
    /// let stack = State::new(RTerm::new(term)).whnf(&sig).stack;
    /// let subst = rule.match_stack(&stack, &sig).unwrap();
    /// let subst = subst.iter().map(|rtt| (**rtt.force()).clone());
    /// assert_eq!(vec!(mk_term("f.")), subst.collect::<Vec<_>>())
    /// ~~~
    pub fn match_stack(&self, stack: &Stack, sig: &Signature) -> Option<Context> {
        let mut subst = vec![vec![]; self.ctx.len()];
        for i in self.lhs.match_stack(stack, sig) {
            let (m, st1) = i?;
            subst.get_mut(m.0).expect("subst").push(RTTerm::new(st1))
        }
        subst
            .into_iter()
            .rev()
            .map(|s| RTTerm::all_convertible(s, sig))
            .collect()
    }
}

impl lazy_st::Evaluate<RTerm> for RState {
    fn evaluate(self) -> RTerm {
        RTerm::from(self)
    }
}

impl From<RState> for RTerm {
    fn from(s: RState) -> Self {
        RTerm::from(s.borrow().state.clone())
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

fn conversion_step(cn: (RTerm, RTerm), cns: &mut Vec<(RTerm, RTerm)>, eta: bool) -> bool {
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
        (a, Abst(_, b)) | (Abst(_, b), a) if eta => {
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
                    if !conversion_step(cn, &mut cns, sig.eta) {
                        break false;
                    }
                }
            }
            None => break true,
        }
    }
}
