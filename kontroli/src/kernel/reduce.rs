//! Reduction to weak head normal form (WHNF), including rewriting.

use super::sterm::{Comb, STerm};
use super::{GCtx, Pattern, Rule, TopPattern};
use crate::pattern::Miller;
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::cell::{Ref, RefCell};
use lazy_st::Thunk;

/// An abstract machine representing arguments applied to a substituted term.
///
/// This representation allows for the lazy evaluation of terms.
///
/// See section 5.1 of the following reference:
/// Asperti, A.; Ricciotti, W.; Sacerdoti Coen, C.; Tassi, E. (2009).
/// "A compact kernel for the calculus of inductive constructions".
/// *Sadhana*. **34**: 71–144.
/// doi: [10.1007/s12046-009-0003-3](https://doi.org/10.1007%2Fs12046-009-0003-3).
#[derive(Clone)]
pub struct State<'s, 't> {
    pub ctx: Context<'s, 't>,
    pub term: STerm<'s, 't>,
    pub stack: Stack<'s, 't>,
}

impl<'s, 't> State<'s, 't> {
    /// Construct a new state from a reference to a term.
    ///
    /// This does not yet evaluate anything, as can be seen from following example:
    ///
    /// ~~~
    /// # use kontroli::{Error, Share, Symbols};
    /// # use kontroli::scope::Term as STerm;
    /// # use kontroli::rc::{GCtx, Term};
    /// # use kontroli::rc::state::State;
    /// let syms = Symbols::new();
    ///
    /// let term: Term = STerm::parse(r"(x => x) (x => x)").share(&syms)?;
    ///
    /// let state = State::new(term.clone());
    /// assert!(Term::ptr_eq(&Term::from(state), &term));
    /// # Ok::<(), Error>(())
    /// ~~~
    pub fn new(term: STerm<'s, 't>) -> Self {
        Self {
            ctx: Context::default(),
            term,
            stack: Stack::default(),
        }
    }
}

/// Map from de Bruijn indices in the term of the abstract machine to lazy terms.
#[derive(Clone, Default)]
pub struct Context<'s, 't>(Vec<RTTerm<'s, 't>>);

/// Arguments to the abstract machine term.
#[derive(Clone, Default)]
pub struct Stack<'s, 't>(Vec<RState<'s, 't>>);

/// A shared lazy term constructed from a shared mutable state.
#[derive(Clone)]
pub struct RTTerm<'s, 't>(Rc<Thunk<RState<'s, 't>, STerm<'s, 't>>>);

impl<'s, 't> RTTerm<'s, 't> {
    pub fn new(st: RState<'s, 't>) -> Self {
        Self(Rc::new(Thunk::new(st)))
    }

    /// Force evaluation of the lazy term.
    pub fn force(&self) -> &STerm<'s, 't> {
        &**self.0
    }
}

/// A shared mutable state.
///
/// We use `RefCell` instead of `Thunk` here
/// because evaluation requires a global context and
/// because we sometimes wish to access the original state.
#[derive(Clone)]
pub struct RState<'s, 't>(Rc<RefCell<WState<'s, 't>>>);

impl<'s, 't> RState<'s, 't> {
    fn new(wst: WState<'s, 't>) -> Self {
        Self(Rc::new(RefCell::new(wst)))
    }

    fn from_ctx_term(ctx: Context<'s, 't>, term: STerm<'s, 't>) -> Self {
        let stack = Stack::default();
        Self::new(WState::new(State { ctx, term, stack }))
    }
}

impl<'s, 't> lazy_st::Evaluate<STerm<'s, 't>> for RState<'s, 't> {
    fn evaluate(self) -> STerm<'s, 't> {
        STerm::from(self)
    }
}

impl<'s, 't> From<RState<'s, 't>> for STerm<'s, 't> {
    fn from(s: RState<'s, 't>) -> Self {
        STerm::from(s.borrow_state().clone())
    }
}

impl<'s, 't> From<State<'s, 't>> for STerm<'s, 't> {
    fn from(mut state: State<'s, 't>) -> Self {
        state.term.psubst(&state.ctx);
        if !state.stack.0.is_empty() {
            let args = state.stack.0.into_iter().rev().map(Self::from);
            state.term = state.term.apply(args);
        }
        state.term
    }
}

impl<'s, 't> STerm<'s, 't> {
    fn psubst(&mut self, args: &Context<'s, 't>) {
        if !args.0.is_empty() {
            self.apply_subst(&psubst(args), 0);
        }
    }
}

fn psubst<'s, 't, 'c>(args: &'c Context<'s, 't>) -> impl Fn(usize, usize) -> STerm<'s, 't> + 'c {
    move |n: usize, k: usize| match args.0.iter().rev().nth(n - k) {
        Some(arg) => arg.force().clone().shift(k),
        None => STerm::Var(n - args.0.len()),
    }
}

/// A version of `State` that tracks whether it was reduced to WHNF yet.
pub struct WState<'s, 't> {
    state: State<'s, 't>,
    whnfed: bool,
}

impl<'s, 't> WState<'s, 't> {
    fn new(state: State<'s, 't>) -> Self {
        let whnfed = false;
        Self { state, whnfed }
    }

    /// Replace the state with its WHNF if it was not in WHNF before.
    fn whnf(&mut self, gc: &'t GCtx<'s>) {
        if !self.whnfed {
            self.state.whnf(gc);
            self.whnfed = true
        }
    }
}

impl<'s, 't> RState<'s, 't> {
    /// Replace the state with its WHNF if it was not in WHNF before.
    fn whnf(&self, gc: &'t GCtx<'s>) {
        self.0.borrow_mut().whnf(gc)
    }

    /// Obtain a reference to the state.
    fn borrow_state(&self) -> Ref<State<'s, 't>> {
        Ref::map(self.0.borrow(), |wst| &wst.state)
    }
}

impl<'s, 't> State<'s, 't> {
    /// Evaluate the state to its weak head normal form.
    ///
    /// ~~~
    /// # use kontroli::{Error, Share, Symbols};
    /// # use kontroli::scope::Term as STerm;
    /// # use kontroli::rc::{GCtx, RTerm, Term};
    /// # use kontroli::rc::state::State;
    /// let gc = GCtx::new();
    /// let syms = Symbols::new();
    ///
    /// let term = STerm::parse(r"(x => x) (x => x)").share(&syms)?;
    /// let mut state = State::new(term);
    /// state.whnf(&gc);
    ///
    /// let expected = STerm::parse(r"(x => x)").share(&syms)?;
    /// assert!(state.ctx.is_empty());
    /// assert!(state.stack.is_empty());
    /// assert_eq!(state.term, expected);
    /// # Ok::<(), Error>(())
    /// ~~~
    fn whnf(&mut self, gc: &'t GCtx<'s>) {
        use STerm::*;
        loop {
            trace!("whnf: {}", self.term);
            match &self.term {
                Type | Kind => break,
                Var(x) => match self.ctx.0.iter().rev().nth(*x) {
                    Some(ctm) => {
                        self.term = ctm.force().clone();
                        self.ctx.0.clear();
                        continue;
                    }
                    None => {
                        if !self.ctx.0.is_empty() {
                            self.term = Var(x - self.ctx.0.len());
                            self.ctx.0.clear();
                        }
                        break;
                    }
                },
                Const(s) => match &gc.get_rules(s) {
                    None => break,
                    Some(rules) => {
                        match rules
                            .iter()
                            .filter_map(|r| Some((self.stack.match_flatten(r, gc)?, r)))
                            .next()
                        {
                            None => break,
                            Some((subst, rule)) => {
                                trace!("rewrite: {} ... ⟶ {}", s, rule);
                                self.ctx = subst;
                                self.term = (&rule.rhs).into();
                                let len = self.stack.0.len() - rule.lhs.args.len();
                                self.stack.0.truncate(len);
                                continue;
                            }
                        }
                    }
                },
                LComb(c) if c.is_whnf(|| self.stack.0.is_empty()) => break,
                SComb(c) if c.is_whnf(|| self.stack.0.is_empty()) => break,
                _ => (),
            };
            let comb = match core::mem::replace(&mut self.term, Kind) {
                LComb(c) => c.into(),
                SComb(c) => Rc::try_unwrap(c).unwrap_or_else(|rc| (*rc).clone()),
                _ => unreachable!(),
            };
            match comb {
                Comb::Prod(..) => unreachable!(),
                Comb::Abst(.., t) => {
                    self.term = t;
                    // unwrap is safe because `is_whnf` assures that
                    // if we have an abstraction, the stack is not empty
                    self.ctx.0.push(RTTerm::new(self.stack.0.pop().unwrap()));
                }
                Comb::Appl(head, tail) => {
                    let tail = tail.into_iter().rev();
                    let tail = tail.map(|tm| RState::from_ctx_term(self.ctx.clone(), tm));
                    self.stack.0.extend(tail);
                    self.term = head;
                }
            }
        }

        if let Var(_) = self.term {
            assert!(self.ctx.0.is_empty())
        }
    }
}

impl<'s, 't> STerm<'s, 't> {
    /// Return the weak head normal form of the term.
    pub fn whnf(self, gc: &'t GCtx<'s>) -> Self {
        trace!("whnf of {}", self);
        let mut state = State::new(self);
        state.whnf(gc);
        Self::from(state)
    }

    /// Return true if the given terms have a common redex.
    pub fn convertible(tm1: Self, tm2: Self, gc: &'t GCtx<'s>) -> bool {
        let mut cns = Vec::from([(tm1, tm2)]);
        while let Some((cn1, cn2)) = cns.pop() {
            trace!("convertible: {} ~? {}", cn1, cn2);
            use super::convertible::step;
            if cn1 != cn2 && !step((cn1.whnf(gc), cn2.whnf(gc)), &mut cns, gc.eta) {
                return false;
            }
        }
        true
    }
}

/// For a sequence of states,
/// return the term corresponding to its first state
/// if it is convertible with all other states.
///
/// This is used for checking nonlinear pattern matches, because there
/// we want to ensure that all states that were
/// matched with the same variable are convertible.
fn all_convertible<'s, 't, I>(mut iter: I, gc: &GCtx<'s>) -> Option<RTTerm<'s, 't>>
where
    I: Iterator<Item = RState<'s, 't>>,
{
    // assure that we have at least one term
    let tm = RTTerm::new(iter.next()?);
    for stn in iter {
        // the first term is only evaluated if we have some other terms
        if !STerm::convertible(tm.force().clone(), STerm::from(stn), gc) {
            return None;
        }
    }
    Some(tm)
}

impl<'s, 't> Stack<'s, 't> {
    /// Determine whether the stack of an abstract machine matches the rule's LHS.
    ///
    /// Return a new machine context containing variable assignments in case of a match.
    ///
    /// ~~~
    /// # use kontroli::rc::state::State;
    /// # use kontroli::rc::{GCtx, RTerm, Rule, Term};
    /// # use kontroli::scope::{Rule as SRule, Term as STerm};
    /// # use kontroli::{Error, Share, Symbols};
    /// let syms: Symbols = vec!["id", "f", "a"].into_iter().collect();
    /// let gc = GCtx::new();

    /// let rule = SRule::parse("[A] id A --> A.").share(&syms)?;
    /// let term = STerm::parse("id f a").share(&syms)?;

    /// let mut state = State::new(term);
    /// state.whnf(&gc);
    /// let subst = state.stack.match_flatten(&rule, &gc).unwrap();
    /// let subst = subst.iter().map(|rtt| (*rtt.force()).clone());

    /// let expected: Term = STerm::parse("f").share(&syms)?;
    /// assert_eq!(vec![expected], subst.collect::<Vec<_>>());
    /// # Ok::<(), Error>(())
    /// ~~~
    fn match_flatten(&self, rule: &'t Rule<'s>, gc: &'t GCtx<'s>) -> Option<Context<'s, 't>> {
        self.match_rule(rule, gc)?
            .into_iter()
            .map(|s| all_convertible(s.into_iter(), gc))
            .rev()
            .collect::<Option<_>>()
            .map(Context)
    }
}

type Subst<'s, 't, 'a> = Box<dyn Iterator<Item = Option<(Miller, RState<'s, 't>)>> + 'a>;

impl<'s, 't> Stack<'s, 't> {
    fn into_match_pats<'a>(self, pats: &'t [Pattern<'s>], gc: &'t GCtx<'s>) -> Subst<'s, 't, 'a>
    where
        't: 'a,
    {
        let iter = self.0.into_iter().rev();
        Box::new(
            iter.zip(pats)
                .map(|(rstate, pat)| rstate.match_pat(pat, gc))
                .flatten(),
        )
    }

    fn match_pats<'a>(&'a self, pats: &'t [Pattern<'s>], gc: &'t GCtx<'s>) -> Subst<'s, 't, 'a>
    where
        't: 'a,
    {
        let iter = self.0.iter().rev();
        Box::new(
            iter.zip(pats)
                .map(|(rstate, pat)| rstate.clone().match_pat(pat, gc))
                .flatten(),
        )
    }

    fn match_top<'a>(&'a self, pat: &'t TopPattern<'s>, gc: &'t GCtx<'s>) -> Subst<'s, 't, 'a>
    where
        't: 'a,
    {
        if self.0.len() < pat.args.len() {
            // we do not have enough arguments on the stack to match against
            return Box::new(core::iter::once(None));
        }

        self.match_pats(&pat.args, gc)
    }

    fn match_rule(&self, rule: &'t Rule<'s>, gc: &'t GCtx<'s>) -> Option<Vec<Vec<RState<'s, 't>>>> {
        let mut subst = alloc::vec![Vec::new(); rule.ctx.len()];
        for i in self.match_top(&rule.lhs, gc) {
            let (m, st1) = i?;
            // the next line should not fail, unless
            // the pattern contains more variables than indicated in the context
            // (which scoping is designed to rule out)
            subst.get_mut(m)?.push(st1)
        }
        Some(subst)
    }
}

impl<'s, 't> RState<'s, 't> {
    fn match_pat<'a>(self, pat: &'t Pattern<'s>, gc: &'t GCtx<'s>) -> Subst<'s, 't, 'a>
    where
        't: 'a,
    {
        match pat {
            Pattern::Symb(sp, pats) => {
                self.whnf(gc);
                let state = self.borrow_state();
                if let STerm::Const(st) = &state.term {
                    // The stack and pattern length have to be equal,
                    // to exclude pattern matches like `f (g a) ~ f g`.
                    // This is unlike `TopPattern::matches`, which
                    // allows matches like `add 0 n ~ add 0`.
                    if sp == st && state.stack.0.len() == pats.len() {
                        return state.stack.clone().into_match_pats(pats, gc);
                    }
                }
                Box::new(core::iter::once(None))
            }
            Pattern::MVar(m) => Box::new(core::iter::once(Some((*m, self)))),
            Pattern::Joker => Box::new(core::iter::empty()),
        }
    }
}
