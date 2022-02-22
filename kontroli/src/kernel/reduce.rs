//! Reduction to weak head normal form (WHNF), including rewriting.

use super::{GCtx, Pattern, Rule, Term, TermC, TopPattern};
use crate::pattern::Miller;
use crate::stack;
use alloc::{boxed::Box, rc::Rc, vec::Vec};
use core::cell::{Ref, RefCell, RefMut};
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
pub struct State<'s> {
    pub ctx: Context<'s>,
    pub term: Term<'s>,
    pub stack: Stack<'s>,
}

impl<'s> State<'s> {
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
    pub fn new(term: Term<'s>) -> Self {
        Self {
            ctx: Context::new(),
            term,
            stack: Stack::new(),
        }
    }
}

/// Map from de Bruijn indices in the term of the abstract machine to lazy terms.
pub type Context<'s> = stack::Stack<RTTerm<'s>>;

/// Arguments to the abstract machine term.
pub type Stack<'s> = stack::Stack<RState<'s>>;

/// A shared lazy term constructed from a shared mutable state.
#[derive(Clone)]
pub struct RTTerm<'s>(Rc<Thunk<RState<'s>, Term<'s>>>);

impl<'s> RTTerm<'s> {
    pub fn new(st: RState<'s>) -> Self {
        Self(Rc::new(Thunk::new(st)))
    }

    /// Force evaluation of the lazy term.
    pub fn force(&self) -> &Term<'s> {
        &**self.0
    }
}

/// A shared mutable state.
///
/// We use `RefCell` instead of `Thunk` here
/// because evaluation requires a global context and
/// because we sometimes wish to access the original state.
#[derive(Clone)]
pub struct RState<'s>(Rc<RefCell<WState<'s>>>);

impl<'s> RState<'s> {
    pub fn new(wst: WState<'s>) -> Self {
        Self(Rc::new(RefCell::new(wst)))
    }

    pub fn borrow(&self) -> Ref<WState<'s>> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<WState<'s>> {
        self.0.borrow_mut()
    }
}

impl<'s> lazy_st::Evaluate<Term<'s>> for RState<'s> {
    fn evaluate(self) -> Term<'s> {
        Term::from(self)
    }
}

impl<'s> From<RState<'s>> for Term<'s> {
    fn from(s: RState<'s>) -> Self {
        Term::from(s.borrow_state().clone())
    }
}

impl<'s> From<State<'s>> for Term<'s> {
    fn from(state: State<'s>) -> Self {
        state
            .term
            .psubst(&state.ctx)
            .apply(state.stack.into_iter().map(Self::from).collect())
    }
}

impl<'s> Term<'s> {
    fn psubst(self, args: &Context<'s>) -> Self {
        if args.is_empty() {
            self
        } else {
            self.apply_subst(&psubst(args), 0)
        }
    }
}

fn psubst<'s, 'c>(args: &'c Context<'s>) -> impl Fn(usize, usize) -> Term<'s> + 'c {
    move |n: usize, k: usize| match args.get(n - k) {
        Some(arg) => arg.force().clone().shift(k),
        None => Term::BVar(n - args.len()),
    }
}

/// A version of `State` that tracks whether it was reduced to WHNF yet.
pub struct WState<'s> {
    state: State<'s>,
    whnfed: bool,
}

impl<'s> WState<'s> {
    fn new(state: State<'s>) -> Self {
        let whnfed = false;
        Self { state, whnfed }
    }

    /// Replace the state with its WHNF if it was not in WHNF before.
    fn whnf(&mut self, gc: &GCtx<'s>) {
        if !self.whnfed {
            self.state.whnf(gc);
            self.whnfed = true
        }
    }
}

impl<'s> RState<'s> {
    /// Replace the state with its WHNF if it was not in WHNF before.
    pub fn whnf(&self, gc: &GCtx<'s>) {
        self.borrow_mut().whnf(gc)
    }

    /// Obtain a reference to the state.
    pub fn borrow_state(&self) -> Ref<State<'s>> {
        Ref::map(self.borrow(), |wst| &wst.state)
    }
}

impl<'s> State<'s> {
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
    pub fn whnf(&mut self, gc: &GCtx<'s>) {
        use crate::Term::*;
        loop {
            trace!("whnf: {}", self.term);
            match &self.term {
                Type | Kind => break,
                BVar(x) => match self.ctx.get(*x) {
                    Some(ctm) => {
                        self.term = ctm.force().clone();
                        self.ctx.clear()
                    }
                    None => {
                        if !self.ctx.is_empty() {
                            self.term = BVar(x - self.ctx.len());
                            self.ctx.clear();
                        }
                        break;
                    }
                },
                Symb(s) => match &gc.rules.get(s) {
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
                                self.term = rule.rhs.clone();
                                self.stack.pop_many(rule.lhs.args.len());
                            }
                        }
                    }
                },
                Comb(c) => match &**c {
                    TermC::Prod(_, _) => break,
                    TermC::Abst(_, t) => match self.stack.pop() {
                        None => break,
                        Some(p) => {
                            self.term = t.clone();
                            self.ctx.push(RTTerm::new(p));
                        }
                    },
                    TermC::Appl(head, tail) => {
                        for t in tail.iter().rev() {
                            let st = State {
                                ctx: self.ctx.clone(),
                                term: t.clone(),
                                stack: Stack::new(),
                            };
                            self.stack.push(RState::new(WState::new(st)))
                        }
                        self.term = head.clone();
                    }
                },
            }
        }

        if let BVar(_) = self.term {
            assert!(self.ctx.is_empty())
        }
    }
}

impl<'s> Term<'s> {
    /// Return the weak head normal form of the term.
    pub fn whnf(self, gc: &GCtx<'s>) -> Self {
        trace!("whnf of {}", self);
        let mut state = State::new(self);
        state.whnf(gc);
        Self::from(state)
    }

    /// Return true if the given terms have a common redex.
    pub fn convertible(tm1: Self, tm2: Self, gc: &GCtx<'s>) -> bool {
        let mut cns = Vec::from([(tm1, tm2)]);
        loop {
            match cns.pop() {
                Some((cn1, cn2)) => {
                    trace!("convertible: {} ~? {}", cn1, cn2);
                    use super::convertible::step;
                    if cn1 != cn2 && !step((cn1.whnf(gc), cn2.whnf(gc)), &mut cns, gc.eta) {
                        break false;
                    }
                }
                None => break true,
            }
        }
    }
}

/// For a sequence of states,
/// return the term corresponding to its first state
/// if it is convertible with all other states.
///
/// This is used for checking nonlinear pattern matches, because there
/// we want to ensure that all states that were
/// matched with the same variable are convertible.
fn all_convertible<'s, I>(mut iter: I, gc: &GCtx<'s>) -> Option<RTTerm<'s>>
where
    I: Iterator<Item = RState<'s>>,
{
    // assure that we have at least one term
    let tm = RTTerm::new(iter.next()?);
    for stn in iter {
        // the first term is only evaluated if we have some other terms
        if !Term::convertible(tm.force().clone(), Term::from(stn), gc) {
            return None;
        }
    }
    Some(tm)
}

impl<'s> Stack<'s> {
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
    pub fn match_flatten(&self, rule: &Rule<'s>, gc: &GCtx<'s>) -> Option<Context<'s>> {
        self.match_rule(rule, gc)?
            .into_iter()
            .map(|s| all_convertible(s.into_iter(), gc))
            .rev()
            .collect()
    }
}

type Subst<'s, 'a> = Box<dyn Iterator<Item = Option<(Miller, RState<'s>)>> + 'a>;

impl<'s> Stack<'s> {
    fn into_match_pats<'a>(self, pats: &'a [Pattern<'s>], gc: &'a GCtx<'s>) -> Subst<'s, 'a> {
        Box::new(
            self.into_iter()
                .zip(pats)
                .map(move |(rstate, pat)| rstate.match_pat(pat, gc))
                .flatten(),
        )
    }

    fn match_pats<'a>(&'a self, pats: &'a [Pattern<'s>], gc: &'a GCtx<'s>) -> Subst<'s, 'a> {
        Box::new(
            self.iter()
                .zip(pats)
                .map(move |(rstate, pat)| rstate.clone().match_pat(pat, gc))
                .flatten(),
        )
    }

    fn match_top<'a>(&'a self, pat: &'a TopPattern<'s>, gc: &'a GCtx<'s>) -> Subst<'s, 'a> {
        if self.len() < pat.args.len() {
            // we do not have enough arguments on the stack to match against
            return Box::new(core::iter::once(None));
        }

        self.match_pats(&pat.args, gc)
    }

    pub fn match_rule(&self, rule: &Rule<'s>, gc: &GCtx<'s>) -> Option<Vec<Vec<RState<'s>>>> {
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

impl<'s> RState<'s> {
    fn match_pat<'a>(self, pat: &'a Pattern<'s>, gc: &'a GCtx<'s>) -> Subst<'s, 'a> {
        match pat {
            Pattern::Symb(sp, pats) => {
                self.whnf(gc);
                let state = self.borrow_state();
                if let Term::Symb(st) = &state.term {
                    // The stack and pattern length have to be equal,
                    // to exclude pattern matches like `f (g a) ~ f g`.
                    // This is unlike `TopPattern::matches`, which
                    // allows matches like `add 0 n ~ add 0`.
                    if sp == st && state.stack.len() == pats.len() {
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
