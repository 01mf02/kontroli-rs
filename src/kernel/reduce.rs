//! Reduction to weak head normal form (WHNF), including rewriting.

use super::state::{Context, RState, RTTerm, Stack, State};
use super::{RTerm, Rule, Signature, Term};
use core::cell::Ref;

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

        let state = core::mem::take(&mut self.state);
        self.state = state.whnf(sig);
        self.whnfed = true
    }
}

impl RState {
    /// Replace the state with its WHNF if it was not in WHNF before.
    pub fn whnf(&self, sig: &Signature) {
        self.borrow_mut().whnf(sig)
    }

    /// Obtain a reference to the state.
    pub fn borrow_state(&self) -> Ref<State> {
        Ref::map(self.borrow(), |wst| &wst.state)
    }
}

impl State {
    /// Evaluate the state to its weak head normal form.
    ///
    /// ~~~
    /// # use kontroli::Error;
    /// # use kontroli::rc::{RTerm, Signature, Symbols, Term};
    /// # use kontroli::rc::state::State;
    /// let sig = Signature::new();
    /// let syms = Symbols::new();
    ///
    /// let term = Term::parse(r"(\ x => x) (\ x => x).", &syms)?;
    /// let whnf = State::new(RTerm::new(term)).whnf(&sig);
    ///
    /// let expected = Term::parse(r"(\ x => x).", &syms)?;
    /// assert!(whnf.ctx.is_empty());
    /// assert!(whnf.stack.is_empty());
    /// assert_eq!(*whnf.term, expected);
    /// # Ok::<(), Error>(())
    /// ~~~
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
                Abst(_, t) => match stack.pop() {
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
                        stack.push(RState::new(WState::new(st)))
                    }
                    term = head.clone();
                }
                Symb(s) => match &sig.rules.get(&s) {
                    None => break,
                    Some(rules) => {
                        match rules
                            .iter()
                            .filter_map(|r| Some((r.match_flatten(&stack, sig)?, r)))
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

        if let BVar(_) = &*term {
            assert!(ctx.is_empty())
        }

        State { ctx, term, stack }
    }
}

impl RTerm {
    /// Return the weak head normal form of the term.
    pub fn whnf(self, sig: &Signature) -> Self {
        trace!("whnf of {}", self);
        Self::from(State::new(self).whnf(sig))
    }
}

/// For a sequence of states,
/// return the term corresponding to its first state
/// if it is convertible with all other states.
///
/// This is used for checking nonlinear pattern matches, because there
/// we want to ensure that all states that were
/// matched with the same variable are convertible.
fn all_convertible(mut iter: impl Iterator<Item = RState>, sig: &Signature) -> Option<RTTerm> {
    // assure that we have at least one term
    let tm = RTTerm::new(iter.next()?);
    for stn in iter {
        // the first term is only evaluated if we have some other terms
        if !RTerm::convertible(tm.force().clone(), RTerm::from(stn), &sig) {
            return None;
        }
    }
    Some(tm)
}

impl Rule {
    /// Determine whether the stack of an abstract machine matches the rule's LHS.
    ///
    /// Return a new machine context containing variable assignments in case of a match.
    ///
    /// ~~~
    /// # use kontroli::Error;
    /// # use kontroli::rc::{RTerm, Rule, Signature, Symbols, Term};
    /// # use kontroli::rc::state::State;
    /// let syms: Symbols = vec!["id", "f", "a"].into_iter().collect();
    /// let sig = Signature::new();
    ///
    /// let rule = Rule::parse("[A] id A --> A.", &syms)?;
    /// let term = Term::parse("id f a.", &syms)?;
    ///
    /// let stack = State::new(RTerm::new(term)).whnf(&sig).stack;
    /// let subst = rule.match_flatten(&stack, &sig).unwrap();
    /// let subst = subst.iter().map(|rtt| (**rtt.force()).clone());
    ///
    /// assert_eq!(vec![Term::parse("f.", &syms)?], subst.collect::<Vec<_>>());
    /// # Ok::<(), Error>(())
    /// ~~~
    pub fn match_flatten(&self, stack: &Stack, sig: &Signature) -> Option<Context> {
        self.matches(stack, sig)?
            .into_iter()
            .map(|s| all_convertible(s.into_iter(), sig))
            .rev()
            .collect()
    }
}