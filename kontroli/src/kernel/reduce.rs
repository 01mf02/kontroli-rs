//! Reduction to weak head normal form (WHNF), including rewriting.

use super::state::{Context, RState, RTTerm, Stack, State};
use super::{RTerm, Rule, Signature};
use core::cell::Ref;

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
    fn whnf(&mut self, sig: &Signature<'s>) {
        if !self.whnfed {
            self.state.whnf(sig);
            self.whnfed = true
        }
    }
}

impl<'s> RState<'s> {
    /// Replace the state with its WHNF if it was not in WHNF before.
    pub fn whnf(&self, sig: &Signature<'s>) {
        self.borrow_mut().whnf(sig)
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
    /// # use kontroli::scope::{BTerm as SBTerm, Term as STerm};
    /// # use kontroli::rc::{RTerm, Signature, Term};
    /// # use kontroli::rc::state::State;
    /// let sig = Signature::new();
    /// let syms = Symbols::new();
    ///
    /// let term = SBTerm::parse(r"(x => x) (x => x)")?.share(&syms)?;
    /// let mut state = State::new(term);
    /// state.whnf(&sig);
    ///
    /// let expected = STerm::parse(r"(x => x)")?.share(&syms)?;
    /// assert!(state.ctx.is_empty());
    /// assert!(state.stack.is_empty());
    /// assert_eq!(*state.term, expected);
    /// # Ok::<(), Error>(())
    /// ~~~
    pub fn whnf(&mut self, sig: &Signature<'s>) {
        use crate::Term::*;
        loop {
            trace!("whnf: {}", self.term);
            match &*self.term {
                Type | Kind | Prod(_, _) => break,
                BVar(x) => match self.ctx.get(*x) {
                    Some(ctm) => {
                        self.term = ctm.force().clone();
                        self.ctx.clear()
                    }
                    None => {
                        if !self.ctx.is_empty() {
                            self.term = RTerm::new(BVar(x - self.ctx.len()));
                            self.ctx.clear();
                        }
                        break;
                    }
                },
                Abst(_, t) => match self.stack.pop() {
                    None => break,
                    Some(p) => {
                        self.term = t.clone();
                        self.ctx.push(RTTerm::new(p));
                    }
                },
                Appl(head, tail) => {
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
                Symb(s) => match &sig.rules.get(&s) {
                    None => break,
                    Some(rules) => {
                        match rules
                            .iter()
                            .filter_map(|r| Some((self.stack.match_flatten(r, sig)?, r)))
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
            }
        }

        if let BVar(_) = &*self.term {
            assert!(self.ctx.is_empty())
        }
    }
}

impl<'s> RTerm<'s> {
    /// Return the weak head normal form of the term.
    pub fn whnf(self, sig: &Signature<'s>) -> Self {
        trace!("whnf of {}", self);
        let mut state = State::new(self);
        state.whnf(sig);
        Self::from(state)
    }
}

/// For a sequence of states,
/// return the term corresponding to its first state
/// if it is convertible with all other states.
///
/// This is used for checking nonlinear pattern matches, because there
/// we want to ensure that all states that were
/// matched with the same variable are convertible.
fn all_convertible<'s>(
    mut iter: impl Iterator<Item = RState<'s>>,
    sig: &Signature<'s>,
) -> Option<RTTerm<'s>> {
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

impl<'s> Stack<'s> {
    /// Determine whether the stack of an abstract machine matches the rule's LHS.
    ///
    /// Return a new machine context containing variable assignments in case of a match.
    ///
    /// ~~~
    /// # use kontroli::rc::state::State;
    /// # use kontroli::rc::{RTerm, Rule, Signature, Term};
    /// # use kontroli::scope::{BTerm as SBTerm, Rule as SRule, Term as STerm};
    /// # use kontroli::{Error, Share, Symbols};
    /// let syms: Symbols = vec!["id", "f", "a"].into_iter().collect();
    /// let sig = Signature::new();

    /// let rule = SRule::parse("[A] id A --> A")?.share(&syms)?;
    /// let term = SBTerm::parse("id f a")?.share(&syms)?;

    /// let mut state = State::new(term);
    /// state.whnf(&sig);
    /// let subst = state.stack.match_flatten(&rule, &sig).unwrap();
    /// let subst = subst.iter().map(|rtt| (**rtt.force()).clone());

    /// let expected: Term = STerm::parse("f")?.share(&syms)?;
    /// assert_eq!(vec![expected], subst.collect::<Vec<_>>());
    /// # Ok::<(), Error>(())
    /// ~~~
    pub fn match_flatten(&self, rule: &Rule<'s>, sig: &Signature<'s>) -> Option<Context<'s>> {
        self.match_rule(rule, sig)?
            .into_iter()
            .map(|s| all_convertible(s.into_iter(), sig))
            .rev()
            .collect()
    }
}
