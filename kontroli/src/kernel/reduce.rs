//! Reduction to weak head normal form (WHNF), including rewriting.

use super::state::{Context, RState, RTTerm, Stack, State};
use super::{GCtx, Rule, Term, TermC};
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
    /// let term = STerm::parse(r"(x => x) (x => x)")?.share(&syms)?;
    /// let mut state = State::new(term);
    /// state.whnf(&gc);
    ///
    /// let expected = STerm::parse(r"(x => x)")?.share(&syms)?;
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

    /// let rule = SRule::parse("[A] id A --> A")?.share(&syms)?;
    /// let term = STerm::parse("id f a")?.share(&syms)?;

    /// let mut state = State::new(term);
    /// state.whnf(&gc);
    /// let subst = state.stack.match_flatten(&rule, &gc).unwrap();
    /// let subst = subst.iter().map(|rtt| (*rtt.force()).clone());

    /// let expected: Term = STerm::parse("f")?.share(&syms)?;
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
