//! Pattern matching.

use super::pattern::{Miller, Pattern, TopPattern};
use super::state::{RState, Stack};
use super::{Rule, Signature, Term};
use alloc::{boxed::Box, vec, vec::Vec};

type Subst<'a> = Box<dyn Iterator<Item = Option<(Miller, RState)>> + 'a>;

impl Stack {
    fn into_matches<'a>(self, pats: &'a [Pattern], sig: &'a Signature) -> Subst<'a> {
        Box::new(
            self.into_iter()
                .zip(pats)
                .map(move |(rstate, pat)| pat.matches(rstate, sig))
                .flatten(),
        )
    }

    fn matches<'a>(&'a self, pats: &'a [Pattern], sig: &'a Signature) -> Subst<'a> {
        Box::new(
            self.iter()
                .zip(pats)
                .map(move |(rstate, pat)| pat.matches(rstate.clone(), sig))
                .flatten(),
        )
    }
}

impl Pattern {
    fn matches<'a>(&'a self, rstate: RState, sig: &'a Signature) -> Subst<'a> {
        match self {
            Self::Symb(sp, pats) => {
                rstate.whnf(sig);
                let state = rstate.borrow_state();
                match &*state.term {
                    Term::Symb(st) => {
                        // The stack and pattern length have to be equal,
                        // to exclude pattern matches like `f (g a) ~ f g`.
                        // This is unlike `TopPattern::matches`, which
                        // allows matches like `add 0 n ~ add 0`.
                        if sp == st && state.stack.len() == pats.len() {
                            state.stack.clone().into_matches(pats, sig)
                        } else {
                            Box::new(core::iter::once(None))
                        }
                    }
                    _ => Box::new(core::iter::once(None)),
                }
            }
            Self::MVar(m) => Box::new(core::iter::once(Some((*m, rstate)))),
            Self::Joker => Box::new(core::iter::empty()),
        }
    }
}

impl TopPattern {
    fn matches<'a>(&'a self, stack: &'a Stack, sig: &'a Signature) -> Subst<'a> {
        if stack.len() < self.args.len() {
            // we do not have enough arguments on the stack to match against
            return Box::new(core::iter::once(None));
        }

        stack.matches(&self.args, sig)
    }
}

impl Rule {
    pub fn matches(&self, stack: &Stack, sig: &Signature) -> Option<Vec<Vec<RState>>> {
        let mut subst = vec![vec![]; self.ctx.len()];
        for i in self.lhs.matches(stack, sig) {
            let (m, st1) = i?;
            subst.get_mut(m.0).expect("subst").push(st1)
        }
        Some(subst)
    }
}
