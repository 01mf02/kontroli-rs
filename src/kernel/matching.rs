//! Pattern matching.

use super::pattern::{Miller, Pattern, TopPattern};
use super::state::{RState, Stack};
use super::{Rule, Signature, Term};
use alloc::{boxed::Box, vec, vec::Vec};

type Subst<'s, 'a> = Box<dyn Iterator<Item = Option<(Miller, RState<'s>)>> + 'a>;

impl<'s> Stack<'s> {
    fn into_matches<'a>(self, pats: &'a [Pattern<'s>], sig: &'a Signature<'s>) -> Subst<'s, 'a> {
        Box::new(
            self.into_iter()
                .zip(pats)
                .map(move |(rstate, pat)| pat.matches(rstate, sig))
                .flatten(),
        )
    }

    fn matches<'a>(&'a self, pats: &'a [Pattern<'s>], sig: &'a Signature<'s>) -> Subst<'s, 'a> {
        Box::new(
            self.iter()
                .zip(pats)
                .map(move |(rstate, pat)| pat.matches(rstate.clone(), sig))
                .flatten(),
        )
    }
}

impl<'s> Pattern<'s> {
    fn matches<'a>(&'a self, rstate: RState<'s>, sig: &'a Signature<'s>) -> Subst<'s, 'a> {
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

impl<'s> TopPattern<'s> {
    fn matches<'a>(&'a self, stack: &'a Stack<'s>, sig: &'a Signature<'s>) -> Subst<'s, 'a> {
        if stack.len() < self.args.len() {
            // we do not have enough arguments on the stack to match against
            return Box::new(core::iter::once(None));
        }

        stack.matches(&self.args, sig)
    }
}

impl<'s> Rule<'s> {
    pub fn matches(&self, stack: &Stack<'s>, sig: &Signature<'s>) -> Option<Vec<Vec<RState<'s>>>> {
        let mut subst = vec![vec![]; self.ctx.len()];
        for i in self.lhs.matches(stack, sig) {
            let (m, st1) = i?;
            // the next line should not fail, unless
            // the pattern contains more variables than indicated in the context
            // (which scoping is designed to rule out)
            subst.get_mut(m)?.push(st1)
        }
        Some(subst)
    }
}
