//! Pattern matching.

use super::state::{RState, Stack};
use super::{Rule, Signature, Term};
use crate::scope::pattern::{Miller, Pattern, TopPattern};
use alloc::{boxed::Box, vec, vec::Vec};

type Subst<'s, 'a> = Box<dyn Iterator<Item = Option<(Miller, RState<'s>)>> + 'a>;

impl<'s> Stack<'s> {
    fn into_match_pats<'a>(self, pats: &'a [Pattern<'s>], sig: &'a Signature<'s>) -> Subst<'s, 'a> {
        Box::new(
            self.into_iter()
                .zip(pats)
                .map(move |(rstate, pat)| rstate.match_pat(pat, sig))
                .flatten(),
        )
    }

    fn match_pats<'a>(&'a self, pats: &'a [Pattern<'s>], sig: &'a Signature<'s>) -> Subst<'s, 'a> {
        Box::new(
            self.iter()
                .zip(pats)
                .map(move |(rstate, pat)| rstate.clone().match_pat(pat, sig))
                .flatten(),
        )
    }

    fn match_top<'a>(&'a self, pat: &'a TopPattern<'s>, sig: &'a Signature<'s>) -> Subst<'s, 'a> {
        if self.len() < pat.args.len() {
            // we do not have enough arguments on the stack to match against
            return Box::new(core::iter::once(None));
        }

        self.match_pats(&pat.args, sig)
    }

    pub fn match_rule(&self, rule: &Rule<'s>, sig: &Signature<'s>) -> Option<Vec<Vec<RState<'s>>>> {
        let mut subst = vec![vec![]; rule.ctx.len()];
        for i in self.match_top(&rule.lhs, sig) {
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
    fn match_pat<'a>(self, pat: &'a Pattern<'s>, sig: &'a Signature<'s>) -> Subst<'s, 'a> {
        match pat {
            Pattern::Symb(sp, pats) => {
                self.whnf(sig);
                let state = self.borrow_state();
                match &*state.term {
                    Term::Symb(st) => {
                        // The stack and pattern length have to be equal,
                        // to exclude pattern matches like `f (g a) ~ f g`.
                        // This is unlike `TopPattern::matches`, which
                        // allows matches like `add 0 n ~ add 0`.
                        if sp == st && state.stack.len() == pats.len() {
                            state.stack.clone().into_match_pats(pats, sig)
                        } else {
                            Box::new(core::iter::once(None))
                        }
                    }
                    _ => Box::new(core::iter::once(None)),
                }
            }
            Pattern::MVar(m) => Box::new(core::iter::once(Some((*m, self)))),
            Pattern::Joker => Box::new(core::iter::empty()),
        }
    }
}
