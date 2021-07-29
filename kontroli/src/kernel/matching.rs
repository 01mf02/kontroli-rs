//! Pattern matching.

use super::state::{RState, Stack};
use super::{GCtx, Pattern, Rule, Term, TopPattern};
use crate::pattern::Miller;
use alloc::{boxed::Box, vec, vec::Vec};

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
        let mut subst = vec![vec![]; rule.ctx.len()];
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
