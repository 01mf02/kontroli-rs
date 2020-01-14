use super::*;
use lazy_st::Lazy;
use std::rc::Rc;

impl Term {
    fn subst_box<S>(self, subst: &S, k: usize) -> BTerm
    where
        S: Fn(usize, usize) -> Option<Term>,
    {
        Box::new(self.apply_subst(subst, k))
    }
    fn apply_subst<S>(self, subst: &S, k: usize) -> Term
    where
        S: Fn(usize, usize) -> Option<Term>,
    {
        use Term::*;
        match self {
            BVar(n) if n >= k => subst(n, k).unwrap_or(self),
            Appl(f, args) => {
                let f2 = f.subst_box(subst, k);
                let args2 = args.into_iter().map(|a| a.subst_box(subst, k)).collect();
                Appl(f2, args2)
            }
            Abst(arg, f) => {
                let ty = arg.ty.map(|a| a.subst_box(subst, k));
                let f = f.subst_box(subst, k + 1);
                Abst(Arg { id: arg.id, ty }, f)
            }
            Prod(arg, f) => {
                let ty = arg.ty.map(|a| a.subst_box(subst, k));
                let f = f.subst_box(subst, k + 1);
                Prod(Arg { id: arg.id, ty }, f)
            }
            _ => self,
        }
    }

    pub fn psubst(self, args: &[Rc<Lazy<Term>>]) -> Term {
        self.apply_subst(&psubst(args), 0)
    }

    pub fn subst(self, u: &Term) -> Term {
        self.apply_subst(&psubst_single(u), 0)
    }
}

// TODO: merge with psubst?
fn psubst_single(u: &Term) -> impl Fn(usize, usize) -> Option<Term> + '_ {
    move |n: usize, k: usize| {
        Some(if n == k {
            u.clone() << k
        } else {
            Term::BVar(n + 1)
        })
    }
}

fn psubst(args: &[Rc<Lazy<Term>>]) -> impl Fn(usize, usize) -> Option<Term> + '_ {
    move |n: usize, k: usize| {
        Some({
            if n >= k + args.len() {
                Term::BVar(n - args.len())
            } else {
                let arg = (**args.get(n - k).expect("args")).clone();
                // TODO: if this turns out to be a performance bottleneck,
                // switch to a shift-memoised version as in Dedukti
                arg << k
            }
        })
    }
}

impl core::ops::Shl<usize> for Term {
    type Output = Term;

    fn shl(self, rhs: usize) -> Self::Output {
        if rhs == 0 {
            self
        } else {
            self.apply_subst(&|n, _k| Some(Term::BVar(n + rhs)), 0)
        }
    }
}
