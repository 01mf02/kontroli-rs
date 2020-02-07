use crate::term::{Arg, BTerm, Term};

impl Term {
    fn subst_box<S>(self, subst: &S, k: usize) -> BTerm
    where
        S: Fn(usize, usize) -> Term,
    {
        Box::new(self.apply_subst(subst, k))
    }
    pub fn apply_subst<S>(self, subst: &S, k: usize) -> Term
    where
        S: Fn(usize, usize) -> Term,
    {
        use Term::*;
        match self {
            BVar(n) if n >= k => subst(n, k),
            Appl(f, args) => {
                let f2 = f.subst_box(subst, k);
                let args2 = args.into_iter().map(|a| a.apply_subst(subst, k)).collect();
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

    pub fn subst(self, u: &Term) -> Term {
        self.apply_subst(&psubst_single(u), 0)
    }
}

// TODO: merge with psubst?
fn psubst_single(u: &Term) -> impl Fn(usize, usize) -> Term + '_ {
    move |n: usize, k: usize| {
        if n == k {
            u.clone() << k
        } else {
            Term::BVar(n - 1)
        }
    }
}

impl core::ops::Shl<usize> for Term {
    type Output = Term;

    fn shl(self, rhs: usize) -> Self::Output {
        if rhs == 0 {
            self
        } else {
            self.apply_subst(&|n, _k| Term::BVar(n + rhs), 0)
        }
    }
}
