use crate::term::{Arg, RTerm, Term};

impl RTerm {
    pub fn apply_subst<S>(self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> Term,
    {
        let tm = (*self).clone().apply_subst(subst, k);
        // TODO: compare only the topmost pointers?
        if tm == *self {
            self
        } else {
            RTerm::new(tm)
        }
    }

    pub fn subst(self, u: &Term) -> Self {
        self.apply_subst(&psubst_single(u), 0)
    }
}

impl Term {
    pub fn apply_subst<S>(self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> Term,
    {
        match self {
            Self::BVar(n) if n >= k => subst(n, k),
            Self::Appl(f, args) => {
                let f2 = f.apply_subst(subst, k);
                let args2 = args.into_iter().map(|a| a.apply_subst(subst, k)).collect();
                Self::Appl(f2, args2)
            }
            Self::Abst(arg, f) => {
                let ty = arg.ty.map(|a| a.apply_subst(subst, k));
                let f = f.apply_subst(subst, k + 1);
                Self::Abst(Arg { id: arg.id, ty }, f)
            }
            Self::Prod(arg, f) => {
                let ty = arg.ty.map(|a| a.apply_subst(subst, k));
                let f = f.apply_subst(subst, k + 1);
                Self::Prod(Arg { id: arg.id, ty }, f)
            }
            _ => self,
        }
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

impl core::ops::Shl<usize> for RTerm {
    type Output = Self;

    fn shl(self, rhs: usize) -> Self::Output {
        if rhs == 0 {
            self
        } else {
            self.apply_subst(&|n, _k| Term::BVar(n + rhs), 0)
        }
    }
}

// TODO: eliminate this in the long run?
impl core::ops::Shl<usize> for Term {
    type Output = Self;

    fn shl(self, rhs: usize) -> Self::Output {
        if rhs == 0 {
            self
        } else {
            self.apply_subst(&|n, _k| Term::BVar(n + rhs), 0)
        }
    }
}
