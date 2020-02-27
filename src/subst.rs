use crate::term::{Arg, RTerm, Term};

impl Arg {
    pub fn apply_subst<S>(&self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> RTerm,
    {
        Self {
            id: self.id.clone(),
            ty: self.ty.as_ref().map(|a| a.clone().apply_subst(subst, k)),
        }
    }
}

impl RTerm {
    pub fn apply_subst<S>(self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> RTerm,
    {
        // TODO: only create new RTerm if something has changed
        match &*self {
            Term::BVar(n) if *n >= k => subst(*n, k),
            Term::Appl(f, args) => {
                let f2 = f.clone().apply_subst(subst, k);
                let args2 = args
                    .iter()
                    .map(|a| a.clone().apply_subst(subst, k))
                    .collect();
                RTerm::new(Term::Appl(f2, args2))
            }
            Term::Abst(arg, f) => {
                let f2 = f.clone().apply_subst(subst, k + 1);
                RTerm::new(Term::Abst(arg.apply_subst(subst, k), f2))
            }
            Term::Prod(arg, f) => {
                let f2 = f.clone().apply_subst(subst, k + 1);
                RTerm::new(Term::Prod(arg.apply_subst(subst, k), f2))
            }
            _ => self,
        }
    }

    pub fn subst(self, u: &RTerm) -> Self {
        self.apply_subst(&psubst_single(u), 0)
    }
}

fn psubst_single(u: &RTerm) -> impl Fn(usize, usize) -> RTerm + '_ {
    move |n: usize, k: usize| {
        if n == k {
            u.clone() << k
        } else {
            RTerm::new(Term::BVar(n - 1))
        }
    }
}

impl core::ops::Shl<usize> for RTerm {
    type Output = Self;

    fn shl(self, rhs: usize) -> Self::Output {
        if rhs == 0 {
            self
        } else {
            self.apply_subst(&|n, _k| RTerm::new(Term::BVar(n + rhs)), 0)
        }
    }
}
