//! Substitution and shifting for terms.

use super::term::{Arg, RTerm, Term};
use alloc::vec::Vec;

impl Arg {
    /// Return the substitution of an argument.
    fn subst<S>(self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> RTerm,
    {
        let ty = self.ty.map(|ty| ty.apply_subst(subst, k));
        Self { id: self.id, ty }
    }

    /// Return the substitution of an argument and a term.
    fn subst_bound<S>(self, tm: RTerm, subst: &S, k: usize) -> (Self, RTerm)
    where
        S: Fn(usize, usize) -> RTerm,
    {
        (self.subst(subst, k), tm.apply_subst(subst, k + 1))
    }
}

impl RTerm {
    pub fn apply_subst<S>(self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> RTerm,
    {
        match &*self {
            Term::BVar(n) if *n >= k => subst(*n, k),
            Term::Appl(f, args) => {
                let f2 = f.clone().apply_subst(subst, k);
                let args2: Vec<RTerm> = args
                    .iter()
                    .map(|a| a.clone().apply_subst(subst, k))
                    .collect();
                if f.ptr_eq(&f2) && args.iter().zip(args2.iter()).all(|(a, a2)| a.ptr_eq(a2)) {
                    self
                } else {
                    Self::new(Term::Appl(f2, args2))
                }
            }
            Term::Abst(arg, f) => {
                let (arg2, f2) = arg.clone().subst_bound(f.clone(), subst, k);
                if arg.ptr_eq(&arg2) && f.ptr_eq(&f2) {
                    self
                } else {
                    Self::new(Term::Abst(arg2, f2))
                }
            }
            Term::Prod(arg, f) => {
                let (arg2, f2) = arg.clone().subst_bound(f.clone(), subst, k);
                if arg.ptr_eq(&arg2) && f.ptr_eq(&f2) {
                    self
                } else {
                    Self::new(Term::Prod(arg2, f2))
                }
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
