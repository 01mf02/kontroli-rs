//! Substitution and shifting for terms.

use super::{RTerm, Term};
use alloc::vec::Vec;

impl<'s> RTerm<'s> {
    pub fn apply_subst<S>(self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> RTerm<'s>,
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
                let arg2 = arg.clone().map_ty(|o| o.map(|ty| ty.apply_subst(subst, k)));
                let f2 = f.clone().apply_subst(subst, k + 1);
                if arg.type_ptr_eq(&arg2) && f.ptr_eq(&f2) {
                    self
                } else {
                    Self::new(Term::Abst(arg2, f2))
                }
            }
            Term::Prod(arg, f) => {
                let arg2 = arg.clone().map_ty(|ty| ty.apply_subst(subst, k));
                let f2 = f.clone().apply_subst(subst, k + 1);
                if arg.ty.ptr_eq(&arg2.ty) && f.ptr_eq(&f2) {
                    self
                } else {
                    Self::new(Term::Prod(arg2, f2))
                }
            }
            _ => self,
        }
    }

    pub fn subst(self, u: &RTerm<'s>) -> Self {
        self.apply_subst(&psubst_single(u), 0)
    }
}

fn psubst_single<'s, 't>(u: &'t RTerm<'s>) -> impl Fn(usize, usize) -> RTerm<'s> + 't {
    move |n: usize, k: usize| {
        if n == k {
            u.clone() << k
        } else {
            RTerm::new(Term::BVar(n - 1))
        }
    }
}

/// Definition of `<<` for terms.
impl<'s> core::ops::Shl<usize> for RTerm<'s> {
    type Output = Self;

    fn shl(self, rhs: usize) -> Self::Output {
        if rhs == 0 {
            self
        } else {
            self.apply_subst(&|n, _k| RTerm::new(Term::BVar(n + rhs)), 0)
        }
    }
}
