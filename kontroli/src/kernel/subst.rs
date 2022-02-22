//! Substitution and shifting for terms.

use super::sterm::{Comb, SComb, STerm};
use alloc::{rc::Rc, vec::Vec};

impl<'s, 't> STerm<'s, 't> {
    pub fn apply_subst<S>(self, subst: &S, k: usize) -> Self
    where
        S: Fn(usize, usize) -> STerm<'s, 't>,
    {
        match self {
            Self::Var(n) if n >= k => subst(n, k),
            Self::LComb(c) => SComb::from(c)
                .apply_subst(subst, k)
                .map(|c| Self::SComb(Rc::new(c)))
                .unwrap_or(self),
            Self::SComb(ref c) => c
                .apply_subst(subst, k)
                .map(|c| Self::SComb(Rc::new(c)))
                .unwrap_or(self),
            _ => self,
        }
    }

    pub fn subst(self, u: &Self) -> Self {
        self.apply_subst(&u.psubst_single(), 0)
    }

    fn psubst_single<'a>(&'a self) -> impl Fn(usize, usize) -> STerm<'s, 't> + 'a {
        move |n: usize, k: usize| {
            if n == k {
                self.clone().shift(k)
            } else {
                Self::Var(n - 1)
            }
        }
    }

    pub fn shift(self, rhs: usize) -> Self {
        if rhs == 0 {
            self
        } else {
            self.apply_subst(&|n, _k| Self::Var(n + rhs), 0)
        }
    }
}

impl<'s, 't> SComb<'s, 't> {
    pub fn apply_subst<S>(&self, subst: &S, k: usize) -> Option<Self>
    where
        S: Fn(usize, usize) -> STerm<'s, 't>,
    {
        let sub = |tm: STerm<'s, 't>| tm.apply_subst(subst, k);
        match &*self {
            Comb::Appl(f, args) => {
                let f2 = sub(f.clone());
                let args2: Vec<_> = args.iter().cloned().map(sub).collect();
                if !f.ptr_eq(&f2) || !args.iter().zip(args2.iter()).all(|(a, a2)| a.ptr_eq(a2)) {
                    return Some(Comb::Appl(f2, args2));
                }
            }
            Comb::Abst(arg, f) => {
                let arg2 = arg.clone().map_type(|o| o.map(sub));
                let f2 = f.clone().apply_subst(subst, k + 1);
                if !arg.eq_ty(&arg2, STerm::ptr_eq) || !f.ptr_eq(&f2) {
                    return Some(Comb::Abst(arg2, f2));
                }
            }
            Comb::Prod(arg, f) => {
                let arg2 = arg.clone().map_type(sub);
                let f2 = f.clone().apply_subst(subst, k + 1);
                if !arg.ty.ptr_eq(&arg2.ty) || !f.ptr_eq(&f2) {
                    return Some(Self::Prod(arg2, f2));
                }
            }
        };
        None
    }
}
