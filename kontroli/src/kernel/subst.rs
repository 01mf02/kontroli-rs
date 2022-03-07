//! Substitution and shifting for terms.

use super::sterm::{Comb, SComb, STerm};

impl<'s, 't> STerm<'s, 't> {
    pub fn apply_subst<S>(&mut self, subst: &S, k: usize) -> bool
    where
        S: Fn(usize, usize) -> STerm<'s, 't>,
    {
        match self {
            Self::Var(n) if *n >= k => *self = subst(*n, k),
            Self::LComb(c) => {
                let mut c = SComb::from(*c);
                let change = c.apply_subst(subst, k);
                *self = Self::SComb(c.into());
                return change;
            }
            Self::SComb(c) => match alloc::rc::Rc::get_mut(c) {
                Some(c) => return c.apply_subst(subst, k),
                None => {
                    let mut c2: SComb = (**c).clone();
                    if c2.apply_subst(subst, k) {
                        *c = c2.into();
                    } else {
                        return false;
                    }
                }
            },
            _ => return false,
        }
        true
    }

    pub fn subst(mut self, u: &Self) -> Self {
        self.apply_subst(&u.psubst_single(), 0);
        self
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

    pub fn shift(mut self, rhs: usize) -> Self {
        if rhs != 0 {
            self.shift_mut(rhs);
        }
        self
    }

    pub fn shift_mut(&mut self, rhs: usize) {
        self.apply_subst(&|n, _k| Self::Var(n + rhs), 0);
    }
}

impl<'s, 't> SComb<'s, 't> {
    pub fn apply_subst<S>(&mut self, subst: &S, k: usize) -> bool
    where
        S: Fn(usize, usize) -> STerm<'s, 't>,
    {
        let sub = |tm: &mut STerm<'s, 't>| tm.apply_subst(subst, k);
        let mut change;
        match self {
            Comb::Appl(f, args) => {
                change = sub(f);
                args.iter_mut().for_each(|x| change = sub(x) || change);
            }
            Comb::Abst(_, ty, f) => {
                change = f.apply_subst(subst, k + 1);
                ty.iter_mut().for_each(|x| change = sub(x) || change);
            }
            Comb::Prod(_, ty, f) => {
                change = sub(ty);
                change = f.apply_subst(subst, k + 1) || change;
            }
        }
        change
    }
}
