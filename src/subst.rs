use crate::term::{Arg, RTerm, Term};

impl Arg {
    /// Return the substitution of an argument only if it changed.
    fn subst<S>(&self, subst: &S, k: usize) -> Option<Self>
    where
        S: Fn(usize, usize) -> RTerm,
    {
        let ty = self.ty.as_ref()?;
        let ty2 = ty.clone().apply_subst(subst, k);
        if ty.ptr_eq(&ty2) {
            None
        } else {
            Some(Self {
                id: self.id.clone(),
                ty: Some(ty2),
            })
        }
    }

    /// Return the substitution of an argument and a term
    /// only if at least one them changed.
    fn subst_bound<S>(&self, f: &RTerm, subst: &S, k: usize) -> Option<(Self, RTerm)>
    where
        S: Fn(usize, usize) -> RTerm,
    {
        let f2 = f.clone().apply_subst(subst, k + 1);
        if f.ptr_eq(&f2) {
            Some((self.subst(subst, k)?, f2))
        } else {
            Some((self.subst(subst, k).unwrap_or_else(|| self.clone()), f2))
        }
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
            Term::Abst(arg, f) => match arg.subst_bound(f, subst, k) {
                Some((arg2, f2)) => Self::new(Term::Abst(arg2, f2)),
                None => self,
            },
            Term::Prod(arg, f) => match arg.subst_bound(f, subst, k) {
                Some((arg2, f2)) => Self::new(Term::Prod(arg2, f2)),
                None => self,
            },
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
