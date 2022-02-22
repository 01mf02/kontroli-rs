//! Convertibility checking.

use super::{GCtx, Term};
use crate::Arg;
use alloc::vec::Vec;

type Constraint<'s> = (Term<'s>, Term<'s>);

/// Return true if the given two terms are potentially convertible, and if so,
/// add convertibility constraints that have to be fulfilled.
fn step<'s>((cn1, cn2): Constraint<'s>, cns: &mut Vec<Constraint<'s>>, eta: bool) -> bool {
    use crate::term::{Term::*, TermC::*};
    match (&cn1, &cn2) {
        (Kind, Kind) | (Type, Type) => true,
        (Symb(s1), Symb(s2)) => s1 == s2,
        (BVar(v1), BVar(v2)) => v1 == v2,
        (Comb(c1), Comb(c2)) => match (&**c1, &**c2) {
            (Abst(_, t1), Abst(_, t2)) => {
                cns.push((t1.clone(), t2.clone()));
                true
            }
            (Prod(Arg { ty: ty1, .. }, tm1), Prod(Arg { ty: ty2, .. }, tm2)) => {
                cns.push((ty1.clone(), ty2.clone()));
                cns.push((tm1.clone(), tm2.clone()));
                true
            }
            (Appl(f1, args1), Appl(f2, args2)) => {
                if args1.len() == args2.len() {
                    cns.push((f1.clone(), f2.clone()));
                    cns.extend(args1.clone().into_iter().zip(args2.clone()));
                    true
                } else {
                    false
                }
            }
            _ => eta && eta_step((cn1, cn2), cns),
        },
        _ => eta && eta_step((cn1, cn2), cns),
    }
}

fn eta_step<'s>((cn1, cn2): Constraint<'s>, cns: &mut Vec<Constraint<'s>>) -> bool {
    // if at least one side of the constraint is an abstraction
    if let Some((a, b)) = match cn2.get_abst() {
        Some((_, b)) => Some((cn1, b)),
        None => cn1.get_abst().map(|(_, b)| (cn2, b)),
    } {
        let app = a.shift(1).apply(Vec::from([Term::BVar(0)]));
        cns.push((b.clone(), app));
        true
    } else {
        false
    }
}

impl<'s> Term<'s> {
    /// Return true if the given terms have a common redex.
    pub fn convertible(tm1: Self, tm2: Self, gc: &GCtx<'s>) -> bool {
        let mut cns = Vec::from([(tm1, tm2)]);
        loop {
            match cns.pop() {
                Some((cn1, cn2)) => {
                    trace!("convertible: {} ~? {}", cn1, cn2);
                    if cn1 != cn2 && !step((cn1.whnf(gc), cn2.whnf(gc)), &mut cns, gc.eta) {
                        break false;
                    }
                }
                None => break true,
            }
        }
    }
}
