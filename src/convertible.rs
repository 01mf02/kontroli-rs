//! Convertibility checking.

use crate::term::{Arg, RTerm, Term};
use crate::Signature;
use alloc::{vec, vec::Vec};

/// Return true if the given two terms are potentially convertible, and if so,
/// add convertibility constraints that have to be fulfilled.
fn step(cn1: RTerm, cn2: RTerm, cns: &mut Vec<(RTerm, RTerm)>, eta: bool) -> bool {
    use Term::*;
    match (&*cn1, &*cn2) {
        (Kind, Kind) | (Type, Type) => true,
        (Symb(s1), Symb(s2)) => s1 == s2,
        (BVar(v1), BVar(v2)) => v1 == v2,
        (Abst(_, t1), Abst(_, t2)) => {
            cns.push((t1.clone(), t2.clone()));
            true
        }
        (Prod(Arg { ty: Some(ty1), .. }, tm1), Prod(Arg { ty: Some(ty2), .. }, tm2)) => {
            cns.push((ty1.clone(), ty2.clone()));
            cns.push((tm1.clone(), tm2.clone()));
            true
        }
        (a, Abst(_, b)) | (Abst(_, b), a) if eta => {
            let app = (RTerm::new(a.clone()) << 1).apply(vec![RTerm::new(BVar(0))]);
            cns.push((b.clone(), app));
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
        _ => false,
    }
}

impl RTerm {
    /// Return true if the given terms have a common redex.
    pub fn convertible(tm1: Self, tm2: Self, sig: &Signature) -> bool {
        let mut cns = vec![(tm1, tm2)];
        loop {
            match cns.pop() {
                Some((cn1, cn2)) => {
                    trace!("convertible: {} ~? {}", cn1, cn2);
                    if cn1 != cn2 && !step(cn1.whnf(sig), cn2.whnf(sig), &mut cns, sig.eta) {
                        break false;
                    }
                }
                None => break true,
            }
        }
    }
}
