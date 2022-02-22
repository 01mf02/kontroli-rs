//! Convertibility checking.

use super::sterm::{Comb, STerm};
use crate::Arg;
use alloc::vec::Vec;

type Constraint<'s, 't> = (STerm<'s, 't>, STerm<'s, 't>);

/// Return true if the given two terms are potentially convertible, and if so,
/// add convertibility constraints that have to be fulfilled.
pub fn step<'s, 't>(cn0: Constraint<'s, 't>, cns: &mut Vec<Constraint<'s, 't>>, eta: bool) -> bool {
    use STerm::*;
    match (&cn0.0, &cn0.1) {
        (Kind, Kind) | (Type, Type) => true,
        (Const(s1), Const(s2)) => s1 == s2,
        (Var(v1), Var(v2)) => v1 == v2,
        (LComb(c1), LComb(c2)) => {
            comb_step(c1, c2, cns).unwrap_or_else(|| eta && eta_step(cn0, cns))
        }
        (LComb(c1), SComb(c2)) => {
            comb_step(c1, &**c2, cns).unwrap_or_else(|| eta && eta_step(cn0, cns))
        }
        (SComb(c1), LComb(c2)) => {
            comb_step(&**c1, c2, cns).unwrap_or_else(|| eta && eta_step(cn0, cns))
        }
        (SComb(c1), SComb(c2)) => {
            comb_step(&**c1, &**c2, cns).unwrap_or_else(|| eta && eta_step(cn0, cns))
        }
        _ => eta && eta_step(cn0, cns),
    }
}

pub fn comb_step<'s, 't, 'l, 'r, IL, IR, TL, TR>(
    cbl: &'l Comb<IL, TL>,
    cbr: &'r Comb<IR, TR>,
    cns: &mut Vec<Constraint<'s, 't>>,
) -> Option<bool>
where
    &'l TL: Into<STerm<'s, 't>>,
    &'r TR: Into<STerm<'s, 't>>,
{
    use Comb::*;
    match (cbl, cbr) {
        (Abst(_, t1), Abst(_, t2)) => {
            cns.push((t1.into(), t2.into()));
            Some(true)
        }
        (Prod(Arg { ty: ty1, .. }, tm1), Prod(Arg { ty: ty2, .. }, tm2)) => {
            cns.extend([(ty1.into(), ty2.into()), (tm1.into(), tm2.into())]);
            Some(true)
        }
        (Appl(f1, args1), Appl(f2, args2)) => {
            if args1.len() == args2.len() {
                let f = core::iter::once((f1, f2));
                let args = args1.iter().zip(args2.iter());
                cns.extend(f.chain(args).map(|(l, r)| (l.into(), r.into())));
                Some(true)
            } else {
                Some(false)
            }
        }
        _ => None,
    }
}

fn eta_step<'s, 't>((cn1, cn2): Constraint<'s, 't>, cns: &mut Vec<Constraint<'s, 't>>) -> bool {
    // if at least one side of the constraint is an abstraction
    if let Some((a, b)) = match cn2.get_abst() {
        Some((_, b)) => Some((cn1, b)),
        None => cn1.get_abst().map(|(_, b)| (cn2, b)),
    } {
        let app = a.shift(1).apply(core::iter::once(STerm::Var(0)));
        cns.push((b, app));
        true
    } else {
        false
    }
}
