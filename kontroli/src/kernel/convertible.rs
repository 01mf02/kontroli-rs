//! Convertibility checking.

use super::sterm::{Comb, STerm};
use alloc::{rc::Rc, vec::Vec};

type Constraint<'s, 't> = (STerm<'s, 't>, STerm<'s, 't>);

/// Return true if the given two terms are potentially convertible, and if so,
/// add convertibility constraints that have to be fulfilled.
pub fn step<'s, 't>(cn0: Constraint<'s, 't>, cns: &mut Vec<Constraint<'s, 't>>, eta: bool) -> bool {
    use STerm::*;
    match (&cn0.0, &cn0.1) {
        (Kind, Kind) | (Type, Type) => true,
        (Const(s1), Const(s2)) => s1 == s2,
        (Var(v1), Var(v2)) => v1 == v2,
        (LComb(c1), _) => step((SComb(Rc::new((*c1).into())), cn0.1), cns, eta),
        (_, LComb(c2)) => step((cn0.0, SComb(Rc::new((*c2).into()))), cns, eta),
        (SComb(c1), SComb(c2)) => match (&**c1, &**c2) {
            (Comb::Abst(.., t1), Comb::Abst(.., t2)) => {
                cns.push((t1.clone(), t2.clone()));
                true
            }
            (Comb::Prod(_, ty1, tm1), Comb::Prod(_, ty2, tm2)) => {
                cns.extend([(ty1.clone(), ty2.clone()), (tm1.clone(), tm2.clone())]);
                true
            }
            (Comb::Appl(f1, args1), Comb::Appl(f2, args2)) => {
                if args1.len() == args2.len() {
                    let f = core::iter::once((f1, f2));
                    let args = args1.iter().zip(args2.iter());
                    cns.extend(f.chain(args).map(|(l, r)| (l.clone(), r.clone())));
                    true
                } else {
                    false
                }
            }
            _ => eta && eta_step(cn0, cns),
        },
        _ => eta && eta_step(cn0, cns),
    }
}

fn eta_step<'s, 't>((cn1, cn2): Constraint<'s, 't>, cns: &mut Vec<Constraint<'s, 't>>) -> bool {
    // if at least one side of the constraint is an abstraction
    if let Some((mut a, b)) = match cn2.get_abst() {
        Some((_, b)) => Some((cn1, b)),
        None => cn1.get_abst().map(|(_, b)| (cn2, b)),
    } {
        a.shift_mut(1);
        cns.push((b, a.apply(core::iter::once(STerm::Var(0)))));
        true
    } else {
        false
    }
}
