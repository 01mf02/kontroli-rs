//! Convertibility checking.

use super::sterm::{Comb, SComb, STerm};
use alloc::{rc::Rc, vec::Vec};

type Pair<T> = (T, T);
type Constraints<'s, 't> = Vec<Pair<STerm<'s, 't>>>;

/// Return true if the given two terms are potentially convertible, and if so,
/// add convertibility constraints that have to be fulfilled.
pub fn step<'s, 't>(cn0: Pair<STerm<'s, 't>>, cns: &mut Constraints<'s, 't>, eta: bool) -> bool {
    use STerm::*;
    let get = |rc: Rc<Comb<_, _>>| Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone());
    match (cn0.0, cn0.1) {
        (Kind, Kind) | (Type, Type) => true,
        (Const(s1), Const(s2)) => s1 == s2,
        (Var(v1), Var(v2)) => v1 == v2,
        (LComb(c1), LComb(c2)) => step2((c1.into(), c2.into()), cns, eta),
        (LComb(c1), SComb(c2)) => step2((c1.into(), get(c2)), cns, eta),
        (SComb(c1), LComb(c2)) => step2((get(c1), c2.into()), cns, eta),
        (SComb(c1), SComb(c2)) => step2((get(c1), get(c2)), cns, eta),
        (SComb(b), a) | (a, SComb(b)) if eta => step1(get(b), a, cns),
        (LComb(b), a) | (a, LComb(b)) if eta => step1(b.into(), a, cns),
        _ => false,
    }
}

fn step2<'s, 't>(cn0: Pair<SComb<'s, 't>>, cns: &mut Constraints<'s, 't>, eta: bool) -> bool {
    match (cn0.0, cn0.1) {
        (Comb::Abst(.., t1), Comb::Abst(.., t2)) => cns.push((t1, t2)),
        (Comb::Prod(_, ty1, tm1), Comb::Prod(_, ty2, tm2)) => cns.extend([(ty1, ty2), (tm1, tm2)]),
        (Comb::Appl(f1, args1), Comb::Appl(f2, args2)) if args1.len() == args2.len() => {
            let f = core::iter::once((f1, f2));
            let args = args1.into_iter().zip(args2.into_iter());
            cns.extend(f.chain(args));
        }
        (Comb::Abst(.., b), a) | (a, Comb::Abst(.., b)) if eta => {
            cns.push((b, shift_app(STerm::SComb(a.into()))));
        }
        _ => return false,
    }
    true
}

fn step1<'s, 't>(c: SComb<'s, 't>, tm: STerm<'s, 't>, cns: &mut Constraints<'s, 't>) -> bool {
    if let Comb::Abst(.., b) = c {
        cns.push((b, shift_app(tm)));
        true
    } else {
        false
    }
}

fn shift_app<'s, 't>(mut tm: STerm<'s, 't>) -> STerm<'s, 't> {
    tm.shift_mut(1);
    tm.apply(core::iter::once(STerm::Var(0)))
}
