use core::fmt::{self, Display};

/// Combinator term.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Comb<Id, Tm> {
    Appl(Tm, alloc::vec::Vec<Tm>),
    Prod(Id, Tm, Tm),
    Abst(Id, Option<Tm>, Tm),
}

impl<Id, Tm> Comb<Id, Tm> {
    pub fn is_whnf(&self, no_args: impl FnOnce() -> bool) -> bool {
        match self {
            Comb::Appl(..) => false,
            Comb::Prod(..) => true,
            Comb::Abst(..) => no_args(),
        }
    }
}

/// This is usually used to convert LComb to SComb.
impl<'c, Id, Tm, Tm2: From<&'c Tm>> From<&'c Comb<Id, Tm>> for Comb<&'c Id, Tm2> {
    fn from(comb: &'c Comb<Id, Tm>) -> Self {
        match comb {
            Comb::Appl(tm, args) => Self::Appl(Tm2::from(tm), args.iter().map(Tm2::from).collect()),
            Comb::Prod(id, ty, tm) => Self::Prod(id, Tm2::from(ty), Tm2::from(tm)),
            Comb::Abst(id, ty, tm) => Self::Abst(id, ty.as_ref().map(Tm2::from), Tm2::from(tm)),
        }
    }
}

/// This is usually used to convert SComb to LComb.
impl<'s, 'c, Id: Clone, Tm, Tm2: TryFrom<&'c Tm>> TryFrom<&'c Comb<&'s Id, Tm>> for Comb<Id, Tm2> {
    type Error = Tm2::Error;
    fn try_from(comb: &'c Comb<&'s Id, Tm>) -> Result<Self, Self::Error> {
        match comb {
            Comb::Appl(head, args) => Ok(Self::Appl(
                Tm2::try_from(head)?,
                args.iter().map(Tm2::try_from).collect::<Result<_, _>>()?,
            )),
            Comb::Prod(id, ty, tm) => Ok(Self::Prod(
                (*id).clone(),
                Tm2::try_from(ty)?,
                Tm2::try_from(tm)?,
            )),
            Comb::Abst(id, ty, tm) => Ok(Self::Abst(
                (*id).clone(),
                ty.as_ref().map(Tm2::try_from).transpose()?,
                Tm2::try_from(tm)?,
            )),
        }
    }
}

impl<V: Display, Tm: Display> Display for Comb<V, Tm> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Appl(head, tail) => crate::app::format(head, tail, f),
            Self::Prod(id, ty, tm) => write!(f, "(Π {} : {}. {})", id, ty, tm),
            Self::Abst(id, None, tm) => write!(f, "(λ {}. {})", id, tm),
            Self::Abst(id, Some(ty), tm) => write!(f, "(λ {} : {}. {})", id, ty, tm),
        }
    }
}
