//! Convert from scoped to shared structures.

use super::{Intro, RTerm, Rc, Rule, Term};
use crate::error::ScopeError as Error;
use crate::pattern::{Pattern, TopPattern};
use crate::{parse, scope};
use crate::{Share, Symbols};
use core::convert::TryFrom;

impl<'s> Term<'s> {
    /// Share a closed term.
    ///
    /// Kontroli differs from Dedukti by allowing to
    /// abstract over variables of name `_`:
    ///
    /// ~~~
    /// # use kontroli::{Error, Symbols};
    /// # use kontroli::rc::Term;
    /// # use kontroli::scope::Term as STerm;
    /// let syms: Symbols = vec!["A"].into_iter().collect();
    /// let tm = Term::share(STerm::parse(r"_ : A => _.\n")?, &syms)?;
    /// # Ok::<_, Error>(())
    /// ~~~
    pub fn share(tm: scope::Term<parse::Symbol>, syms: &Symbols<'s>) -> Result<Self, Error> {
        tm.try_map(|c| c.share(syms), Rc::new, |tm| RTerm::share(tm, syms))
    }
}

impl<'s> RTerm<'s> {
    pub fn share(tm: scope::BTerm<parse::Symbol>, syms: &Symbols<'s>) -> Result<Self, Error> {
        Ok(Self::new(Term::share(tm.get(), syms)?))
    }
}

impl<'s> Rule<'s> {
    pub fn share(rule: scope::Rule<parse::Symbol>, syms: &Symbols<'s>) -> Result<Self, Error> {
        let ctx = rule.ctx;
        let lhs = Pattern::try_from(rule.lhs).map_err(|_| Error::NoPrepattern)?;
        let lhs = lhs.try_map(&|c| c.share(syms))?;
        let lhs = TopPattern::try_from(lhs).map_err(|_| Error::NoTopPattern)?;
        let rhs = RTerm::share(rule.rhs, &syms)?;
        Ok(Self { ctx, lhs, rhs })
    }
}

impl<'s> Intro<'s> {
    pub fn share(intro: scope::Intro<parse::Symbol>, syms: &Symbols<'s>) -> Result<Self, Error> {
        intro
            .map_type_err(|ty| RTerm::share(ty, syms))?
            .map_term_err(|tm| RTerm::share(tm, syms))
    }
}
