//! Convert from scoped to shared structures.

use super::{Intro, RTerm, Rc, Rule, Term};
use crate::error::ScopeError as Error;
use crate::pattern::{Pattern, TopPattern};
use crate::{parse, scope};
use crate::{Share, Symbols};
use core::convert::TryFrom;

impl<'s> Share<'s, Term<'s>> for scope::Term<parse::Symbol> {
    /// Share a closed term.
    ///
    /// Kontroli differs from Dedukti by allowing to
    /// abstract over variables of name `_`:
    ///
    /// ~~~
    /// # use kontroli::{Error, Share, Symbols};
    /// # use kontroli::rc::Term;
    /// # use kontroli::scope::Term as STerm;
    /// let syms: Symbols = vec!["A"].into_iter().collect();
    /// let tm: Term = STerm::parse(r"_ : A => _.\n")?.share(&syms)?;
    /// # Ok::<_, Error>(())
    /// ~~~
    fn share(self, syms: &Symbols<'s>) -> Result<Term<'s>, Error> {
        self.try_map(|c| c.share(syms), Rc::new, |tm| tm.share(syms))
    }
}

impl<'s> Share<'s, RTerm<'s>> for scope::BTerm<parse::Symbol> {
    fn share(self, syms: &Symbols<'s>) -> Result<RTerm<'s>, Error> {
        Ok(RTerm::new(self.get().share(syms)?))
    }
}

impl<'s> Share<'s, Rule<'s>> for scope::Rule<parse::Symbol> {
    fn share(self, syms: &Symbols<'s>) -> Result<Rule<'s>, Error> {
        let ctx = self.ctx;
        let lhs = Pattern::try_from(self.lhs).map_err(|_| Error::NoPrepattern)?;
        let lhs = lhs.try_map(&|c| c.share(syms))?;
        let lhs = TopPattern::try_from(lhs).map_err(|_| Error::NoTopPattern)?;
        let rhs = self.rhs.share(&syms)?;
        Ok(Rule { ctx, lhs, rhs })
    }
}

impl<'s> Share<'s, Intro<'s>> for scope::Intro<parse::Symbol> {
    fn share(self, syms: &Symbols<'s>) -> Result<Intro<'s>, Error> {
        self.map_type_err(|ty| ty.share(syms))?
            .map_term_err(|tm| tm.share(syms))
    }
}
