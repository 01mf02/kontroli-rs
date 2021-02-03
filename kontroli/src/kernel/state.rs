//! Abstract machines for the lazy evaluation of terms.

use super::reduce::WState;
use super::{RTerm, Term};
use crate::stack;
use alloc::rc::Rc;
use core::cell::{Ref, RefCell, RefMut};
use lazy_st::Thunk;

/// An abstract machine representing arguments applied to a substituted term.
///
/// This representation allows for the lazy evaluation of terms.
///
/// See section 5.1 of the following reference:
/// Asperti, A.; Ricciotti, W.; Sacerdoti Coen, C.; Tassi, E. (2009).
/// "A compact kernel for the calculus of inductive constructions".
/// *Sadhana*. **34**: 71–144.
/// doi: [10.1007/s12046-009-0003-3](https://doi.org/10.1007%2Fs12046-009-0003-3).
#[derive(Clone, Default)]
pub struct State<'s> {
    pub ctx: Context<'s>,
    pub term: RTerm<'s>,
    pub stack: Stack<'s>,
}

impl<'s> State<'s> {
    /// Construct a new state from a reference to a term.
    ///
    /// This does not yet evaluate anything, as can be seen from following example:
    ///
    /// ~~~
    /// # use kontroli::Error;
    /// # use kontroli::scope::{Term as STerm, Symbols};
    /// # use kontroli::rc::{RTerm, Signature, Term};
    /// # use kontroli::rc::state::State;
    /// let syms = Symbols::new();
    ///
    /// let term = Term::from(STerm::parse(r"(x => x) (x => x).", &syms)?);
    /// let rterm = RTerm::new(term);
    ///
    /// let state = State::new(rterm.clone());
    /// assert!(RTerm::ptr_eq(&RTerm::from(state), &rterm));
    /// # Ok::<(), Error>(())
    /// ~~~
    pub fn new(term: RTerm<'s>) -> Self {
        Self {
            ctx: Context::new(),
            term,
            stack: Stack::new(),
        }
    }
}

/// Map from de Bruijn indices in the term of the abstract machine to lazy terms.
pub type Context<'s> = stack::Stack<RTTerm<'s>>;

/// Arguments to the abstract machine term.
pub type Stack<'s> = stack::Stack<RState<'s>>;

/// A shared lazy term constructed from a shared mutable state.
#[derive(Clone)]
pub struct RTTerm<'s>(Rc<Thunk<RState<'s>, RTerm<'s>>>);

impl<'s> RTTerm<'s> {
    pub fn new(st: RState<'s>) -> Self {
        Self(Rc::new(Thunk::new(st)))
    }

    /// Force evaluation of the lazy term.
    pub fn force(&self) -> &RTerm<'s> {
        &**self.0
    }
}

/// A shared mutable state.
///
/// We use `RefCell` instead of `Thunk` here
/// because evaluation requires a signature and
/// because we sometimes wish to access the original state.
#[derive(Clone)]
pub struct RState<'s>(Rc<RefCell<WState<'s>>>);

impl<'s> RState<'s> {
    pub fn new(wst: WState<'s>) -> Self {
        Self(Rc::new(RefCell::new(wst)))
    }

    pub fn borrow(&self) -> Ref<WState<'s>> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<WState<'s>> {
        self.0.borrow_mut()
    }
}

impl<'s> lazy_st::Evaluate<RTerm<'s>> for RState<'s> {
    fn evaluate(self) -> RTerm<'s> {
        RTerm::from(self)
    }
}

impl<'s> From<RState<'s>> for RTerm<'s> {
    fn from(s: RState<'s>) -> Self {
        RTerm::from(s.borrow_state().clone())
    }
}

impl<'s> From<State<'s>> for RTerm<'s> {
    fn from(state: State<'s>) -> Self {
        state
            .term
            .psubst(&state.ctx)
            .apply(state.stack.into_iter().map(Self::from).collect())
    }
}

impl<'s> RTerm<'s> {
    fn psubst(self, args: &Context<'s>) -> Self {
        if args.is_empty() {
            self
        } else {
            self.apply_subst(&psubst(args), 0)
        }
    }
}

fn psubst<'s, 'c>(args: &'c Context<'s>) -> impl Fn(usize, usize) -> RTerm<'s> + 'c {
    move |n: usize, k: usize| match args.get(n - k) {
        Some(arg) => arg.force().clone() << k,
        None => RTerm::new(Term::BVar(n - args.len())),
    }
}
