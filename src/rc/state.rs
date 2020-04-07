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
pub struct State {
    pub ctx: Context,
    pub term: RTerm,
    pub stack: Stack,
}

impl State {
    /// Construct a new state from a reference to a term.
    ///
    /// This does not yet evaluate anything, as can be seen from following example:
    ///
    /// ~~~
    /// # use kontroli::rc::{Error, RTerm, Signature, Symbols, Term};
    /// # use kontroli::rc::state::State;
    /// let syms = Symbols::new();
    ///
    /// let term = Term::parse(r"(\ x => x) (\ x => x).", &syms)?;
    /// let rterm = RTerm::new(term);
    ///
    /// let state = State::new(rterm.clone());
    /// assert!(RTerm::ptr_eq(&RTerm::from(state), &rterm));
    /// # Ok::<(), Error>(())
    /// ~~~
    pub fn new(term: RTerm) -> Self {
        Self {
            ctx: Context::new(),
            term,
            stack: Stack::new(),
        }
    }
}

/// Map from de Bruijn indices in the term of the abstract machine to lazy terms.
pub type Context = stack::Stack<RTTerm>;

/// Arguments to the abstract machine term.
pub type Stack = stack::Stack<RState>;

/// A shared lazy term constructed from a shared mutable state.
#[derive(Clone)]
pub struct RTTerm(Rc<Thunk<RState, RTerm>>);

impl RTTerm {
    pub fn new(st: RState) -> Self {
        Self(Rc::new(Thunk::new(st)))
    }

    /// Force evaluation of the lazy term.
    pub fn force(&self) -> &RTerm {
        &**self.0
    }
}

/// A shared mutable state.
///
/// We use `RefCell` instead of `Thunk` here
/// because evaluation requires a signature and
/// because we sometimes wish to access the original state.
#[derive(Clone)]
pub struct RState(Rc<RefCell<WState>>);

impl RState {
    pub fn new(wst: WState) -> Self {
        Self(Rc::new(RefCell::new(wst)))
    }

    pub fn borrow(&self) -> Ref<WState> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<WState> {
        self.0.borrow_mut()
    }
}

impl lazy_st::Evaluate<RTerm> for RState {
    fn evaluate(self) -> RTerm {
        RTerm::from(self)
    }
}

impl From<RState> for RTerm {
    fn from(s: RState) -> Self {
        RTerm::from(s.borrow_state().clone())
    }
}

impl From<State> for RTerm {
    fn from(state: State) -> Self {
        state
            .term
            .psubst(&state.ctx)
            .apply(state.stack.into_iter().map(Self::from).collect())
    }
}

impl RTerm {
    fn psubst(self, args: &Context) -> Self {
        if args.is_empty() {
            self
        } else {
            self.apply_subst(&psubst(args), 0)
        }
    }
}

fn psubst(args: &Context) -> impl Fn(usize, usize) -> RTerm + '_ {
    move |n: usize, k: usize| match args.get(n - k) {
        Some(arg) => arg.force().clone() << k,
        None => RTerm::new(Term::BVar(n - args.len())),
    }
}
