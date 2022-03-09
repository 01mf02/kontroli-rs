/// Normalised, verified form of introduction commands.
///
/// An introduction command can have many shapes:
/// `x: A`, `x := t`, `x: A := t`, ...
/// A typing provides a uniform presentation of introduction commands
/// with respect to type checking.
#[derive(Clone)]
pub struct Typing<Ty, Tm = Ty> {
    /// local context
    pub lc: alloc::vec::Vec<Ty>,
    pub ty: Ty,
    pub tm: Tm,
}

impl<Ty, Tm> Typing<Ty, Tm> {
    pub fn new(ty: Ty, tm: Tm) -> Self {
        Self {
            lc: Default::default(),
            ty,
            tm,
        }
    }
}
