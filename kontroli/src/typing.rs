/// Normalised, verified form of introduction commands.
///
/// An introduction command can have many shapes:
/// `x: A`, `x := t`, `x: A := t`, ...
/// A typing provides a uniform presentation of introduction commands
/// with respect to type checking.
#[derive(Clone)]
pub struct Typing<Ty, Tm = Ty> {
    /// local context
    pub lc: crate::Stack<Ty>,
    pub ty: Ty,
    pub tm: Tm,
}
