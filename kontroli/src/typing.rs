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

impl<Ty, Tm> Typing<Ty, Tm> {
    pub fn map_tm<Tm2>(self, f: impl Fn(Tm) -> Tm2) -> Typing<Ty, Tm2> {
        Typing {
            lc: self.lc,
            ty: self.ty,
            tm: f(self.tm),
        }
    }
}

/// Have we assured that a given term matches a given type?
#[derive(Clone)]
pub enum Check {
    Checked,
    Unchecked,
}
