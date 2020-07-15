/// Argument of a binder.
/// For example, the `x` and `A` in the term `\ x : A => t`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arg<Id, Ty> {
    pub id: Id,
    pub ty: Ty,
}

impl<Id, Ty> Arg<Id, Ty> {
    pub fn map<F, U>(self, f: F) -> Arg<Id, U>
    where
        F: FnOnce(Ty) -> U,
    {
        let ty = f(self.ty);
        Arg { id: self.id, ty }
    }
}
