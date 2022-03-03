/// Argument of a binder.
/// For example, the `x` and `A` in the term `\ x : A => t`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arg<Id, Ty> {
    pub id: Id,
    pub ty: Ty,
}

impl<Id, Ty> Arg<Id, Ty> {
    pub fn map_type<U>(self, f: impl FnOnce(Ty) -> U) -> Arg<Id, U> {
        let ty = f(self.ty);
        Arg { id: self.id, ty }
    }
}
