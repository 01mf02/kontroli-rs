/// Argument of a binder.
/// For example, the `x` and `A` in the term `\ x : A => t`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arg<Id, Ty> {
    pub id: Id,
    pub ty: Ty,
}

impl<Id, Ty> Arg<Id, Ty> {
    pub fn map_id<F, J>(self, f: F) -> Arg<J, Ty>
    where
        F: FnOnce(Id) -> J,
    {
        let id = f(self.id);
        Arg { id, ty: self.ty }
    }

    pub fn map_ty<F, U>(self, f: F) -> Arg<Id, U>
    where
        F: FnOnce(Ty) -> U,
    {
        let ty = f(self.ty);
        Arg { id: self.id, ty }
    }

    pub fn map_ty_res<F, U, E>(self, f: F) -> Result<Arg<Id, U>, E>
    where
        F: FnOnce(Ty) -> Result<U, E>,
    {
        let ty = f(self.ty)?;
        Ok(Arg { id: self.id, ty })
    }
}
