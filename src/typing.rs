/// Normalised, verified form of introduction commands.
///
/// An introduction command can have many shapes:
/// `x: A`, `x := t`, `x: A := t`, ...
/// A typing provides a uniform presentation of introduction commands
/// with respect to type checking.
#[derive(Clone)]
pub struct Typing<T> {
    pub typ: T,
    pub term: Option<(T, Check)>,
    pub rewritable: bool,
}

/// Have we assured that a given term matches a given type?
#[derive(Clone)]
pub enum Check {
    Checked,
    Unchecked,
}
