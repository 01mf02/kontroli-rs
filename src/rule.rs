use super::*;

enum Prepattern {
    Appl(String, Vec<Box<Prepattern>>),
    Abst(String, Box<Prepattern>),
}

type Miller = usize;

enum Pattern {
    MVar(Miller, Vec<DeBruijn>),
    Abst(String, Box<Pattern>),
    Symb(String, Vec<Box<Pattern>>),
    BVar(DeBruijn, Vec<Box<Pattern>>),
}

pub struct Rule {
    pat: Pattern,
    rhs: Term,
}
