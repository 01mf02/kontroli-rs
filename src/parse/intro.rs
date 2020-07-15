use super::term::{Arg, BTerm};
use alloc::{boxed::Box, vec::Vec};

pub type Intro = crate::Intro<BTerm, BTerm>;

impl Intro {
    pub fn parametrise(self, args: Vec<Arg>) -> Self {
        self.map_type(|tm| Box::new(tm.prods(args.iter().cloned().rev())))
            .map_term(|tm| Box::new(tm.absts(args.into_iter().rev())))
    }
}
