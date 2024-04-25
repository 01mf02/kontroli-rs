//! For every constant appearing in a Dedukti file, output its number of occurrences.

use core::hash::Hash;
use dedukti_parse::{term, Command, Lazy, Symb};
use std::collections::HashMap;

fn atoms<A, V>(t: &term::Term<A, V>) -> Box<dyn Iterator<Item = &A> + '_> {
    use term::AppH;
    let head = match &t.head {
        AppH::Atom(a) => Box::new(core::iter::once(a)) as Box<dyn Iterator<Item = _>>,
        AppH::Abst(_, ty, tm) => Box::new(ty.iter().flat_map(|ty| atoms(ty)).chain(atoms(tm))),
        AppH::Prod(_, ty, tm) => Box::new(atoms(ty).chain(atoms(tm))),
    };
    Box::new(head.chain(t.args.iter().flat_map(|a| atoms(a))))
}

fn insert<C: Clone + Eq + Hash>(map: &mut HashMap<C, usize>, a: &term::Atom<C>) {
    if let term::Atom::Const(c) = a {
        *map.entry(c.clone()).or_default() += 1;
    }
}

fn main() -> std::io::Result<()> {
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    let mut map = HashMap::new();
    let f = |m: &mut _, t: &_| atoms(t).for_each(|a| insert(m, a));

    let args: Vec<String> = std::env::args().collect();
    let file = File::open(&args[1])?;
    let reader = BufReader::new(file);

    let cmds = Lazy::<_, _, String>::new(reader.lines().map(|l| l.unwrap()));
    for cmd in cmds {
        let cmd = cmd.unwrap();
        //println!("{cmd:?}");

        if let Command::Intro(name, args, intro) = cmd {
            map.insert(Symb::new(name), 0);
            args.iter().for_each(|(_, t)| f(&mut map, &t));
            intro
                .map_type(|t| {
                    f(&mut map, &t);
                    t
                })
                .map_term(|t| {
                    f(&mut map, &t);
                    t
                });
        }
    }

    for (sym, count) in map {
        println!("{count}\t{sym}");
    }
    Ok(())
}
