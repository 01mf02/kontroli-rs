use core::hash::Hash;
use dedukti_parse::{term, Command, Intro, Lazy, Symb};
use std::collections::{HashMap, HashSet};

fn map_atoms<A, B, V>(t: term::Term<A, V>, f: &impl Fn(A) -> term::Term<B, V>) -> term::Term<B, V> {
    use term::AppH;
    let args = t.args.into_iter().map(|a| map_atoms(a, f));
    let head = match t.head {
        AppH::Atom(a) => {
            let b = f(a);
            return term::Term {
                head: b.head,
                args: b.args.into_iter().chain(args).collect(),
            };
        }
        AppH::Abst(v, ty, tm) => AppH::Abst(
            v,
            ty.map(|t| Box::new(map_atoms(*t, f))),
            Box::new(map_atoms(*tm, f)),
        ),
        AppH::Prod(v, ty, tm) => AppH::Prod(
            v,
            Box::new(map_atoms(*ty, f)),
            Box::new(map_atoms(*tm, f)),
        ),
    };
    let args = args.collect();
    term::Term { head, args }
}

fn subst<A: Clone + Eq + Hash, V: Clone>(
    map: &HashMap<A, term::Term<A, V>>,
    t: term::Term<A, V>,
) -> term::Term<A, V> {
    map_atoms(t, &|a| match map.get(&a) {
        None => term::Term {
            head: term::AppH::Atom(a),
            args: Vec::new(),
        },
        Some(t) => t.clone(),
    })
}

fn main() -> std::io::Result<()> {
    let mut map = HashMap::new();

    use std::fs::File;
    use std::io::{BufRead, BufReader};

    let args: Vec<String> = std::env::args().collect();
    let set: Result<HashSet<String>, _> = BufReader::new(File::open(&args[2])?).lines().collect();
    let set = set?;

    let file = File::open(&args[1])?;
    let reader = BufReader::new(file);

    let cmds = Lazy::<_, _, String>::new(reader.lines().map(|l| l.unwrap()));
    for cmd in cmds {
        let cmd = cmd.unwrap();
        //println!("{cmd:?}");

        match cmd {
            Command::Intro(name, args, Intro::Definition(_, Some(tm)) | Intro::Theorem(_, tm)) if set.contains(&name) && args.is_empty() => {
                map.insert(Symb::new(name), subst(&map, tm));
            }
            Command::Intro(name, args, intro) => {
                let args = args
                    .into_iter()
                    .map(|(v, ty)| (v, subst(&map, ty)))
                    .collect();
                let intro = intro
                    .map_type(|t| subst(&map, t))
                    .map_term(|t| subst(&map, t));
                println!("{}.", Command::Intro(name, args, intro));

            }
            cmd @ Command::Rules(..) => println!("{cmd}."),
        }
    }

    Ok(())
}
