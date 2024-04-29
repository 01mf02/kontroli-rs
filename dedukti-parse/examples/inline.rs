use core::hash::Hash;
use dedukti_parse::{term, Command, Intro, Strict, Symb, Term};
use std::collections::{HashMap, HashSet};

fn map_atoms<A, B, V>(t: Term<A, V>, f: &mut impl FnMut(A) -> Term<B, V>) -> Term<B, V> {
    use term::AppH;
    let args = t.args.into_iter().map(|a| map_atoms(a, f)).collect();
    let head = match t.head {
        AppH::Atom(a) => {
            let b = f(a);
            return Term {
                head: b.head,
                args: b.args.into_iter().chain(args).collect(),
            };
        }
        AppH::Abst(v, ty, tm) => AppH::Abst(
            v,
            ty.map(|t| Box::new(map_atoms(*t, f))),
            Box::new(map_atoms(*tm, f)),
        ),
        AppH::Prod(v, ty, tm) => {
            AppH::Prod(v, Box::new(map_atoms(*ty, f)), Box::new(map_atoms(*tm, f)))
        }
    };
    Term { head, args }
}

fn subst<A: Clone + Eq + Hash, V: Clone>(
    map: &mut HashMap<A, Term<A, V>>,
    t: Term<A, V>,
) -> Term<A, V> {
    map_atoms(t, &mut |a| match map.remove(&a) {
        None => Term {
            head: term::AppH::Atom(a),
            args: Vec::new(),
        },
        Some(t) => t,
    })
}

fn main() -> std::io::Result<()> {
    let mut map: HashMap<Symb<&str>, _> = HashMap::new();

    let args: Vec<String> = std::env::args().collect();
    let to_inline = std::fs::read_to_string(&args[2])?;
    let set: HashSet<&str> = to_inline.lines().collect();

    let file = std::fs::read_to_string(&args[1])?;

    let cmds = Strict::<_, _, &str>::new(&file);
    for cmd in cmds {
        let cmd = cmd.unwrap();
        //println!("{cmd:?}");

        match cmd {
            Command::Intro(name, args, Intro::Definition(_, Some(tm)) | Intro::Theorem(_, tm))
                if set.contains(&name) && args.is_empty() =>
            {
                let tm = subst(&mut map, tm);
                map.insert(Symb::new(name), tm);
            }
            Command::Intro(name, args, intro) => {
                let args = args
                    .into_iter()
                    .map(|(v, ty)| (v, subst(&mut map, ty)))
                    .collect();
                let intro = intro
                    .map_type(|t| subst(&mut map, t))
                    .map_term(|t| subst(&mut map, t));
                println!("{}.", Command::Intro(name, args, intro));
            }
            cmd @ Command::Rules(..) => println!("{cmd}."),
        }
    }

    Ok(())
}
