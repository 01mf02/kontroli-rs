use criterion::{criterion_group, criterion_main, Criterion};
use kontroli::rc::{Intro, Signature, Typing};
use kontroli::{Command, Error, Scope, Share, Symbols};

fn check<'s>(cmds: Vec<kontroli::scope::Command<&'s str>>) -> Result<(), Error> {
    use colosseum::unsync::Arena;

    let arena = Arena::new();
    let mut syms = Symbols::new();
    let mut sig = Signature::new();

    for c in cmds.into_iter() {
        match c {
            // introduction of a new name
            Command::Intro(id, it) => {
                let it: Intro = it.share(&syms)?;

                let id: &str = arena.alloc(id);
                // add symbol to symbol table and fail if it is not new
                let sym = syms.insert(id)?;

                // typecheck and insert into signature
                let rewritable = it.rewritable();
                let typing: Typing = Typing::intro(it, &sig)?;
                typing.check(&sig)?;
                sig.insert(sym, typing, rewritable)?
            }
            // addition of rewrite rules
            Command::Rules(rules) => {
                for rule in rules {
                    sig.add_rule(rule.share(&syms)?)?
                }
            }
        }
    }

    Ok(())
}

fn parse(file: &str) -> Vec<kontroli::scope::Command<&str>> {
    use kontroli::parse::{lexes, Command, Parse};
    lexes(file)
        .map(|tokens| Command::parse_vec(tokens?))
        .map(|cmd| cmd.unwrap().scope())
        .collect()
}

macro_rules! include_ex {
    ($x:expr) => {
        include_str!(concat!("../../examples/", $x))
    };
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let fpure = include_ex!("pure.dk");
    let ppure = parse(fpure);

    let boole = parse(include_ex!("bool.dk"));
    let nat = parse(include_ex!("nat.dk"));
    let sudoku = parse(include_ex!("sudoku/sudoku.dk"));
    let sudoku_easy = parse(include_ex!("sudoku/solve_easy.dk"));
    let or_n = parse(include_ex!("bench/or_n.dk"));

    let cmd = "def eq : Dep (fib (mul 2 4)) := dep (fib (mul 4 2)).";
    let fib8 = [nat.clone(), parse(cmd)].concat();

    let cmd = "def eq : Bool_Dep (or_n (mul 2 4)) := bool_dep F.";
    let or8 = [boole.clone(), nat, or_n, parse(cmd)].concat();

    let sudo = [boole, sudoku, sudoku_easy].concat();

    c.bench_function("parse", |b| b.iter(|| parse(fpure)));
    c.bench_function("fib8", |b| b.iter(|| check(fib8.clone()).unwrap()));
    c.bench_function("or8", |b| b.iter(|| check(or8.clone()).unwrap()));
    c.bench_function("pure", |b| b.iter(|| check(ppure.clone()).unwrap()));
    c.bench_function("sudo", |b| b.iter(|| check(sudo.clone()).unwrap()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
