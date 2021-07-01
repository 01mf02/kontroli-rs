use criterion::{criterion_group, criterion_main, Criterion};
use kontroli::rc::{Signature, Typing};
use kontroli::{Command, Error, Share, Symbols};
use std::include_bytes;

fn check(cmds: Vec<kontroli::parse::Command>) -> Result<(), Error> {
    use colosseum::unsync::Arena;

    let arena = Arena::new();
    let mut syms = Symbols::new();
    let mut sig = Signature::new();

    for c in cmds.into_iter() {
        use kontroli::Scope;
        match c.scope() {
            // introduction of a new name
            Command::Intro(id, it) => {
                let it = it.share(&syms)?;

                let id: &str = arena.alloc(id);
                // add symbol to symbol table and fail if it is not new
                let sym = syms.insert(id)?;

                // typecheck and insert into signature
                let typing: Typing = Typing::new(it, &sig)?.check(&sig)?;
                sig.insert(sym, typing)?
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

fn parse(buffer: &[u8]) -> Vec<kontroli::parse::Command> {
    use kontroli::parse::{opt_lex, phrase, Parser};
    use nom::combinator::iterator;
    let parse = opt_lex(phrase(kontroli::parse::Command::parse));
    iterator(buffer, parse).filter_map(|c| c).collect()
}

macro_rules! include_ex {
    ($x:expr) => {
        include_bytes!(concat!("../../examples/", $x))
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

    let cmd = b"def eq : Dep (fib (mul 2 4)) := dep (fib (mul 4 2)).\n";
    let fib8 = [nat.clone(), parse(cmd)].concat();

    let cmd = b"def eq : Bool_Dep (or_n (mul 2 4)) := bool_dep F.\n";
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
