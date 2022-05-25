use criterion::{criterion_group, criterion_main, Criterion};
use kontroli::kernel::{self, GCtx};
use kontroli::{Command, Error, Share, Symbols};

type Commands<'s> = Vec<kontroli::parse::Item<&'s str>>;

fn check<'s>(cmds: Commands<'s>) -> Result<(), Error> {
    use colosseum::unsync::Arena;

    let arena = Arena::new();
    let mut syms = Symbols::new();
    let mut gc = GCtx::new();

    for c in cmds.into_iter() {
        match c.share(&syms)? {
            // introduction of a new name
            Command::Intro(id, it) => {
                let owned = kontroli::symbol::Owned::new(id.clone());
                // add symbol to symbol table and fail if it is not new
                let sym = syms.insert(id, arena.alloc(owned))?;

                // typecheck and insert into global context
                let rewritable = it.rewritable();
                let (typing, check) = kernel::intro(it, &gc)?;
                if let Some(check) = check {
                    check.check(&gc)?
                }
                gc.insert(sym, typing, rewritable)?
            }
            // addition of rewrite rules
            Command::Rules(rules) => rules.into_iter().try_for_each(|r| gc.add_rule(r))?,
        }
    }

    Ok(())
}

fn parse(file: &str) -> Commands {
    kontroli::parse::Strict::new(file)
        .map(|cmd| cmd.unwrap())
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
