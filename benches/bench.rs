use criterion::{criterion_group, criterion_main, Criterion};
use kontroli::pre::parse::{opt_lexeme, phrase, Parser};
use kontroli::rc::{Command, Signature, Symbols, Typing};
use kontroli::{pre, Error};
use std::io::Read;
use std::path::PathBuf;

fn check(cmds: Vec<pre::Command>) -> Result<(), Error> {
    let mut syms = Symbols::new();
    let mut sig = Signature::new();

    for c in cmds.into_iter() {
        let cmd: Command = Command::scope(c, &syms)?;
        match cmd {
            // introduction of a new name
            Command::Intro(id, it) => {
                // add symbol to symbol table and fail if it is not new
                let sym = syms.insert(id)?;

                // typecheck and insert into signature
                let typing: Typing = Typing::new(it, &sig)?.check(&sig)?;
                sig.insert(&sym, typing)?
            }
            // addition of a rewrite rule
            Command::Rule(rule) => sig.add_rule(rule)?,
        }
    }

    Ok(())
}

fn read(file: PathBuf) -> Vec<u8> {
    let mut file = std::fs::File::open(file).unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();
    buffer
}

fn parse(mut buffer: &[u8]) -> Vec<pre::Command> {
    let mut cmds = Vec::new();
    loop {
        match opt_lexeme(phrase(pre::Command::parse))(buffer) {
            Ok((i, x)) => {
                buffer = i;
                if let Some(cmd) = x {
                    cmds.push(cmd)
                }
            }
            Err(nom::Err::Incomplete(_)) if buffer.is_empty() => break cmds,
            _ => panic!("parsing failed"),
        }
    }
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let fpure = read(PathBuf::from("examples/pure.ko"));
    let ppure = parse(&fpure);

    let bol = parse(&read(PathBuf::from("examples/bool.ko")));
    let nat = parse(&read(PathBuf::from("examples/nat.ko")));
    let sudoku = parse(&read(PathBuf::from("examples/sudoku/sudoku.ko")));
    let sudoku_easy = parse(&read(PathBuf::from("examples/sudoku/solve_easy.ko")));

    let sudo = [bol, sudoku, sudoku_easy].concat();

    let cmd = b"def fail : Dep (fib 9) := dep (succ 9).";
    let fib9 = [nat, parse(cmd)].concat();

    c.bench_function("parse", |b| b.iter(|| parse(&fpure)));
    c.bench_function("fib9", |b| b.iter(|| assert!(check(fib9.clone()).is_err())));
    c.bench_function("pure", |b| b.iter(|| assert!(check(ppure.clone()).is_ok())));
    c.bench_function("sudo", |b| b.iter(|| assert!(check(sudo.clone()).is_ok())));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
