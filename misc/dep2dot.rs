//! Given a Makefile as produced e.g. by `dkdep`,
//! create a theory dependency graph in DOT.
//!
//! Example usage:
//!     rustc dep2dot
//!     ./dep2dot < Makefile > graph.dot

use std::io::BufRead;

fn trim(filename: &str) -> &str {
    filename.trim_end_matches(".dko")
}

fn main() -> Result<(), std::io::Error> {
    println!("digraph G {{");
    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line?;
        let target_deps: Vec<_> = line.split(" : ").collect();
        match target_deps[..] {
            [target, deps] => {
                let target = trim(target);

                // get ".dko" dependencies
                for dep in deps.split_whitespace().skip(1).map(trim) {
                    println!("{} -> {};", target, dep)
                }
            }
            _ => (),
        }
    }
    println!("}}");
    Ok(())
}
