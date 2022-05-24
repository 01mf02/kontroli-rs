fn fs(f: &str, n: usize) {
    if n == 0 {
        print!("{} x y", f);
    } else {
        print!("{} (", f);
        fs("fst", n - 1);
        print!(") (");
        fs("snd", n - 1);
        print!(")")
    }
}

/// Define constants `fsn` containing `2^(n-1)` occurrences of `fst x y` and `snd x y`.
fn main() {
    println!("A : Type.");
    println!("def fst (x : A) (y : A) := x.");
    println!("def snd (x : A) (y : A) := y.");
    for i in 1.. {
        print!("def fs{} (x : A) (y : A) := ", i);
        fs("fst", i);
        println!(".")
    }
}
