/// Print constants `an` whose type contains `2^n` occurrences of `f 0 0`.
fn main() {
    println!("a : Type.");
    println!("0 : a.");
    println!("p : a -> Type.");
    println!("f : a -> a -> a.");

    for i in 0.. {
        print!("a{} : p", i);
        btree_app(i + 1);
        println!(".");
    }
}

fn btree_app(n: usize) {
    if n == 0 {
        print!(" 0")
    } else {
        print!(" (f");
        btree_app(n - 1);
        btree_app(n - 1);
        print!(")");
    }
}
