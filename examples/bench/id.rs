fn id(n: usize) {
    if n == 0 {
        print!("x")
    } else {
        print!("(id ");
        id(n - 1);
        print!(")");
    }
}

/// Define constants `idn` as `id (id (... (x) ...))`, having `n` occurrences of `id`.
fn main() {
    println!("A : Type.");
    println!("def id (x : A) := x.");
    for i in 1.. {
        print!("def id{} (x : A) := ", i);
        id(i);
        println!(".")
    }
}
