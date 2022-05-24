/// Define constants `an` as `x : a => x : a => ... => x`, having `n` abstractions.
fn main() {
    println!("a : Type.");
    for i in 1.. {
        print!("def a{} := ", i);
        for _ in 0..i {
            print!("x : a => ");
        }
        println!("x.");
    }
}
