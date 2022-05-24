/// Create constants `an` of type `a -> a -> a`, having `n` occurrences of `a`.
fn main() {
    println!("a : Type.");
    for i in 1.. {
        print!("a{} : a", i);
        for j in 1..i {
            print!(" -> a");
        }
        println!(".");
    }
}
