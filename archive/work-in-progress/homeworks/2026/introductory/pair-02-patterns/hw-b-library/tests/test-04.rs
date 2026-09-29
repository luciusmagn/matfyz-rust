mod solution;

fn main() {
    let out = solution::run_program(&["oops", "reset", "print", "add 7", "print"]);
    println!("{:?}", out);
}
