mod solution;

fn main() {
    let out = solution::run_program(&["add 4", "print", "sub 1", "print"]);
    println!("{:?}", out);
}
