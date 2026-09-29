mod solution;

fn main() {
    let mut s = 2;
    println!("{:?}", solution::apply_op(&mut s, &solution::Op::Mul(5)));
    println!("{:?}", solution::apply_op(&mut s, &solution::Op::Print));
}
