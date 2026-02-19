mod solution;

fn main() {
    println!("{:?}", solution::longest_field("ab|cdef|xy", '|'));
    println!("{:?}", solution::longest_field("", '|'));
}
