mod solution;

fn main() {
    let fields = solution::split_fields("a,,b,", ',');
    println!("{}", fields.len());
    println!("{:?}", solution::longest_field("x|yy|yy", '|'));
}
