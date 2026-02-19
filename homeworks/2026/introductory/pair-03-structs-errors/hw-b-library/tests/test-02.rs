mod solution;

fn main() {
    println!("{:?}", solution::parse_record("bad"));
    println!("{:?}", solution::parse_record("a:xx"));
}
