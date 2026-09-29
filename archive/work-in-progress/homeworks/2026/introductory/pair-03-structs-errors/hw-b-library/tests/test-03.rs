mod solution;

fn main() {
    let records = vec![
        solution::parse_record("x:1").unwrap(),
        solution::parse_record("y:2").unwrap(),
        solution::parse_record("x:3").unwrap(),
    ];
    let map = solution::aggregate(&records);
    println!("{}", solution::render(&map));
}
