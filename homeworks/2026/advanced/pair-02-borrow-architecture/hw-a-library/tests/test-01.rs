mod solution;

fn main() {
    let fields = solution::split_fields("alpha|beta|gamma", '|');
    println!("{}", fields.join(","));
}
