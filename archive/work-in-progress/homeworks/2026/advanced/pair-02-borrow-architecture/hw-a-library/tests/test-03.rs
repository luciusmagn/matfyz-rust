mod solution;

fn main() {
    let left = String::from("n0;n1;n2;n3");
    let right = String::from("x0;x1;x2");
    let pairs = solution::zip_fields(&left, &right, ';');

    for (a, b) in pairs {
        println!("{a}|{b}");
    }
}
