mod solution;

fn main() {
    let picked = solution::pick_columns("n0;n1;n2;n3", ';', &[3, 0, 7, 2]);
    println!("{}", picked.join("|"));
}
