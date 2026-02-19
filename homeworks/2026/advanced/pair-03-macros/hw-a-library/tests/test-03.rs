#[macro_use]
mod solution;

fn main() {
    let row = csv_line!("log", 42, "ok");
    let joined = solution::join_nonempty(&[&row, "", "done"]);
    println!("{joined}");
}
