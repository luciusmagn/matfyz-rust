mod solution;

fn main() {
    println!("{}", solution::join_path("/srv/app", "logs/today.txt"));
    println!("{}", solution::join_path("/srv/app", "/opt/data"));
}
