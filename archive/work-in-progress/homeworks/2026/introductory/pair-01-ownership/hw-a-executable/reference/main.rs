use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();

    let n: usize = match lines.next().and_then(|x| x.trim().parse().ok()) {
        Some(v) => v,
        None => {
            println!("invalid input");
            return;
        }
    };

    let mut total = 0usize;

    for i in 0..n {
        let Some(word) = lines.next() else {
            println!("invalid input");
            return;
        };
        let rev: String = word.chars().rev().collect();
        total += word.chars().count();
        println!("{i}:{}:{rev}", word.chars().count());
    }

    println!("total_chars={total}");
}
