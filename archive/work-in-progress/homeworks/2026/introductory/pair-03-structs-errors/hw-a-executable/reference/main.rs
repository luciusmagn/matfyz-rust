use std::collections::BTreeMap;
use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();

    let n: usize = match lines.next().and_then(|x| x.trim().parse().ok()) {
        Some(v) => v,
        None => {
            println!("error");
            return;
        }
    };

    let mut map = BTreeMap::<String, i64>::new();

    for _ in 0..n {
        let Some(line) = lines.next() else {
            println!("error");
            return;
        };
        let Some((name, value_raw)) = line.split_once(':') else {
            println!("error");
            return;
        };
        if name.is_empty() || value_raw.contains(':') {
            println!("error");
            return;
        }
        let value: i64 = match value_raw.parse() {
            Ok(v) => v,
            Err(_) => {
                println!("error");
                return;
            }
        };
        *map.entry(name.to_string()).or_insert(0) += value;
    }

    for (name, value) in map {
        println!("{name}={value}");
    }
}
