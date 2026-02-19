use std::collections::HashMap;
use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();

    let n: usize = match lines.next().and_then(|s| s.trim().parse().ok()) {
        Some(v) => v,
        None => {
            println!("invalid input");
            return;
        }
    };

    let mut table = HashMap::<String, String>::new();
    for _ in 0..n {
        let Some(line) = lines.next() else {
            println!("invalid input");
            return;
        };
        let Some((k, v)) = line.split_once(':') else {
            println!("invalid input");
            return;
        };
        if k.is_empty() || v.is_empty() {
            println!("invalid input");
            return;
        }
        table.insert(k.to_string(), v.to_string());
    }

    let q: usize = match lines.next().and_then(|s| s.trim().parse().ok()) {
        Some(v) => v,
        None => {
            println!("invalid input");
            return;
        }
    };

    let mut found = 0usize;
    let mut missing = 0usize;

    for _ in 0..q {
        let Some(key) = lines.next() else {
            println!("invalid input");
            return;
        };
        if let Some(value) = table.get(key) {
            println!("{key}={value}");
            found += 1;
        } else {
            println!("{key}=<missing>");
            missing += 1;
        }
    }

    println!("found={found} missing={missing}");
}
