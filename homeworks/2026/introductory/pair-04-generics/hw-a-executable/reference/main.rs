use std::collections::BTreeSet;
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

    let nums_line = lines.next().unwrap_or("");
    let parts: Vec<_> = if nums_line.trim().is_empty() {
        Vec::new()
    } else {
        nums_line.split_whitespace().collect()
    };

    if parts.len() != n {
        println!("invalid input");
        return;
    }

    let mut values = Vec::<i64>::new();
    for p in parts {
        match p.parse::<i64>() {
            Ok(v) => values.push(v),
            Err(_) => {
                println!("invalid input");
                return;
            }
        }
    }

    let unique: Vec<_> = BTreeSet::<i64>::from_iter(values.iter().copied())
        .into_iter()
        .map(|x| x.to_string())
        .collect();
    println!("unique={}", unique.join(","));

    let sum: i64 = values.iter().sum();
    println!("sum={sum}");

    match values.iter().min() {
        Some(v) => println!("min={v}"),
        None => println!("min=none"),
    }
    match values.iter().max() {
        Some(v) => println!("max={v}"),
        None => println!("max=none"),
    }
}
