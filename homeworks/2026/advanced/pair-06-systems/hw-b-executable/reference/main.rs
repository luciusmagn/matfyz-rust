use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();

    let n: usize = match lines.next().and_then(|x| x.trim().parse().ok()) {
        Some(v) => v,
        None => return,
    };

    let (mut info, mut warn, mut error, mut other) = (0usize, 0usize, 0usize, 0usize);

    for _ in 0..n {
        let Some(line) = lines.next() else {
            other += 1;
            continue;
        };
        let level = line.split_whitespace().next();
        match level {
            Some("INFO") => info += 1,
            Some("WARN") => warn += 1,
            Some("ERROR") => error += 1,
            _ => other += 1,
        }
    }

    println!("INFO={info}");
    println!("WARN={warn}");
    println!("ERROR={error}");
    println!("OTHER={other}");
    println!("status={}", if error > 0 { "degraded" } else { "ok" });
}
