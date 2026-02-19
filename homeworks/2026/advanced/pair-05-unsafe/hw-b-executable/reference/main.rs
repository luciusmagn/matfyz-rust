use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();

    let capacity: usize = match lines.next().and_then(|x| x.trim().parse().ok()) {
        Some(v) => v,
        None => return,
    };
    let n: usize = match lines.next().and_then(|x| x.trim().parse().ok()) {
        Some(v) => v,
        None => return,
    };

    let mut stack = Vec::<i64>::new();

    for _ in 0..n {
        let Some(line) = lines.next() else {
            break;
        };
        let parts: Vec<_> = line.split_whitespace().collect();
        if parts.is_empty() {
            continue;
        }

        match parts[0] {
            "push" => {
                if parts.len() != 2 {
                    println!("empty");
                    continue;
                }
                let value: i64 = match parts[1].parse() {
                    Ok(v) => v,
                    Err(_) => {
                        println!("empty");
                        continue;
                    }
                };
                if stack.len() >= capacity {
                    println!("full");
                } else {
                    stack.push(value);
                    println!("ok");
                }
            }
            "pop" => match stack.pop() {
                Some(v) => println!("{v}"),
                None => println!("empty"),
            },
            "peek" => match stack.last() {
                Some(v) => println!("{v}"),
                None => println!("empty"),
            },
            _ => println!("empty"),
        }
    }

    println!("size={}", stack.len());
}
