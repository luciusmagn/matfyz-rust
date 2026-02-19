use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();

    let n: usize = match lines.next().and_then(|x| x.trim().parse().ok()) {
        Some(v) => v,
        None => return,
    };

    let mut state = 0_i64;

    for _ in 0..n {
        let Some(line) = lines.next() else {
            println!("err");
            continue;
        };
        let parts: Vec<_> = line.split_whitespace().collect();
        if parts.is_empty() {
            println!("err");
            continue;
        }

        match parts[0] {
            "add" | "sub" | "mul" => {
                if parts.len() != 2 {
                    println!("err");
                    continue;
                }
                let x: i64 = match parts[1].parse() {
                    Ok(v) => v,
                    Err(_) => {
                        println!("err");
                        continue;
                    }
                };
                match parts[0] {
                    "add" => state += x,
                    "sub" => state -= x,
                    "mul" => state *= x,
                    _ => unreachable!(),
                }
            }
            "reset" => {
                if parts.len() != 1 {
                    println!("err");
                    continue;
                }
                state = 0;
            }
            "print" => {
                if parts.len() != 1 {
                    println!("err");
                    continue;
                }
                println!("{state}");
            }
            _ => println!("err"),
        }
    }

    println!("final={state}");
}
