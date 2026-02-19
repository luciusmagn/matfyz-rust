use std::io::{self, Read};

fn eval(op: &str, a: i64, b: i64) -> Option<i64> {
    match op {
        "add" => Some(a + b),
        "sub" => Some(a - b),
        "mul" => Some(a * b),
        "div" if b != 0 => Some(a / b),
        _ => None,
    }
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();

    let Some(first) = lines.next() else {
        return;
    };
    let n: usize = match first.trim().parse() {
        Ok(v) => v,
        Err(_) => {
            println!("ok=0 err=0");
            return;
        }
    };

    let mut ok = 0usize;
    let mut err = 0usize;

    for _ in 0..n {
        let Some(line) = lines.next() else {
            println!("err");
            err += 1;
            continue;
        };
        let parts: Vec<_> = line.split_whitespace().collect();
        if parts.len() != 3 {
            println!("err");
            err += 1;
            continue;
        }

        let a = match parts[1].parse::<i64>() {
            Ok(v) => v,
            Err(_) => {
                println!("err");
                err += 1;
                continue;
            }
        };
        let b = match parts[2].parse::<i64>() {
            Ok(v) => v,
            Err(_) => {
                println!("err");
                err += 1;
                continue;
            }
        };

        match eval(parts[0], a, b) {
            Some(v) => {
                println!("{v}");
                ok += 1;
            }
            None => {
                println!("err");
                err += 1;
            }
        }
    }

    println!("ok={ok} err={err}");
}
