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
        let Some((k_raw, v_raw)) = line.split_once('=') else {
            println!("error");
            return;
        };
        if v_raw.contains('=') {
            println!("error");
            return;
        }

        let key = k_raw.trim();
        if key.is_empty() {
            println!("error");
            return;
        }

        let value: i64 = match v_raw.trim().parse() {
            Ok(v) => v,
            Err(_) => {
                println!("error");
                return;
            }
        };

        map.insert(key.to_string(), value);
    }

    let mut total = 0_i64;
    for (k, v) in &map {
        println!("{k}={v}");
        total += v;
    }
    println!("total={total}");
}
