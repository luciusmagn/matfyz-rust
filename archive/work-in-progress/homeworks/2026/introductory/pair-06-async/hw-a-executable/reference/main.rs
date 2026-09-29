use std::io::{self, Read};
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
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

    let mut values = Vec::with_capacity(n);
    for p in parts {
        match p.parse::<i64>() {
            Ok(v) => values.push(v),
            Err(_) => {
                println!("invalid input");
                return;
            }
        }
    }

    let mut handles = Vec::new();
    for (idx, value) in values.into_iter().enumerate() {
        handles.push(tokio::spawn(async move {
            sleep(Duration::from_millis((idx % 3) as u64)).await;
            (idx, value * 2)
        }));
    }

    let mut out = vec![0_i64; handles.len()];
    for h in handles {
        let (idx, doubled) = h.await.expect("task panicked");
        out[idx] = doubled;
    }

    let mut sum = 0_i64;
    for (idx, value) in out.iter().copied().enumerate() {
        println!("task {idx}={value}");
        sum += value;
    }
    println!("sum={sum}");
}
