use std::io::{self, Read};
use std::thread;

fn make_chunks(len: usize, workers: usize) -> Vec<(usize, usize)> {
    let effective_workers = workers.max(1).min(len.max(1));
    (0..effective_workers)
        .map(|i| {
            let start = i * len / effective_workers;
            let end = (i + 1) * len / effective_workers;
            (start, end)
        })
        .collect()
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();

    let workers: usize = match lines.next().and_then(|x| x.trim().parse().ok()) {
        Some(v) if v > 0 => v,
        _ => {
            println!("invalid input");
            return;
        }
    };

    let n: usize = match lines.next().and_then(|x| x.trim().parse().ok()) {
        Some(v) => v,
        None => {
            println!("invalid input");
            return;
        }
    };

    let values_line = lines.next().unwrap_or("");
    let parts: Vec<_> = if values_line.trim().is_empty() {
        Vec::new()
    } else {
        values_line.split_whitespace().collect()
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

    let chunks = make_chunks(values.len(), workers);
    let mut handles = Vec::new();

    for (idx, (start, end)) in chunks.iter().copied().enumerate() {
        let chunk = values[start..end].to_vec();
        handles.push(thread::spawn(move || (idx, chunk.iter().sum::<i64>())));
    }

    let mut sums = vec![0_i64; chunks.len()];
    for h in handles {
        let (idx, sum) = h.join().unwrap();
        sums[idx] = sum;
    }

    for (idx, sum) in sums.iter().enumerate() {
        println!("chunk {idx}={sum}");
    }

    let total: i64 = sums.iter().sum();
    println!("total={total}");
}
