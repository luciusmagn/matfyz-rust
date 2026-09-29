use std::io::{self, Read};

fn split_fields<'a>(line: &'a str, sep: char) -> Vec<&'a str> {
    if line.is_empty() {
        Vec::new()
    } else {
        line.split(sep).collect()
    }
}

fn longest_field<'a>(line: &'a str, sep: char) -> Option<&'a str> {
    let mut best = None;
    let mut best_len = 0;

    for field in split_fields(line, sep) {
        if best.is_none() || field.len() > best_len {
            best = Some(field);
            best_len = field.len();
        }
    }

    best
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let mut lines = input.lines();

    let sep_line = match lines.next() {
        Some(v) => v,
        None => {
            println!("invalid input");
            return;
        }
    };
    let mut sep_chars = sep_line.chars();
    let sep = match (sep_chars.next(), sep_chars.next()) {
        (Some(c), None) => c,
        _ => {
            println!("invalid input");
            return;
        }
    };

    let n: usize = match lines.next().and_then(|s| s.trim().parse().ok()) {
        Some(v) => v,
        None => {
            println!("invalid input");
            return;
        }
    };

    let mut rows = Vec::with_capacity(n);
    for _ in 0..n {
        let Some(line) = lines.next() else {
            println!("invalid input");
            return;
        };
        rows.push(line.to_string());
    }

    let q: usize = match lines.next().and_then(|s| s.trim().parse().ok()) {
        Some(v) => v,
        None => {
            println!("invalid input");
            return;
        }
    };

    for _ in 0..q {
        let Some(query) = lines.next() else {
            println!("invalid input");
            return;
        };

        let parts: Vec<_> = query.split_whitespace().collect();
        match parts.as_slice() {
            ["count", row_raw] => {
                let Ok(row_idx) = row_raw.parse::<usize>() else {
                    println!("invalid input");
                    return;
                };

                match rows.get(row_idx) {
                    Some(row) => println!("count={}", split_fields(row, sep).len()),
                    None => println!("count=<missing>"),
                }
            }
            ["get", row_raw, col_raw] => {
                let Ok(row_idx) = row_raw.parse::<usize>() else {
                    println!("invalid input");
                    return;
                };
                let Ok(col_idx) = col_raw.parse::<usize>() else {
                    println!("invalid input");
                    return;
                };

                let field = rows
                    .get(row_idx)
                    .and_then(|row| split_fields(row, sep).get(col_idx).copied());

                match field {
                    Some(value) => println!("field={value}"),
                    None => println!("field=<missing>"),
                }
            }
            ["longest", row_raw] => {
                let Ok(row_idx) = row_raw.parse::<usize>() else {
                    println!("invalid input");
                    return;
                };

                let value = rows.get(row_idx).and_then(|row| longest_field(row, sep));

                match value {
                    Some(value) => println!("longest={value}"),
                    None => println!("longest=<missing>"),
                }
            }
            ["zip", left_raw, right_raw] => {
                let Ok(left_idx) = left_raw.parse::<usize>() else {
                    println!("invalid input");
                    return;
                };
                let Ok(right_idx) = right_raw.parse::<usize>() else {
                    println!("invalid input");
                    return;
                };

                let Some(left) = rows.get(left_idx) else {
                    println!("zip=<missing>");
                    continue;
                };
                let Some(right) = rows.get(right_idx) else {
                    println!("zip=<missing>");
                    continue;
                };

                let left = split_fields(left, sep);
                let right = split_fields(right, sep);
                let zipped = left
                    .into_iter()
                    .zip(right.into_iter())
                    .map(|(a, b)| format!("{a}:{b}"))
                    .collect::<Vec<_>>()
                    .join(",");
                println!("zip={zipped}");
            }
            _ => {
                println!("invalid input");
                return;
            }
        }
    }
}
