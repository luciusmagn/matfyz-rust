use std::sync::mpsc;
use std::thread;

pub fn chunk_ranges(len: usize, workers: usize) -> Vec<(usize, usize)> {
    let effective_workers = workers.max(1).min(len.max(1));
    (0..effective_workers)
        .map(|i| {
            let start = i * len / effective_workers;
            let end = (i + 1) * len / effective_workers;
            (start, end)
        })
        .collect()
}

pub fn dispatch_jobs(values: Vec<i64>, workers: usize) -> Vec<(usize, i64)> {
    let chunks = chunk_ranges(values.len(), workers);
    let (tx, rx) = mpsc::channel();

    for (idx, (start, end)) in chunks.iter().copied().enumerate() {
        let tx = tx.clone();
        let chunk = values[start..end].to_vec();
        thread::spawn(move || {
            let sum = chunk.iter().sum::<i64>();
            tx.send((idx, sum)).unwrap();
        });
    }
    drop(tx);

    let mut out: Vec<(usize, i64)> = rx.into_iter().collect();
    out.sort_by_key(|(idx, _)| *idx);
    out
}

pub fn parallel_square_sum(values: Vec<i64>, workers: usize) -> i64 {
    let chunks = chunk_ranges(values.len(), workers);
    let mut handles = Vec::new();

    for (start, end) in chunks {
        let chunk = values[start..end].to_vec();
        handles.push(thread::spawn(move || {
            chunk.into_iter().map(|v| v * v).sum::<i64>()
        }));
    }

    handles.into_iter().map(|h| h.join().unwrap()).sum()
}
