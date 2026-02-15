use std::thread;

pub fn make_chunks(len: usize, workers: usize) -> Vec<(usize, usize)> {
    let effective_workers = workers.max(1).min(len.max(1));
    let mut chunks = Vec::with_capacity(effective_workers);

    for i in 0..effective_workers {
        let start = i * len / effective_workers;
        let end = (i + 1) * len / effective_workers;
        chunks.push((start, end));
    }

    chunks
}

pub fn parallel_chunk_sums(values: Vec<i64>, workers: usize) -> Vec<i64> {
    if values.is_empty() {
        return vec![0];
    }

    let chunks = make_chunks(values.len(), workers);
    let mut handles = Vec::with_capacity(chunks.len());

    for (start, end) in chunks {
        let chunk = values[start..end].to_vec();
        handles.push(thread::spawn(move || chunk.iter().sum::<i64>()));
    }

    handles
        .into_iter()
        .map(|handle| handle.join().expect("worker panicked"))
        .collect()
}

pub fn parallel_weighted_checksum(values: Vec<i64>, workers: usize) -> i64 {
    if values.is_empty() {
        return 0;
    }

    let chunks = make_chunks(values.len(), workers);
    let mut handles = Vec::with_capacity(chunks.len());

    for (start, end) in chunks {
        let chunk = values[start..end].to_vec();
        handles.push(thread::spawn(move || {
            chunk
                .iter()
                .enumerate()
                .map(|(i, value)| (start as i64 + i as i64 + 1) * value)
                .sum::<i64>()
        }));
    }

    handles
        .into_iter()
        .map(|handle| handle.join().expect("worker panicked"))
        .sum()
}
