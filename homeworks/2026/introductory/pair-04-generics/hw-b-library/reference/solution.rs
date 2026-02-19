use std::collections::BTreeMap;

pub fn top_n<T: Ord + Clone>(values: &[T], n: usize) -> Vec<T> {
    let mut v = values.to_vec();
    v.sort_by(|a, b| b.cmp(a));
    v.truncate(n);
    v
}

pub fn frequencies<T: Ord + Clone>(values: &[T]) -> Vec<(T, usize)> {
    let mut map = BTreeMap::<T, usize>::new();
    for value in values {
        *map.entry(value.clone()).or_insert(0) += 1;
    }
    map.into_iter().collect()
}

pub fn interleave<T: Clone>(a: &[T], b: &[T]) -> Vec<T> {
    let mut out = Vec::with_capacity(a.len() + b.len());
    let mut i = 0usize;

    while i < a.len() || i < b.len() {
        if i < a.len() {
            out.push(a[i].clone());
        }
        if i < b.len() {
            out.push(b[i].clone());
        }
        i += 1;
    }

    out
}
