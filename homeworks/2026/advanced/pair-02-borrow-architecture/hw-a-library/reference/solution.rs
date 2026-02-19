pub fn split_fields<'a>(line: &'a str, sep: char) -> Vec<&'a str> {
    if line.is_empty() {
        Vec::new()
    } else {
        line.split(sep).collect()
    }
}

pub fn longest_field<'a>(line: &'a str, sep: char) -> Option<&'a str> {
    split_fields(line, sep)
        .into_iter()
        .max_by_key(|field| field.len())
}

pub fn pick_columns<'a>(line: &'a str, sep: char, idxs: &[usize]) -> Vec<&'a str> {
    let fields = split_fields(line, sep);
    idxs.iter()
        .filter_map(|idx| fields.get(*idx).copied())
        .collect()
}
