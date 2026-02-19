#[macro_export]
macro_rules! csv_line {
    () => {
        String::new()
    };
    ($first:expr $(, $rest:expr)* $(,)?) => {{
        let mut out = String::new();
        out.push_str(&format!("{}", $first));
        $(
            out.push(',');
            out.push_str(&format!("{}", $rest));
        )*
        out
    }};
}

#[macro_export]
macro_rules! repeat_vec {
    ($value:expr; $count:expr) => {{
        let mut out = Vec::new();
        let count = $count;
        for _ in 0..count {
            out.push($value.clone());
        }
        out
    }};
}

pub fn join_nonempty(parts: &[&str]) -> String {
    parts
        .iter()
        .copied()
        .filter(|p| !p.is_empty())
        .collect::<Vec<_>>()
        .join("|")
}
