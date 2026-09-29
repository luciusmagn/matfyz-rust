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

#[macro_export]
macro_rules! join_nonempty {
    () => {
        String::new()
    };
    ($($value:expr),+ $(,)?) => {{
        let mut out = String::new();
        $(
            let piece = format!("{}", $value);
            if !piece.is_empty() {
                if !out.is_empty() {
                    out.push('|');
                }
                out.push_str(&piece);
            }
        )*
        out
    }};
}
