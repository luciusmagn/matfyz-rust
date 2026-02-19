use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConfigError {
    MissingEquals(usize),
    EmptyKey(usize),
    InvalidValue(usize),
    DuplicateKey(String),
}

pub fn parse_config(input: &str) -> Result<BTreeMap<String, i64>, ConfigError> {
    let mut out = BTreeMap::new();

    for (line_no, line) in input.lines().enumerate() {
        let line_no = line_no + 1;
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let Some((key_raw, value_raw)) = trimmed.split_once('=') else {
            return Err(ConfigError::MissingEquals(line_no));
        };

        if value_raw.contains('=') {
            return Err(ConfigError::MissingEquals(line_no));
        }

        let key = key_raw.trim();
        if key.is_empty() {
            return Err(ConfigError::EmptyKey(line_no));
        }

        let value: i64 = match value_raw.trim().parse() {
            Ok(v) => v,
            Err(_) => return Err(ConfigError::InvalidValue(line_no)),
        };

        if out.contains_key(key) {
            return Err(ConfigError::DuplicateKey(key.to_string()));
        }

        out.insert(key.to_string(), value);
    }

    Ok(out)
}

pub fn render_config(map: &BTreeMap<String, i64>) -> String {
    map.iter()
        .map(|(k, v)| format!("{k}={v}"))
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn sum_values(map: &BTreeMap<String, i64>) -> i64 {
    map.values().copied().sum()
}
