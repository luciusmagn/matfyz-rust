use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
    pub name: String,
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordError {
    MissingColon,
    EmptyName,
    InvalidValue,
}

pub fn parse_record(line: &str) -> Result<Record, RecordError> {
    let Some((name, value_raw)) = line.split_once(':') else {
        return Err(RecordError::MissingColon);
    };
    if name.is_empty() || value_raw.contains(':') {
        return Err(RecordError::EmptyName);
    }
    let value = value_raw.parse::<i64>().map_err(|_| RecordError::InvalidValue)?;
    Ok(Record {
        name: name.to_string(),
        value,
    })
}

pub fn aggregate(records: &[Record]) -> BTreeMap<String, i64> {
    let mut map = BTreeMap::new();
    for rec in records {
        *map.entry(rec.name.clone()).or_insert(0) += rec.value;
    }
    map
}

pub fn render(map: &BTreeMap<String, i64>) -> String {
    map.iter()
        .map(|(k, v)| format!("{k}={v}"))
        .collect::<Vec<_>>()
        .join("\n")
}
