# I3B Record Parsing Library

Course track: Introductory Rust (2026)  
Homework pair: I3  
Type: library  
Submission filename: `solution.rs`

## Task

Implement record parsing and aggregation helpers.

Required API:

```rust
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

pub fn parse_record(line: &str) -> Result<Record, RecordError>;
pub fn aggregate(records: &[Record]) -> BTreeMap<String, i64>;
pub fn render(map: &BTreeMap<String, i64>) -> String;
```
