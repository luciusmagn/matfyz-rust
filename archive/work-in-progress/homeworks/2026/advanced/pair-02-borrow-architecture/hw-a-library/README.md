# A2A Borrowed Row Views

Course track: Advanced Rust (2026)  
Homework pair: A2  
Type: library  
Submission filename: `solution.rs`

## Task

Implement the required API in `solution.rs`:

```rust
pub struct RowView<'a>;

impl<'a> RowView<'a> {
    pub fn new(line: &'a str, sep: char) -> Self;
    pub fn field_count(&self) -> usize;
    pub fn get(&self, idx: usize) -> Option<&'a str>;
    pub fn longest_field(&self) -> Option<&'a str>;
    pub fn iter(&self) -> impl Iterator<Item = &'a str> + '_;
}

pub fn zip_fields<'a, 'b>(left: &'a str, right: &'b str, sep: char) -> Vec<(&'a str, &'b str)>;
```

## Behavior contract

1. `RowView::new` stores borrowed field views split by `sep`.
2. For empty input line, `field_count` is `0`.
3. Consecutive separators create empty fields, and a trailing separator creates a trailing empty field.
4. `get` returns the field at `idx`, or `None` if out of range.
5. `longest_field` returns the longest field, and returns the first one on ties.
6. `iter` yields borrowed fields in original order.
7. `zip_fields` pairs fields from `left` and `right` up to the shorter field count.

## Requirements

1. Keep returned field slices borrowed from the original input lines.
2. Keep the signatures exactly as specified.
3. Do not print from library functions.
