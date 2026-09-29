# A2B Delimited Row Query Engine

Course track: Advanced Rust (2026)  
Homework pair: A2  
Type: executable  
Submission filename: `main.rs`

## Task

Write a program that answers queries over borrowed views of delimited rows.

## Input format

1. Line 1: separator character `sep`.
2. Line 2: `n` (number of rows).
3. Next `n` lines: raw rows.
4. Next line: `q` (number of queries).
5. Next `q` lines: one query per line.

Queries:

- `count r`
- `get r c`
- `longest r`
- `zip a b`

All row and column indexes are zero-based.

## Output format

For each query:

- `count r`:
  - valid row: `count=<field_count>`
  - invalid row: `count=<missing>`
- `get r c`:
  - valid row and field: `field=<value>`
  - otherwise: `field=<missing>`
- `longest r`:
  - valid row with at least one field: `longest=<value>`
  - otherwise: `longest=<missing>`
- `zip a b`:
  - print `zip=<x1:y1,x2:y2,...>` up to the shorter row length
  - invalid row: `zip=<missing>`

Malformed input or malformed query syntax prints exactly `invalid input` and terminates.

## Requirements

1. Keep output order exactly as query order.
2. Split fields using the separator from line 1.
3. If several fields have the same maximal length, use the first such field.
4. Do not print debug output.

## Implementation guidance

This homework belongs to the borrow-checker block, so the intended solution should work mainly with borrowed `&str` slices derived from the stored input rows.

In particular:

1. Store each input row once.
2. Derive field views from that stored row data.
3. Avoid cloning whole rows or fields just to answer `count`, `get`, `longest`, or `zip`.
4. Temporary allocation for final output formatting is fine.

Re-splitting a row when answering a query is acceptable. The point is to practice lifetime-aware API and data-flow design, not to manufacture owned copies everywhere.
