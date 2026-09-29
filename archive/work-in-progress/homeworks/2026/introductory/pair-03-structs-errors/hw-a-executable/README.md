# I3A Record Aggregator

Course track: Introductory Rust (2026)  
Homework pair: I3  
Type: executable  
Submission filename: `main.rs`

## Task

Read `name:value` records and aggregate values by name.

## Input format

1. Line 1: `n` (number of records)
2. Next `n` lines: `name:value` where `value` is `i64`

## Output format

For valid input:

1. Print aggregated sums sorted by `name`:
`name=sum`
2. One output line per distinct name.

For invalid record (missing `:` / empty name / invalid value), print `error` and terminate.
