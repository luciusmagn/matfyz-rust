# A1B Deterministic Parallel Job Processor

Course track: Advanced Rust (2026)  
Homework pair: A1  
Type: executable  
Submission filename: `main.rs`

## Task

Write a program that processes independent jobs and prints deterministic results.

## Input format

1. Line 1: `workers` (positive integer).
2. Line 2: `job_count` (non-negative integer).
3. Next `job_count` lines:
- `job_id value_count v1 v2 ... vN`
- `value_count` is the number of following integers on that line.

All integers fit in `i64`.

## Computation

For each job:

1. `sum = v1 + v2 + ... + vN`
2. `checksum = 1*v1 + 2*v2 + ... + N*vN`

Then sort jobs by `job_id` ascending and print one line per job:

```text
job <job_id>: sum=<sum> checksum=<checksum>
```

Finally print:

```text
total_sum=<sum of all job sums>
total_checksum=<sum of all job checksums>
```

## Concurrency expectation

Use `workers` as the desired parallelism level when processing jobs, but final observable output must stay deterministic.

## Testing contract

1. Tests are `*.in.txt` / matching `*.out.txt` files.
2. Program stdout is compared against expected output.
3. Output comparison is lenient about trailing whitespace and missing final newline.

## Requirements

1. Parse input exactly as described.
2. Keep output formatting exact.
3. Do not print debug output.
