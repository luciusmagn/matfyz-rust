# I3B Builder + Error API

Course track: Introductory Rust (2026)
Homework pair: I3
Type: library
Submission filename: `solution.rs`

Goal:
Implement the required library API in `solution.rs` so that test drivers in `tests/test-*.rs` can import and exercise it.

Testing contract:

1. Each test driver is a Rust file named `test-XYZ.rs`.
2. Each test declares `mod solution;`.
3. Test stdout is compared against `test-XYZ.out.txt`.
4. Output comparison is lenient about trailing whitespace and final newline.

Notes:

1. Keep API behavior deterministic.
2. Avoid printing from library functions unless assignment explicitly requires it.
