# Rust Toolchain Policy (NPRG082 + NPRG074)

This policy applies to:

1. NPRG082 (Programming in Rust, basic/winter)
2. NPRG074 (Advanced Programming in Rust, spring)

## Required Rust Version

Use exactly stable Rust `1.88`.

- `rustc --version` must report `1.88`
- Nightly-only features are not part of graded coursework unless explicitly stated in a specific assignment.

Rationale:

1. ReCodEx and local student setup must be consistent.
2. We want deterministic grading and fewer environment-related failures.

## ReCodEx Compatibility

All graded assignments are evaluated in ReCodEx against the course Rust environment.
Students should always test locally with the same compiler version before submission.

## Homework Submission File Names

Each assignment must explicitly state expected submission filename.

1. Executable homeworks:
   - Student submits `main.rs` (plus additional files if assignment allows it).
2. Library homeworks:
   - Student submits `solution.rs`.
   - Test files use `mod solution;` and import from student code.

## Output Matching Rules (Current)

Both homework types use the same lenient output matching.

- Trailing whitespace is ignored.
- Missing final newline is ignored.
- Empty-line handling may be lenient as well (assignment text should avoid ambiguity where possible).

## Recommended Student Setup

```bash
rustup toolchain install 1.88
rustup default 1.88
rustc --version
cargo --version
```

If needed per-directory:

```bash
rustup override set 1.88
```

## Notes for Assignment Authors

1. Keep assignment statements explicit about expected filename (`main.rs` vs `solution.rs`).
2. Provide at least 4 tests per assignment.
3. For library tasks, include both behavior coverage and API-shape checks in `test-XYZ.rs`.
