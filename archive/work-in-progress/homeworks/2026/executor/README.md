# Local Homework Runner (Draft)

This runner mimics the two ReCodEx Rust homework formats used in this project.

## 1) Executable homework mode

- Student submission default: `main.rs`
- Tests use input/output text files:
  - `*.in.txt`
  - matching `*.out.txt`

Run:

```bash
python3 homeworks/2026/executor/run_homework.py executable \
  --student /path/to/main.rs \
  --tests /path/to/tests
```

## 2) Library homework mode

- Student submission default: `solution.rs`
- Tests are Rust sources named `test-*.rs`
- Each test should contain `mod solution;`
- Each test has matching expected output `test-*.out.txt`

Run:

```bash
python3 homeworks/2026/executor/run_homework.py library \
  --student /path/to/solution.rs \
  --tests /path/to/tests
```

## Options

- `--cargo-template`: path to Cargo.toml template
  - default: `recodex-envs-and-utils/rust-cargo-builder/Cargo.toml`
- `--timeout-seconds N`: timeout per cargo command (default `8.0`)
- `--release`: run build/tests with `cargo --release`
- `--extra FILE`: add extra student `.rs` files (repeatable)
- `--ignore-empty-lines`: optionally ignore empty lines in output comparison

## Lenient output matching

Always applied:

1. Trailing whitespace per line is ignored.
2. Missing final newline is ignored.
3. Trailing blank lines at end are ignored.

Optional:

- `--ignore-empty-lines` removes all blank lines before comparison.

## Project template parity

The runner copies `Cargo.toml` and, when available, also reuses adjacent `.cargo/`, `Cargo.lock`, and `vendor/` from the template directory.
