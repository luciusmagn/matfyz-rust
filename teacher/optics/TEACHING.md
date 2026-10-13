# Teaching notes

## Starting point

The local introductory lecture includes `String`, `Vec`, moves, shared borrowing,
`Option`, `Result`, numeric parsing, and exhaustive `match`. It mentions `?` and
shows `split_whitespace`, `next`, and `unwrap_or`. The reference builds on those.

The main additions are **enums carrying data**, **structs**, and a handful of
standard-library operations. The implementation uses explicit loops and a
small number of functions. There are no external crates.

## Language additions

| Construct | Use in this program |
|---|---|
| `enum Element { Lens(f64, f64), Gap(f64), Stop(f64) }` | Attach different parameters to each kind of element. Introduce this after the lecture's fieldless `State` enum. |
| `struct Analysis { ... }` | Give names to the several results returned by a function. Construct one with `Analysis { ... }` and access fields with a dot. |
| `for`, early `return` | Traverse records and report invalid input at its source. |
| `&[Element]` | Borrow the contents of a vector without taking ownership. Passing `&elements` performs the vector-to-slice coercion. |
| `match *element` | Inspect a borrowed element. The patterns copy its `f64` fields; no owned text or vector is moved out. |
| A `match` expression | Compute the optional opening associated with an element. Each arm produces `Some(diameter)` or `None`. |
| `if let Some(value) = option` | Handle the present case when the absent case needs no action. Show the equivalent two-arm `match` first if useful. |
| Tuple patterns | Require both focal length and pupil diameter to be present before calculating f-number. |
| `?` | Return the error from a failed parse immediately, or use its successful value. All application errors here are owned `String`s. |
| Field shorthand | `Analysis { length, ... }` means `Analysis { length: length, ... }`. |

`Option<f64>` and `Option<usize>` can be copied, like their numeric contents.
This is why reading these fields through `&Analysis` works without cloning.

## Standard-library items

| Item | What students need to know |
|---|---|
| `std::env::args()` | Iterate over command-line arguments. The first is the program name. |
| `std::fs::read_to_string(path)` | Read a UTF-8 file into an owned `String`; return an I/O error on failure. |
| `std::process::exit(code)` | Finish the program with a success/failure status understood by the shell. |
| `str::lines()` | Iterate over borrowed lines; supports Unix and Windows line endings. |
| `str::split_whitespace()` | Iterate over borrowed words separated by whitespace. |
| `Iterator::collect()` | Put iterator results into a collection. `Vec<&str>` stores borrowed input words; `Vec<String>` owns command-line arguments. The type annotation selects the collection. |
| `Iterator::enumerate()` | Produce `(index, value)` pairs. Indices start at zero. |
| `slice::iter()` | Iterate over shared references to elements. |
| `Vec::new`, `push`, `len`, `is_empty` | Build a list and check its size before indexing. |
| `str::parse::<f64>()` | Convert a token to a number, returning `Result`. Extend the lecture's integer example. |
| `f64::abs`, `is_finite`, `INFINITY` | Handle sign and exceptional floating-point values. Infinity is the initial comparison bound for an unrestricted opening. |
| `Option::unwrap_or` | Use a supplied default for an absent value. |
| `format!`, `eprintln!` | Build an owned error message; print diagnostics on standard error. |
| `{value:.3}` | Print a floating-point value with three digits after the decimal point. |

There is no need to teach the iterator trait's definition to use `lines`,
`enumerate`, and `collect`. Start by showing the values each operation produces.

## Ownership in the parser

`read_to_string` creates the owned input. `parse_elements` borrows it as `&str`.
The line and word slices borrow from that same allocation. `words` owns only a
vector of references; it does not copy the words.

Parsing each word into `f64` produces an independent number. The returned
`Vec<Element>` contains numbers rather than references into the text, so it can
outlive the input string. `analyse` then borrows that vector and returns a record
of numeric results.

## Suggested implementation order

1. Represent one lens and one air gap; propagate the reference ray through a
   hardcoded vector. Check a single lens against its stated focal length.
2. Parse `lens` and `gap` records from a string. Validate the field count before
   indexing and return line-numbered errors.
3. Read the text from a file named on the command line.
4. Add `stop`, clear-aperture constraints, and entrance-pupil diameter.
5. Calculate f-number; handle diverging and afocal systems explicitly.
6. Compare the example prescriptions and add tests.

Provide the propagation equations and aperture formula. Students should spend
the session writing Rust rather than deriving pupil magnification. Reading
input and writing the parser are part of the exercise.

For a shorter first-lab target, stop after focal length and back focal distance;
use aperture handling as the extension. The numerical-overflow guards and the
relative afocal tolerance are useful reference details to explain afterwards.

## Tests

The test module is separate from the implementation. `#[cfg(test)] mod tests`
loads it for `cargo test`; `#[test]` marks a test function. `assert!` and
`assert_eq!` check conditions. `expect` and `unwrap` extract values whose presence
the test requires, reporting a failed test if the requirement is not met.

The `close` helper compares floating-point values within a tolerance. The
analytic two-lens formula provides an independent check of focal length and
back focus. `include_str!` reads an example file at compile time so tests can use
the same optical prescriptions as the command-line examples.

### Student tests

The starter in `../lens-calculator-student/` includes 16 function tests for
`calculate` and 34 executable-interface tests. Run `cargo test` for both suites,
`cargo test --bin lens-calculator` for the function tests, or
`cargo test --test lenses` for the CLI tests. See `tests/README.md` for the cases.

Use `optics-overview-student.pdf` as the handout. The final page of the teacher
edition contains a Tessar layout, the equivalent three-thin-lens model and the
worked results for `examples/tessar-model.opt`.
