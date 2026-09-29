#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 4],
  config-info(
    title: [Advanced Rust (2026): Libraries and Testing],
    subtitle: [Lecture 4],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= General Principles

== Writing Libraries
- A library is judged mainly by its interface and long-term behavior #pause
- Internals matter, but callers mostly see consistency, reliability, and documentation #pause
- Bad API decisions survive much longer than bad internal code

== Consistency
- Similar things should be done the same way #pause
- Naming, ownership style, error handling, and mutability conventions should line up #pause
- Inconsistency forces users to memorize special cases

== Consistency Example
- Suppose we expose a `Fraction` type #pause
- Bad design: `add` and `subtract` return new values, but `multiply` and `divide` mutate in place #pause
- Good design: all arithmetic operations follow one coherent model #pause
- In Rust, users expect behavior to resemble primitive operators where possible

== API Stability
- Public types, traits, functions, constants, and modules form the API #pause
- Internal rewrites are cheap only if the public surface is narrow enough #pause
- Good library design is partly about leaving room for future change

== SemVer
- Cargo understands semantic versioning #pause
- In practice, users rely on compatible upgrades not breaking their code #pause
- Breaking changes are sometimes necessary, but they should not be casual

== Encapsulation
- Hide internal representation unless exposing it is truly necessary #pause
- Prefer private fields and explicit constructors or methods #pause
- Hidden representation improves safety, refactoring freedom, and invariants

== Ease of Use
- Library should be easy to start with #pause
- Library should also be hard to misuse #pause
- Good defaults, clear types, and small API surface help more than slogans about ergonomics

= Rust Library Design

== Rust Style
- Idiomatic Rust is usually the path of least resistance once the design is sound #pause
- Rust pushes strongly toward explicit ownership, small interfaces, and trait-based reuse #pause
- Fighting the language is usually a sign of design mismatch

== Documentation and Tests
- In Rust, documentation examples can be executable tests #pause
- This is useful because it checks that the documented usage still compiles and runs #pause
- Documentation quality directly affects API quality

== Doc Tests
```rust
/// # Examples
/// let mut vec = Vec::with_capacity(3);
/// vec.push(1);
/// vec.push(2);
/// assert_eq!(vec.len(), 2);
```

== Doc-Test Notes
- `cargo test` runs doctests too #pause
- Use `#` to hide support lines in examples #pause
- Use `no_run` when the code should compile but not execute #pause
- `#![deny(missing_docs)]` is reasonable for real libraries

== Standard Crate Layout
```text
.
|-- Cargo.toml
|-- benches
|-- examples
|-- src
|   |-- bin
|   |-- lib.rs
|   `-- main.rs
`-- tests
```

== Make Illegal States Unrepresentable
- Type system should rule out nonsense where practical #pause
- This is one of Rust's biggest strengths #pause
- If invalid states are easy to construct, the API is weak

== Stringly Typed API
```rust
use chrono::{DateTime, Utc, Weekday};

fn is_matching_day(datetime: DateTime<Utc>, day: &str) -> bool {
    let weekday = datetime.weekday();
    match day.to_lowercase().as_str() {
        "monday" => weekday == Weekday::Mon,
        "tuesday" => weekday == Weekday::Tue,
        "wednesday" => weekday == Weekday::Wed,
```
==
```rust
        "thursday" => weekday == Weekday::Thu,
        "friday" => weekday == Weekday::Fri,
        "saturday" => weekday == Weekday::Sat,
        "sunday" => weekday == Weekday::Sun,
        _ => unreachable!("there are only seven days in a week"),
    }
}
```

== Why This Is Bad
```rust
let is_tuesday = is_matching_day(
    some_date,
    "If you ask Rick Astley for a copy of the movie UP, he cannot give you it",
);
```
- Random strings are the wrong representation of a finite domain #pause
- The fix is not better validation text, but a better type #pause
- Enum is the natural model here

== Builder Pattern
- Rust has neither default parameters nor variadic functions in the C++ sense #pause
- Builder pattern is common for large configurable constructors #pause
- It also hides internal representation and allows validation at `build`

== Builder Example
```rust
fn main() {
    let car = CarBuilder::new("Toyota", "Corolla")
        .year(2020)
        .color(Color::Blue)
        .automatic(true)
        .build();

    println!("{:#?}", car);
}
```

== Builder Notes
- Typical shape: `new` then chained setters then `build` #pause
- Builder methods usually return `Self` or `&mut Self` #pause
- Session-type builders can enforce ordering constraints through different intermediate types

= Conversion and Trait Design

== Accept Flexible Input Types
```rust
use std::fs;
use std::path::Path;

fn file_exists(path: &Path) -> bool {
    fs::metadata(path).is_ok()
}
```
- Works, but it forces callers into one concrete representation

== `AsRef<Path>`
```rust
use std::fs;
use std::path::Path;

fn file_exists<P: AsRef<Path>>(path: P) -> bool {
    fs::metadata(path.as_ref()).is_ok()
}
```
==
```rust
fn main() {
    let path_str = "./example.txt";
    println!("{}", file_exists(path_str));

    let path_buf = Path::new("./example.txt").to_path_buf();
    println!("{}", file_exists(path_buf));
}
```

== Useful Conversion Traits
- `AsRef` and `AsMut` for cheap borrowed conversion #pause
- `From` and therefore `Into` for value conversion #pause
- `TryFrom` and `TryInto` for fallible conversion #pause
- `FromStr` to support `.parse()` on strings

== Useful Traits to Implement
- `Debug` and often `Display` #pause
- `Display` and `Error` for error types #pause
- `Clone`, sometimes `Copy` #pause
- `PartialEq`, `Eq`, `PartialOrd`, `Ord`, `Hash` where meaningful #pause
- `Default`, iterator traits, and serde traits where appropriate

== `FromIterator`
```rust
struct MyCollection<T> {
    elements: Vec<T>,
}

impl<T> FromIterator<T> for MyCollection<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut c = MyCollection { elements: Vec::new() };
```
==
```rust
        for i in iter {
            c.elements.push(i);
        }
        c
    }
}

let collected: MyCollection<i32> = vec![1, 2, 3, 4, 5].into_iter().collect();
```

== Orphan Rules
- You cannot implement a foreign trait for a foreign type #pause
- This is Rust's coherence rule at work #pause
- The usual escape hatch is a newtype wrapper

== Newtype Example
```rust
use std::fmt;

pub struct DisplayVec<T>(pub Vec<T>);

impl<T: fmt::Display> fmt::Display for DisplayVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let xs: Vec<String> = self.0.iter().map(|e| e.to_string()).collect();
        write!(f, "[{}]", xs.join(", "))
    }
}
```

== Extension Traits
- Extension traits add methods to existing types in a controlled way #pause
- Conventionally named `SomethingExt` #pause
- They are useful when you own the trait but not the target type

= Testing

== Test Categories
- Unit tests sit next to the code and may inspect internals #pause
- Integration tests live in `tests/` and treat the crate as an external dependency #pause
- Benchmarks are a different concern again

== Test Syntax
```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_addition() {
        let sum = 2 + 2;
        assert_eq!(sum, 4);
    }
```
==
```rust
    #[test]
    #[should_panic(expected = "assertion failed")]
    fn test_failure_scenario() {
        assert!(false, "This test will panic!");
    }
}
```

== `Result`-Returning Tests
```rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_division() -> Result<(), String> {
        let result = 10 / 2;
        if result == 5 {
            Ok(())
```
==
```rust
        } else {
            Err(String::from("Division result was not as expected"))
        }
    }
}
```

== Dev Dependencies
- Put test-only and benchmark-only crates into `[dev-dependencies]` #pause
- Typical examples: `criterion`, proptest, snapshot testing tools #pause
- Keep library dependencies and test dependencies conceptually separate

== Benchmarking
```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci(n - 1) + fibonacci(n - 2),
    }
}
```
==
```rust
fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
```

= Maintenance

== Maintenance Tools
- `rustfmt` for formatting #pause
- `clippy` for linting #pause
- CI should run both automatically #pause
- Publishing code with warnings is usually a bad sign

== Warnings and Checks
- `#![deny(warnings)]` is common in library code #pause
- You can enable additional compiler and clippy lints #pause
- Other useful tools: `cargo-machete`, `cargo-outdated`, `cargo-llvm-cov`, `cargo-tarpaulin`

== Summary
- Keep API small, consistent, and type-driven #pause
- Integrate with standard traits where it helps #pause
- Write tests and documentation as part of the design process #pause
- Use tooling so maintenance is not left to memory

= Questions?
