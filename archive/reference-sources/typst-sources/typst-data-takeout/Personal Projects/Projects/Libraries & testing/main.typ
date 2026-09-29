#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
 aspect-ratio: "16-9",
 footer: self => "",
 config-info(
   title: [Zen and the art of library maintenance],
   author: [Lukáš Hozda],
   date: datetime.today(),
   logo: emoji.key,
   institution: [Braiins Systems s.r.o],
 ),
)

#title-slide()

== Zen and the Art of Library Maintenance


#image("obrazek.png", height: 80%)
*Zen and the Art of Motorcycle Maintenance by Robert Pirsig - Good book about quality*

== Writing libraries and testing

- General principles
- Rust design
- Testing
- Maintenance

= General principles

== General principles

- There are some general points to consider when writing a library #pause
- Consistency #pause
- API stability #pause
  - The best library in the world is useless if I cannot rely on it not breaking completely every release#pause
- Encapsulation #pause
  - Data Hiding #pause
  - Modularity #pause
- Ease of use - use the right level of abstraction, provide sensible defaults #pause
- Technical aspects #pause
  - Performance #pause
  - Achievement of its goals #pause
- Licensing #pause
  - No serious developer will touch your "OSS" library if you forget a license

== Consistency

- Similar things should be done the same way as much as possible #pause
- Suppose we are writing a library for rational arithmetic (with fractions) #pause
  - We have a Fraction type #pause
  - Methods: #pause
    - `add(other_frac)`
    - `subtract(other_frac)` 
    - `multiply(other_frac)`
    - `divide(other_frac)` #pause
  - Bad: divide() and multiply() mutate the original, add() and subtract() return new Fraction #pause
  - Good: they all return a new Fraction and are *consistent* with primitive numbers' operators #pause
  - You laugh, but I have seen this exact thing #pause
- Also, be consistent across things like generics #pause
  - If my type can work with "anything that can be turned into a Path," the constructor should not take only a hardcoded string type

== API stability

- API (Application Programming Interface) #pause
  - You know this #pause
  - Types, methods, functions, constants, global variables (DONT), publicly exposed by a library #pause
- Naturally, for new libraries and SW in general, some breaking changes are unavoidable #pause
  - It would be pretty bad to be forever shackled to the poor decisions of your youth #pause
- What we need is a good API design and versioning #pause
- Good API design #pause
  - Maximum flexibility, minimal surface (if you expose the entirety of your library, every change is a breaking API change) #pause
    - Allows me to do extensive rewrites of the underlying logic while maintaining the same API #pause
- Versioning - SemVer (Semantic Versioning) #pause
  - "I can depend on the latest version that will not break my stuff" #pause
  - Built into Cargo with a quirk #pause
    => "1.0.0" considered to be "^1.0.0"

== Encapsulation

- Data hiding #pause
  - Your library should only expose what needs to be exposed #pause
  - Consider private visibility the default, and mark stuff as public when needed #pause
    - In the context of Rust, only mark the actual API as `pub`, use `pub(crate)` if something needs to be accessible everywhere in your lib #pause
  - Hiding the internal state of your objects is good for both rewrites and security #pause
    (You do not need to account for the possibility that someone writes invalid values into your `struct`'s fields) #pause
- Modularity #pause
  - Types understood through their interfaces as distinct units

== Ease of use

- Quick to get started with #pause
- Easy to use in the correct way #pause
- Performant, achieve stated goal, flexible enough #pause
- Well tested and documented #pause
  - This actually helps with design, since it forces you to look at your library from a user perspective #pause
- Small API #pause
  - Less to learn, maintain, less opportunity to make breaking changes

= Rust library design

== Rust library design

- Write idiomatic code (code that looks nice) #pause
  - Typically also happens to be easy to write (in terms of the language not putting obstacles in your way) #pause
- Rust railroads you into idiomatic code pretty hard #pause
  - Everyone fought the borrow-checker and various APIs at one time

== Going back to the "well tested" point

- In Rust, we have doc-tests #pause
  - Documentation examples become tests: #pause
#let r = raw.with(lang: "rust")
#r("
/// # Examples
/// ```
/// let mut vec = Vec::with_capacity(10);
/// assert_eq!(vec.len(), 0);
/// assert!(vec.capacity() >= 10);
/// for i in 0..10 {
///     vec.push(i);
/// }
/// assert_eq!(vec.len(), 10);
/// assert!(vec.capacity() >= 10);
/// ```
");

- Rust highlighting by default, use "\#" to hide lines, use `no_run` (after opening backticks) to prevent #pause
  snippet from being a doctest #pause
- `cargo test` will run doctests #pause
- Document public items in any case #pause
  - You can use `#![deny(missing_docs)]` in your crate root

== Standard layout

```
$ tree
.
|-- Cargo.toml
|-- benches                 - benchmarks
|-- examples                - independent examples (binaries)
|-- src                     ↓
|   |-- bin                 ↓
|   |   `-- something.rs    - other binaries (if any)
|   |-- lib.rs              - library entrypoint
|   `-- main.rs             - if your crate also has an executable (eg. CLI)
`-- tests                   - "integration" tests
```

== API design

- Make illegal states unrepresentable (Haskell proverb) #pause
  - Meaning - only correct usage, within reason, should compile #pause
- Avoid "stringly-typed" APIs (Pascal Hertleif quote)
==
```rs
use chrono::{DateTime, Utc, Weekday};

fn is_matching_day(datetime: DateTime<Utc>, day: &str) -> bool {
    let weekday = datetime.weekday();
    match day.to_lowercase().as_str() {
        "monday" => weekday == Weekday::Mon,
        "tuesday" => weekday == Weekday::Tue,
        "wednesday" => weekday == Weekday::Wed,
        "thursday" => weekday == Weekday::Thu,
        "friday" => weekday == Weekday::Fri,
        "saturday" => weekday == Weekday::Sat,
        "sunday" => weekday == Weekday::Sun,
        _ => unreachable!("there is only 7 days in a week, no?"),
    }
}

```

== Oops

```rs 
let is_tuesday = is_matching_day(
  some_date,
  "If you ask Rick Astley for a copy of the movie “UP”, he cannot give you it as he can never give you up. But, by doing that, he is letting you down, and thus, is creating something known as the Astley Paradox.",
);
```

- The correct thing to do is use a more concrete type (e.g. an enum for all the days in this case) #pause
  - Enums are great for representing states in general #pause
  - Unlike random strings, you can document enums

== Builder pattern

- Extremely popular in Rust (because we have no default parameters and variadic functions) #pause

```rs
fn main() {
    let car = CarBuilder::new("Toyota", "Corolla")
        .year(2020)
        .color(Color::Blue)
        .automatic(true)
        .build();

    println!("{:#?}", car);
}
```

- Essentially: Constructor (`T::new()`) -> methods that modify the instance -> `build()/finish()/whatever()` #pause
- Generally, the builder methods return either *T* or *&mut T* (and rarely *&T* if doing interior mutability magic) #pause
- Let's you validate and convert parameters implicitly, use defaults, and keep internal structure hidden #pause
  - Forward compatibility - you can change the struct fields however you want #pause
- In std: e.g. `std::process::Command` #pause
- You can also do *session types* where your builder goes through several types with different methods #pause
(Meaning you can force an order of operations - useful with protocols, e.g. HTTP requests) #pause
(starts to smell like substructural type systems :))

== Type conversion ergonomics

```rs
use std::path::Path; // let's pretend Path::exists() doesn't exist :)
use std::fs;

fn file_exists(path: &Path) -> bool {
    fs::metadata(path).is_ok()
}

fn main() {
    // Example usage
    let path = Path::new("./example.txt");
    println!("Does the file exist? {}", file_exists(path));
}
```
- Not ideal, since we now require user to construct a Path directly. Less flexible

==

```rs
use std::path::Path; // let's pretend Path::exists() doesn't exist :)
use std::fs;

fn file_exists<P: AsRef<Path>>(path: P) -> bool {
    fs::metadata(path.as_ref()).is_ok()
}

fn main() {
    // Example usage with a &str
    let path_str = "./example.txt";
    println!("Does the file exist? {}", file_exists(path_str));

    // Example usage with a PathBuf
    let path_buf = Path::new("./example.txt").to_path_buf();
    println!("Does the file exist? {}", file_exists(path_buf));
}

```

==

Nice traits to use: #pause
- AsRef #pause
- AsMut #pause
- From #pause
- Into - Do not implement directly unless needed (blanket impl on From\<T\>) #pause
- TryFrom #pause
- TryInto #pause
- FromStr (gives you a free `.parse()` on string types) #pause

Naturally, you can implement these on your types wherever applicable

== Nice traits to implement

- Debug and optionally Display #pause
- Display & Error (for your Error types) #pause
- (Partial)Ord, (Partial)Eq #pause
- Clone, and Copy if it makes sense #pause
- Hash (if applicable) #pause
- Serde Serialize & Deserialize #pause
- *Default* #pause
- Also implement Iterator if your type is a collection or a stream #pause
- And FromIterator if you want to use `.collect()` #pause
- TIP: Prefer taking a slice to taking a Vec, and if possible, just take a generic Iterator #pause
  - e.g `fn hi<T: Iterator<Item=u32>>(numbers: T)`

==

```rs
struct MyCollection<T> { // simple example
    elements: Vec<T>,    // consider e.g. address book or any tree ADT
}

// Implementing the FromIterator trait for MyCollection
impl<T> FromIterator<T> for MyCollection<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut c = MyCollection { elements: Vec::new() };
        
        for i in iter { c.elements.push(i); }
        
        c
    }
}

let collected: MyCollection<i32> = vec![1, 2, 3, 4, 5].into_iter().collect();
println!("{:?}", collected.elements); // Prints: [1, 2, 3, 4, 5]
```
  
== Extension traits and orphan rules

- It is handy to implement foreign traits, sometimes for foreign types #pause
  - Forbidden to do ForeignTrait on ForeignType -> Use `newtype` pattern #pause
- You can also write extension traits to provide additional functionality to existing items: #pause
  - By convention name is `TraitOrTypeNameExt` #pause
  - In std for example `std::ascii::AsciiExt` (deprecated)
- https://github.com/Ixrec/rust-orphan-rules

==

```rs
use std::fmt;
pub struct DisplayVec<T>(pub Vec<T>);

impl<T: fmt::Display> fmt::Display for DisplayVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let elements_as_strings: Vec<String> = self.0.iter().map(|e| e.to_string()).collect();
        write!(f, "[{}]", elements_as_strings.join(", "))
    }
}

let numbers = DisplayVec(vec![1, 2, 3, 4, 5]);
println!("{}", numbers); // Prints: "[1, 2, 3, 4, 5]"
```

== Testing
- Use `[dev-dependencies]` in Cargo.toml for things you only need for examples #pause
  and testing #pause
- e.g. criterion for writing benchmarks #pause
- In Rust: #pause
  - Unified built-in syntax for writing tests #pause
  - Types of tests: #pause
    - Unit - next to your code or `src/tests.rs` #pause
    - Integration - in `tests/`, view your code as a dependency #pause
      - -> you cannot touch internal states #pause
    - Benchmarks - similar to integration tests, but for making perf statistics #pause
      - Some parts of support are nightly-only

== Test syntax in Rust

```rs
#[cfg(test)]
mod tests {
    #[test]
    fn test_addition() {
        let sum = 2 + 2;
        assert_eq!(sum, 4);
    }

    #[test]
    #[should_panic(expected = "assertion failed")]
    fn test_failure_scenario() {
        assert!(false, "This test will panic!");
    }
}
```
Run with `cargo test`

== Result\<T\> tests
```rs
#[cfg(test)]
mod tests {
    #[test]
    fn test_division() -> Result<(), String> {
        let result = 10 / 2;
        if result == 5 {
            Ok(())
        } else {
            Err(String::from("Division result was not as expected."))
        }
    }
}
```

== Benchmark

```rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn fibonacci(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        _ => fibonacci(n - 1) + fibonacci(n - 2),
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fibonacci 20", |b| b.iter(|| fibonacci(black_box(20))));
}

criterion_group!(benches, criterion_benchmark); criterion_main!(benches);
```
Run with `cargo bench`
== Maintenance

== Maintenance
- Use `rustfmt` (formatting) and `clippy` (linter) #pause
  - VS Code and others can do format-on-save, handy #pause
- You can add them as automatic checks to your CI pipeline #pause
  - E.g. GitHub Actions, Gitlab CI #pause
- `#![deny(warnings)]` in crate root #pause
  - In Rust, it is a standard practice not to publish code with warnings #pause
  - You can enable additional lints and additional clippy lints #pause
- Other nice checks: #pause
  - `cargo-machete` - detect unused deps #pause
  - `cargo-outdated` - outdated deps #pause
  - `cargo-tarpaulin/cargo-llvm-cov` - code coverage

== Conclusion

- Make your API minimal and consistent #pause
- Don't hardcode types where you don't need to #pause
  - Use generics with trait bounds #pause
- Integrate with Rust std traits #pause
- Write tests and documentation #pause
- Use tools to perform automatic checks