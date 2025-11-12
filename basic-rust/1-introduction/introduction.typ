#import "@preview/tiaoma:0.3.0": qrcode
#import "@preview/touying:0.6.1": *
#import themes.metropolis: *


#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => "",
  config-info(
    title: [Programming in Rust -- NPRG082],
    author: [Lukáš Hozda & Luukas Pörtfors],
    date: datetime.today(),
    logo: emoji.gear,
    //logo: image("images/ferris.png", width: 20pt, height: 20pt),
    institution: [Braiins Systems s.r.o],
  ),
)

#title-slide(
  config: config-page(
    background: [#place(
        bottom + left,
        box(
          inset: 0.5em,
          {
            set text(12pt)
            sys.inputs.at("store-path", default: none)
          },
        ),
      )],
  ),
)

== Today's topics

+ Organizational stuff

+ Rust background, history, and motivation

+ Installing and getting started with Rust

+ How Rust stands out from other languages

= Organizational stuff

#let discord-qr = {
  set align(center)
  figure(supplement: none, caption: [Discord invite], qrcode("https://discord.gg/E4XpeWbJpx", options: (scale: 6.0)))
}

== Organizational stuff

- This semester - basic Rust
- Next semester - advanced Rust
- The subject
  - 13 lessons from 2.10.2025
  - Odd lessons - lectures
  - Even lessons - labs
    - Ideally bring your laptop, so you can follow along :)
- Homework
  - ReCodEx
  - 60% to pass
- We want this subject to be open
  - Bring friends and whoever else may be interested :)
  - Feel free to share course materials

#slide(title: [Contacts and getting help])[
  - There's not going to be a Microsoft Teams
  - Discord server:
    - https://discord.gg/E4XpeWbJpx
  - Rust CZ/SK discord:
    - https://discord.gg/ZPdaXex8
  - You can reach out to Lukáš (preferably) or myself directly as well
][
  #discord-qr
]
= Meet your #strike[torturers] teachers

- Lukáš Hozda
- Luukas Pörtfors
- Pavel Simerda

#slide[
  #image("magnusi-dc.png")
][
  #image("luukasa-dc.png")
]

== Rust background and history

#grid(
  columns: (2.5fr, 1fr),
  [
    - Main inspirations:
      - C++: Performance, zero-cost abstractions
      - Haskell: Functional programming concepts, type system
      - Cyclone: Memory safety without garbage collection
      - Erlang: Concurrency model
      - OCaml: Type inference, pattern matching, syntax inspiration
    - History of Rust:
      - 2006: Personal project by Mozilla employee Graydon Hoare
      - 2009: Mozilla begins sponsoring the project
      - 2010: Rust announced to the public
      - 2015: Rust 1.0 released
      - 2018: Non-Lexical Lifetimes (NLL) stabilized
      - 2019: Async/await stabilized
      - 2021: Rust Foundation established
  ],
  [
    #image("what-the-hell-is-this.png")
  ],
)

#slide(title: "Why Rust?")[
  - Memory safety without garbage collection
  - Concurrency without data races
  - Zero-cost abstractions
  - Modern language features (pattern matching, closures, etc.)
  - Excellent package manager (Cargo)
  - Strong type system and ownership model
  - Cross-platform support
  - Growing ecosystem and community
][
  #image("rust-compiler.png")
]

#slide[
  #set align(center)
  #image("obrazekkkk.png")
]

== Installing Rust

- Use Rustup: https://rustup.rs/
  - Manages Rust versions and toolchains
  - Easy to update and switch between versions
- Discouraged: Using package managers (apt, brew, etc.)
- Windows: Use Rustup installer
  - Consider Windows Subsystem for Linux (WSL) for better experience
- macOS: Use Rustup via Terminal
- Linux: Use Rustup via Terminal (preferred method)


== Basic setup

- Ensure Rust Analyzer is installed:
  - VS Code: Install "rust-analyzer" extension
  - Other editors: Follow editor-specific instructions
- Install rustfmt and rust-src:
  - Usually comes with Rustup installation
  - If not:
    ```
    rustup component add rustfmt
    rustup component add rust-src
    ```
- Verify installation:
  - `rustc --version`
  - `cargo --version`
  - `rustfmt --version`

== VS Code/VS Codium

- Probably easiest option with best support
- Install "rust-analyzer" extension
  - Provides intelligent code completion and navigation
- Rust works well with debuggers
  - Use "CodeLLDB" extension for debugging Rust code
- Additional useful extensions:
  - "Better TOML" for Cargo.toml editing
  - "crates" for dependency version management

== Other editors

#slide[
  - Neovim or Emacs is recommended
  - RustRover is also good
    - https://www.jetbrains.com/rust/
][
  #figure(
    supplement: none,
    caption: [https://www.gnu.org/fun/jokes/ed-msg.html],
    image("ed-is-the-standard-editor.png"),
  )
]

== Dependencies

- Rust's standard library (std) is deliberately small
  - Focuses on core functionality
- Foreign dependencies are often needed
- Finding and getting dependencies:
  - crates.io: Official Rust package registry
  - libs.rs: Better UI for crates.io with recommendations
  - awesome-rust: Curated list of Rust libraries and resources
- In Rust parlance --- crates == packages
  - Both library and binary
- Use Cargo to manage dependencies:
  - Add to Cargo.toml
  - `cargo add <crate-name>` to automatically add and fetch


== What makes Rust different?

#grid(
  columns: (2fr, 1fr),
  [
    - Ownership and borrowing:
      - Unique approach to memory management
      - Eliminates entire classes of bugs at compile-time

    - Traits instead of traditional OOP:
      - No inheritance, favors composition
      - Interfaces through traits for polymorphism
      - #emoji.excl.double Marker traits #emoji.excl.double

    - Pattern matching:
      - Powerful and expressive way to destructure data

    - Enums with associated data:
      - More expressive than enums in other languages

    - Macros:
      - Powerful metaprogramming capabilities
  ],
  [
    #image("you-have-failed-the-borrow-check.png")
  ],
)

==

#grid(
  columns: (2fr, 1fr),
  [
    - Zero-cost abstractions:
      - High-level constructs compile to efficient low-level code

    - Fearless concurrency:
      - Type system prevents data races

    - No null or exceptions:
      - Uses Option\<T\> for nullable values
      - Result\<T, E\> for error handling

    - Lifetimes:
      - Explicit control over reference validity

    - Cargo ecosystem:
      - Integrated build system and package manager
  ],
  [
    #set align(center)
    #image("runtime-error.png")
  ],
)

== Hello world

```rust
fn main() {
    println!("Hello, World!");
}
```

- Save as *hello.rs*
- Compile: `rustc hello.rs`
- Run: `./hello`

Congratulations, you will never use *rustc* directly again through your entire career\ (unless you're working with the Linux kernel #emoji.eyes)

== Primitive Types

```rust
fn main() {
    // Ints
    let x: i32 = 42; let y: u64 = 1337;
    // Floats
    let pi: f64 = 3.14159;
    // Bools
    let am_i_locked_in: bool = true;
    // Chars
    let letter: char = 'A';
    // String slices
    let greeting: &str = "Hello, Rust!";
    // Printing values
    println!("x = {}, y = {}", x, y);
    println!("pi = {:.2}", pi);
    println!("Letter: {}", letter);
    println!("{}", greeting);
}
```
#slide[
  #set align(center)
  #table(
    columns: (auto, auto, auto),
    inset: 10pt,
    align: horizon,
    [Type], [Size], [Description],
    [i8, i16, i32, i64, i128], [8 to 128 bits], [Signed integers],
    [u8, u16, u32, u64, u128], [8 to 128 bits], [Unsigned integers],
    [isize, usize], [arch-dependent], [Pointer-sized integers],
    [f32, f64], [32 or 64 bits], [Floating-point numbers],
    [bool], [1 byte], [Boolean (true or false)],
    [char], [4 bytes], [Unicode scalar value],
    [()], [0 bytes], [Unit type (empty tuple)],
  )
]


== Till next

#slide[
  - Join discord (optional)

  - Set up Rust on your machine

  - See you next week!
][
  #discord-qr
]

#focus-slide[= Questions? ]
