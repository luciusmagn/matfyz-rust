# Introduction: teaching notes

## 90-minute allocation

| Minutes | Slides | Material |
| --- | --- | --- |
| 0–10 | 1–5 | Organization, homework, contacts, course topics |
| 10–20 | 6–10 | History, influences, design goals, static guarantees |
| 20–35 | 11–21 | Setup, tools, dependencies, first program, representation |
| 35–62 | 22–32 | Ownership, copying, borrowing, vector growth, cleanup |
| 62–75 | 33–37 | Absence, errors, data modelling, exhaustive matching |
| 75–85 | 38–44 | Threads, abstractions, runtime costs, unsafe code |
| 85–90 | 45–47 | Diagnostics, preparation, references, remaining questions |

This allocation includes live edits, compiler output, and discussion. The slide count alone does not determine duration. The main live sequence is assignment, cloning, vector growth, reserved capacity, and ending a borrow. The thread pair is the second longer demonstration. The short setup and reference slides need less time.

## Original material coverage

| Original introduction | Revised deck |
| --- | --- |
| Organization, ReCodEx, visitors, sharing | Course organization; Homework |
| Contacts and help | Contacts, with both original Discord links |
| History and language influences | Separate history and influence slides |
| Rust motivation | Design goals; Resource management; Static guarantees |
| Installation, basic setup, editors | Installing Rust through Cargo project |
| Dependencies | Crates and packages; Dependencies |
| Hello world, primitive types | First program; Values and types; Basic types |
| Ownership, borrowing, lifetimes | Owned assignment through Resource cleanup |
| Pattern matching and associated-data enums | Absence as a value; Data modelling; Exhaustive matching |
| Traits, composition, marker traits | Traits and dispatch; Shared state (`Send` / `Sync`) |
| Macros | `println!` and format checking in First program |
| Zero-cost abstractions | Abstraction costs; Traits and dispatch; Runtime costs |
| Concurrency | Borrowing across a thread boundary through Shared state |
| Option, Result | Absence as a value through Error-handling decisions |
| Preparation for next class | Before the next lab |

The old photos and memes are available in `basic-rust/1-introduction/`. The new slides use code and bullet lists instead.

## Administrative details to confirm

The old deck lists a 60% pass threshold, a 2 October 2025 start, alternating odd/even meetings, and Lukáš Hozda, Luukas Pörtfors, and Pavel Simerda as teachers. Those are historical details. The revised deck uses the local 2026 course outline for the lecture/lab structure and homework pairs, and names Lukáš on the title slide. Confirm this year's pass threshold and teaching team before adding them. The Discord invites are copied from the original deck; their validity has not been checked.

## Technical references

- [Rust's abstraction and trait design](https://blog.rust-lang.org/2015/05/11/traits/)
- [Ownership](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html)
- [Rustup on Windows](https://rust-lang.github.io/rustup/installation/windows.html)
- [Cargo packages](https://doc.rust-lang.org/cargo/guide/creating-a-new-package.html)
