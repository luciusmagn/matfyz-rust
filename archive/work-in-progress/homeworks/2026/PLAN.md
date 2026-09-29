# Homework Plan 2026 (Draft)

This plan follows the agreed cadence:

1. Homeworks are released in pairs.
2. Each pair maps to one lecture theme.
3. Lab reinforces the same theme.
4. Students effectively get three weeks (lecture + next lecture in between) to finish a pair.

Rust target for all assignments: stable `1.88`.

## Introductory Rust (2026)

### Pair I1 (Lecture 1: ownership, moves, borrowing)

`I1A` is an executable task focused on deterministic parsing and transformation with strict ownership-aware handling of inputs, so students immediately confront move/borrow decisions in practical code. `I1B` is a library task that exposes a small API over owned and borrowed data, forcing students to design function signatures instead of only writing local logic.

### Pair I2 (Lecture 2: enums, pattern matching, Option/Result)

`I2A` is an executable state-driven command processor where enum modeling is mandatory for correctness and readability. `I2B` is a library assignment centered on typed domain states and error paths, with tests that intentionally probe invalid branches and force exhaustive pattern handling.

### Pair I3 (Lecture 3: structs, modules, lifetimes, custom errors)

`I3A` is an executable integration task that consumes a structured API and validates formatted output against edge conditions. `I3B` is a library assignment where students build a module with constructor/builder-like entry points and explicit error types, balancing ergonomics with predictable ownership/lifetime behavior.

### Pair I4 (Lecture 4: traits, generics, iterators)

`I4A` is an executable data pipeline that rewards iterator composition and generic helper functions over repetitive loops. `I4B` is a library assignment with trait-bounded generic operations and reusable adapters, testing both static polymorphism and API clarity under constraints.

### Pair I5 (Lecture 5: threads, shared state, channels)

`I5A` is a deterministic concurrent executable where students must coordinate worker results into stable output (no race-dependent output ordering). `I5B` is a library assignment around channel-based task dispatch and synchronization primitives, emphasizing thread-safe API boundaries and deadlock-avoidance patterns.

### Pair I6 (Lecture 6: async basics with Tokio)

`I6A` is an executable async processing task (timeouts/retries/pipeline behavior) with deterministic final output to keep judging robust. `I6B` is a library assignment implementing async utilities (e.g., bounded retries/cancellation-safe helpers), testing practical runtime usage without requiring nightly features.

## Advanced Rust (2026)

### Pair A1 (Lecture 1: advanced concurrency + memory model intuition)

`A1A` is a library assignment requiring a concurrency abstraction that makes ordering and synchronization choices explicit in API design. `A1B` is an executable workload processor where correctness must hold under parallel execution while maintaining deterministic externally observed behavior.

### Pair A2 (Lecture 2: borrow-checker-aware API architecture)

`A2A` is a library assignment with lifetime-sensitive views/slices/borrows that must compile cleanly without over-allocating. `A2B` is an executable validator that exercises those APIs across edge lifetimes and ownership transitions, focusing on signature quality rather than syntax tricks.

### Pair A3 (Lecture 3: declarative macros in production-style code)

`A3A` is a library task based on `macro_rules!` repetition, formatting, and small reusable helpers. `A3B` is a second library task focused on recursive declarative macros, token-tree matching, and macro-generated impl blocks, which better matches the lecture's emphasis on recursive `macro_rules!`, `$crate`, and matcher-level reasoning.

### Pair A4 (Lecture 4: library engineering and testing strategy)

`A4A` is an async rate-limiter library assignment with shared state, clone semantics, timeout behavior, and a deliberately small public API. `A4B` is an async retry-policy library assignment that tests generic futures, error surface design, delay calculation, and deterministic retry semantics.

### Pair A5 (Lecture 5: unsafe boundaries and sound wrappers)

`A5A` is a manual fixed-capacity vector assignment, including zero-sized type handling, drop correctness, and safe slice exposure over an unsafe representation. `A5B` is a typed byte-view assignment using an unsafe marker trait, alignment checks, size checks, and safe observation of a raw byte buffer as typed values.

### Pair A6 (Lecture 6: systems interfaces, libc/nix/FFI)

`A6A` is a C-string boundary assignment that forces explicit handling of NUL termination, interior NULs, borrowed C strings, and safe calls into `libc::strlen`. `A6B` is a Unix file-descriptor assignment using `OwnedFd` and `BorrowedFd`, pipe creation, retry-on-`EINTR` read/write loops, EOF handling, and descriptor flag updates.

## Assignment Format Rules (Both Courses)

1. Executable homework submission filename: `main.rs` unless stated otherwise.
2. Library homework submission filename: `solution.rs`.
3. Each homework should provide at least 4 tests.
4. Output checking follows the same lenient rules as ReCodEx setup (trailing whitespace and missing final newline ignored; optional empty-line leniency can be toggled in local runner).
