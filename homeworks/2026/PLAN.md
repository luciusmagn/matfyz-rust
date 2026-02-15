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

`A3A` is a library task based on `macro_rules!` for removing repeated boilerplate while preserving readable expansion behavior. `A3B` is an executable that consumes macro-generated structures and validates output contracts, reinforcing macro hygiene and maintainability instead of proc-macro complexity.

### Pair A4 (Lecture 4: library engineering and testing strategy)

`A4A` is a library assignment with strong emphasis on public API shape, error surface design, and stable behavior under multiple usage modes. `A4B` is an executable contract-style test harness task that checks compatibility and expected output from multiple API interaction scenarios.

### Pair A5 (Lecture 5: unsafe boundaries and sound wrappers)

`A5A` is a library task where a minimal unsafe core is wrapped in a safe interface with documented invariants and explicit assumptions. `A5B` is an executable behavior-check task that interacts only with the safe wrapper, ensuring students prove sound usage externally rather than exposing internals.

### Pair A6 (Lecture 6: systems interfaces, libc/nix/FFI)

`A6A` is a library assignment implementing thin, principled wrappers around selected systems/FFI operations with robust error mapping. `A6B` is an executable end-to-end tool that uses those wrappers in realistic workflows (I/O/process/network-like patterns) and produces judge-friendly deterministic output.

## Assignment Format Rules (Both Courses)

1. Executable homework submission filename: `main.rs` unless stated otherwise.
2. Library homework submission filename: `solution.rs`.
3. Each homework should provide at least 4 tests.
4. Output checking follows the same lenient rules as ReCodEx setup (trailing whitespace and missing final newline ignored; optional empty-line leniency can be toggled in local runner).
