#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 4],
  config-info(
    title: [Advanced Rust (2026): Library Engineering, Testing],
    subtitle: [Lecture 4],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Library Contract Design

== Public API is a Stability Boundary
- `pub` items define compatibility contract.
- Internal refactors are free only behind stable behavior.
- Every exported type/trait/error participates in semver surface.

== Layered Architecture
- Core: pure deterministic domain logic.
- Adapters: I/O, parsing formats, OS/runtime integration.
- Facade: ergonomic API over core + adapters.

== Why Deterministic Core Matters
- Easier tests.
- Easier benchmarking.
- Easier cross-platform behavior guarantees.

== Constructor Taxonomy
- `new`: cheap, obvious, infallible defaults.
- `try_new`: validation and fallible setup.
- Builder: many optional knobs and invariants.

```rust
pub struct Parser {
    limit: usize,
}

impl Parser {
    pub fn new() -> Self { Self { limit: 1024 } }
    pub fn try_with_limit(limit: usize) -> Result<Self, ConfigError> {
        if limit == 0 { return Err(ConfigError::ZeroLimit); }
        Ok(Self { limit })
    }
}
```

== Error Surface Strategy
- Domain-level error enum for callers.
- Preserve source error context where useful.
- Avoid leaking unrelated third-party error types.

```rust
#[derive(Debug)]
pub enum LoadError {
    Io(std::io::Error),
    InvalidHeader,
    InvalidRecord(usize),
}
```

== `thiserror`-Style Ergonomics
- Derive-based error display can reduce boilerplate.
- Keep variant taxonomy conceptual, not implementation-specific.
- `From` conversions should not erase meaning.

= Trait and Type System Boundaries

== Coherence and Orphan Rules
- Cannot implement foreign trait for foreign type.
- This shapes extension points for downstream users.
- Newtype wrappers are the standard escape hatch.

== Newtype Extension Pattern
```rust
pub struct Millis(pub u64);

impl std::fmt::Display for Millis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}ms", self.0)
    }
}
```

== Blanket Impls and Future Breakage
- Broad impls can block downstream specialization.
- Adding impl later can become breaking due to coherence conflicts.
- Be conservative with blanket coverage.

== Trait Object vs Generic API
- Generics: static dispatch, inlining, larger codegen.
- Trait objects: dynamic dispatch, stable ABI-like boundary.
- Pick based on performance + extensibility tradeoff.

== `Send + Sync` in Public Types
- Thread-safe bounds become part of API contract.
- Internal non-thread-safe field choice can leak into semver.
- Decide concurrency model early.

= Versioning and Evolution

== SemVer-Visible Changes
- Removing public item is breaking.
- Changing function signature is breaking.
- Adding enum variant can break exhaustive matches.
- Tightening trait bounds can break downstream.

== Non-Exhaustive Enums
- `#[non_exhaustive]` can preserve forward-compatibility.
- Tradeoff: callers cannot exhaustively match outside crate.

== Deprecation Workflow
- Deprecate old API path.
- Offer migration target with examples.
- Remove only at major version boundary.

== Feature Flag Design
- Features must compose coherently.
- Avoid hidden behavior changes by optional dependency toggles.
- CI should test meaningful feature combinations.

= Testing Strategy

== Test Layers
- Unit tests for local invariants.
- Integration tests for public behavior.
- End-to-end tests for workflow contracts.
- Regression tests for previously fixed bugs.

== Golden Output Tests
- Appropriate for formatters/CLI rendering.
- Keep fixtures small and reviewable.
- Document formatting contract explicitly.

```rust
#[test]
fn render_stable() {
    let out = render_items(&["a", "b"]);
    assert_eq!(out, "a\nb\n");
}
```

== Property Testing
- Validate algebraic invariants over broad input space.
- Good for parser/serializer roundtrip laws.
- Complements, not replaces, example-based tests.

== Fuzzing Role
- Good for parser robustness and panic discovery.
- Add corpus from real incidents when possible.
- Track crashes as regression tests.

== Determinism Under Parallelism
- Public API should state ordering guarantees.
- If non-deterministic, API must document it explicitly.
- Tests should verify exactly promised determinism level.

== Time and Flakiness
- Avoid wall-clock assertions when possible.
- Use deterministic schedulers/mocks where practical.
- CI flake budget should be effectively zero.

= Documentation as Engineering Artifact

== Rustdoc Contract Sections
- `# Panics`
- `# Errors`
- `# Safety` (for unsafe-related APIs)
- `# Examples` with realistic usage

== Executable Examples
- Doc examples are tests.
- Keep examples minimal but semantically complete.
- Include one failure-mode example where useful.

== Invariant Documentation
- Types should state representation invariants.
- Methods should state preconditions and postconditions.
- This reduces ambiguous bug reports.

= Practical Case Study

== Case: Parser + Renderer Crate
- Core parse model independent from input source.
- Renderer independent from parse source.
- IO wrappers at edge.

```rust
pub fn parse(input: &str) -> Result<Model, ParseError> { /* ... */ # unimplemented!() }
pub fn render(model: &Model) -> String { /* ... */ # unimplemented!() }
pub fn load_from_path(path: &std::path::Path) -> Result<Model, LoadError> { /* ... */ # unimplemented!() }
```

== Case: Async API Choice
- Provide sync core + async wrappers when feasible.
- Avoid forcing runtime choice into core semantics.
- Feature-gate runtime integrations explicitly.

== Case: Iterator-Oriented API
- Returning iterator avoids forced allocation.
- If stability of iteration order matters, define it.
- Expose owned collection only when required.

= Review Rubric

== API Review Questions
- What is stable contract here?
- What can change without major version bump?
- Is error taxonomy domain-based?
- Are trait bounds minimal and justified?

== Test Review Questions
- Which invariants are untested?
- Are regressions encoded as tests?
- Are nondeterministic elements controlled?
- Is failure output diagnostic enough?

== Final Rules
- Contract clarity beats clever type tricks.
- Core determinism first, adapters second.
- Test design is part of API design.
- If migration story is unclear, API is not ready.
