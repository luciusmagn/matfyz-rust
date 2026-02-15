#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 1],
  config-info(
    title: [Advanced Rust (2026): Parallel Model and Determinism],
    subtitle: [Lecture 1],
    author: [Lukas Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Lecture Framing

== Learning goals
- Build a precise mental model of parallel execution in Rust.
- Distinguish memory safety from logical correctness under concurrency.
- Design deterministic outputs even when execution is parallel.
- Prepare for homework pair A1 (library + executable).

== Time budget (90 min)
- 10 min: framing and vocabulary
- 20 min: Send/Sync + ownership in multithreaded code
- 20 min: synchronization patterns and tradeoffs
- 20 min: deterministic parallel architecture patterns
- 20 min: guided Q&A and assignment kickoff

= Rust's Concurrency Model

== Data race vs race condition
- Data race: unsynchronized conflicting access, at least one write.
- Safe Rust prevents data races in safe code.
- Race conditions can still happen and break logic.
- Memory-safe is not equal to correct.

== Send and Sync
- `Send`: value can move across thread boundary.
- `Sync`: shared reference can cross thread boundary.
- Most types auto-derive these traits.
- Manual impls are `unsafe` and require strict invariants.

== Ownership under parallel execution
- `thread::spawn` requires captured data to outlive thread (`'static`).
- `move` transfers ownership into worker closures.
- `thread::scope` allows borrowing from parent stack safely.

```rust
std::thread::scope(|scope| {
    let data = vec![1, 2, 3, 4];
    scope.spawn(|| {
        // borrow from parent scope is valid here
        println!("{}", data.len());
    });
});
```

= Synchronization Patterns

== Shared state and lock-based coordination
- `Arc<Mutex<T>>`: simple, explicit, can serialize throughput.
- `Arc<RwLock<T>>`: helps read-heavy workloads.
- Keep lock hold time short; never lock in inconsistent order.

== Atomics
- Great for counters/flags/small coordination values.
- Do not replace structured ownership or message passing.
- Choose ordering intentionally; default to `SeqCst` until justified.

== Message passing
- Channels isolate mutable state behind worker boundaries.
- Fits job queues and fan-out/fan-in workflows.
- Often easiest path to deterministic output with final aggregation.

= Deterministic Parallel Design

== Why determinism matters in this course
- ReCodEx compares stdout against expected text.
- Nondeterministic ordering means flaky submissions.
- Rule: parallel internal execution, deterministic external contract.

== Pattern A: index-tagged parallel map
- Attach stable index/id to each task.
- Compute in parallel.
- Sort or place by index before output.

== Pattern B: staged pipeline
- Stage 1 parse and validate input.
- Stage 2 parallel compute on immutable task snapshots.
- Stage 3 single-thread aggregation and formatting.

== Pattern C: deterministic reduction
- Use associative reductions for partial results.
- Merge partials in predefined order, not arrival order.

= Applied Segment

== Mini case study
- Input: list of jobs with `job_id` and values.
- Parallel worker stage computes per-job metrics.
- Final output sorted by `job_id`.
- This is exactly the shape of A1B.

== Common failure modes to watch
- Writing output directly from workers.
- Hidden shared mutable state (`Arc<Mutex<Vec<_>>>`) without ordering policy.
- Non-explicit API contracts for edge cases.

== Testing strategy for concurrency tasks
- Keep expected output deterministic.
- Include edge cases: empty input, single worker, workers > workload.
- Include negative numbers and mixed-size jobs.
- Avoid sleep-based "proof" of correctness.

= Homework Pair A1

== A1A (library)
- API for deterministic chunk planning and parallel reductions.
- Focus: explicit behavior contract and edge-case handling.

== A1B (executable)
- Deterministic parallel job processor.
- Focus: stable output contract under internal concurrency.

== Delivery expectations
- Submission files: `solution.rs` (library), `main.rs` (executable).
- At least four tests per homework are provided.
- Target toolchain: stable Rust 1.88.

= Discussion Block

== Questions to drive discussion
- When would you prefer channels over locks?
- Where does deterministic ordering belong: workers or aggregator?
- Which correctness properties should be tested first?

== Next lecture preview
- Borrow-checker-driven API architecture.
- Lifetimes as design constraints, not syntax obstacles.
