#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Extension Lecture],
  config-info(
    title: [Advanced Rust (2026): Performance, Profiling, Measurement],
    subtitle: [Extension Lecture],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Measurement Discipline

== Performance Claim Standard
- Claim must include measurement method.
- Claim must include workload definition.
- Claim must include environment details.
- Claim without these is anecdote.

== Optimization Workflow
- Form hypothesis.
- Measure baseline.
- Apply one change.
- Re-measure and compare.
- Keep regression notes.

== Noise Sources
- CPU frequency scaling.
- Scheduler interference.
- Warmup effects and cache state.
- Input-size mismatch with production.

== Benchmark Build Mode
- Debug benchmarks are almost always misleading.
- Use release with representative compiler flags.
- Track Rust version and target triple.

= Benchmark Tooling

== Criterion Basics
- Statistical benchmarking with warmup and sampling.
- Better than ad-hoc `Instant` loop for microbenchmarks.

```rust
use criterion::{Criterion, black_box};

fn bench_sum(c: &mut Criterion) {
    let xs: Vec<i64> = (0..100_000).collect();
    c.bench_function("sum", |b| {
        b.iter(|| xs.iter().copied().sum::<i64>())
    });
}
```

== `black_box` Role
- Prevents optimizer from removing dead computations.
- Does not simulate realistic pipeline context.
- Use it precisely where elimination risk exists.

== Macro vs End-to-End Benchmarks
- Microbenchmark isolates local cost.
- End-to-end benchmark captures interaction overheads.
- Need both for reliable decisions.

= CPU and Memory Effects

== Cache Hierarchy Impact
- Data locality often dominates arithmetic complexity.
- Contiguous iteration can beat algorithmically similar random access.
- Cache miss rate must be observed, not guessed.

== False Sharing
- Independent counters on same cache line can thrash.
- Contention appears even without lock contention.
- Align/pad hot independent atomics when needed.

```rust
#[repr(align(64))]
struct Padded<T>(T);
```

== Branch Prediction
- Highly predictable branches are cheap.
- Data-dependent unpredictable branches inflate latency.
- Layout/data transform can outperform branch micro-tuning.

== Allocation Pressure
- Frequent allocate/free can dominate runtime.
- Reuse buffers in hot loops.
- Measure allocator behavior before custom allocator changes.

= Concurrency Performance

== Lock Contention Analysis
- Lock-free is not automatically faster.
- Measure critical-section length and contention frequency.
- Sometimes sharding + reduction beats lock-free complexity.

== Work Distribution
- Static chunking vs dynamic stealing tradeoff.
- Static: lower overhead, potential imbalance.
- Dynamic: better balance, scheduling overhead.

== Backpressure Throughput Tradeoff
- Unbounded queues maximize throughput short-term.
- Bounded queues protect memory and tail latency.
- Queue policy is architectural choice.

= Profiling

== Sampling Profilers
- Find where CPU time is spent.
- Good first pass before micro-optimization.
- Validate hotspots against intuition.

== Flamegraphs
- Visualize stack contribution by width.
- Identify unexpected hot call paths.
- Useful for inlining/abstraction overhead decisions.

== Hardware Counters
- Cache misses, branch misses, cycles/instructions.
- Need careful interpretation with workload context.
- Counters guide hypothesis generation, not final proof alone.

= Correctness vs Performance

== Guardrail Principle
- Never weaken correctness invariant without proof.
- Optimize representation and algorithm first.
- Weaken atomics/orderings only with explicit memory-model argument.

== SeqCst-to-RA Transition Pattern
- Start with strongest ordering for correctness.
- Validate behavior and tests.
- Weaken to Release/Acquire only with proof and benchmarks.

== Benchmark Reproducibility
- Commit benchmark code.
- Store baseline artifact/summary in repo or CI artifacts.
- Re-run on regressions with same methodology.

= Case Studies

== Case: Parser Throughput
- Baseline with naive split/alloc pattern.
- Optimize with borrowed slices and pre-allocated output.
- Measure speedup and memory impact separately.

== Case: Concurrent Counter
- Compare mutex, sharded mutex, atomic relaxed.
- Report throughput under 1, 2, 4, 8, 16 threads.
- Explain why winner changes by contention regime.

== Case: Channel Pipeline
- Stage imbalance can hide in average throughput.
- Measure queue depths and per-stage latency.
- Fix bottleneck before global tuning.

= Reporting

== Performance Report Template
- Problem statement.
- Environment and versions.
- Workload definition.
- Baseline metrics.
- Change and rationale.
- New metrics + confidence.
- Risks and follow-up.

== Red Flags in Reports
- Only one run.
- No variance.
- No environment details.
- No code/data availability.
- No mention of correctness checks.

== Final Rules
- Measurement quality is part of engineering quality.
- Profile first, optimize second.
- Prefer simple fast-enough design over fragile peak benchmark wins.
- Keep optimization arguments reviewable and reproducible.
