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

= Measurement

== Performance Claims
- A performance claim without workload, method, and environment is anecdote #pause
- Throughput, latency, memory use, and tail behavior are different metrics #pause
- Optimization begins with a question, not with code motion

== What Must Be Reported
- Input shape and size #pause
- Rust version, target, and build mode #pause
- Machine characteristics that matter #pause
- Number of runs and observed variance

== Microbenchmark Pitfall
```rust
use std::time::Instant;

let start = Instant::now();
let result = compute();
let elapsed = start.elapsed();
println!("{result} in {elapsed:?}");
```
- This is often too noisy to tell you anything useful #pause
- Single-run timing is almost never enough

== Release Mode
- Benchmarking debug builds is usually pointless #pause
- Optimizer changes both absolute speed and code shape #pause
- Build mode is part of the experiment

== `black_box`
```rust
use std::hint::black_box;

let x = black_box(20_u64);
let y = fibonacci(x);
black_box(y);
```
- `black_box` helps stop the optimizer from deleting the benchmarked work #pause
- It does not magically make the benchmark realistic

== Criterion
```rust
use criterion::{black_box, Criterion};

fn bench_sum(c: &mut Criterion) {
    let xs: Vec<i64> = (0..100_000).collect();
    c.bench_function("sum", |b| {
        b.iter(|| xs.iter().copied().sum::<i64>())
    });
}
```
- Criterion handles warmup, repeated sampling, and statistics for you

== Benchmark Layers
- Microbenchmarks isolate local effects #pause
- End-to-end benchmarks show interaction costs and queueing effects #pause
- You usually need both

= Profiling

== Sampling Profilers
- Profilers answer where time is actually going #pause
- This often contradicts intuition #pause
- Sampling is usually the first tool, not the last

== Typical Tools
- `perf` on Linux #pause
- `cargo flamegraph` as a convenient wrapper #pause
- `cargo asm` or Compiler Explorer to inspect generated code

== Flamegraph
- Width corresponds to sampled time #pause
- Deep stack is not itself a problem #pause
- Wide unexpected frames are what you look for

== Hardware Counters
- Cycles, instructions, cache misses, branch misses #pause
- Counter values are only meaningful in context of the workload #pause
- Use them to guide hypotheses, not as standalone truth

= CPU and Memory Effects

== Cache Locality
- Contiguous access patterns often beat clever pointer-heavy structures #pause
- Big-O notation says nothing about cache misses #pause
- Data layout is part of algorithm design

== Allocation Pressure
- Repeated allocation can dominate hot paths #pause
- Reusing buffers and reserving capacity can matter more than arithmetic tweaks #pause
- Measure allocator behavior before rewriting everything around arenas

== Branch Prediction
- Predictable branches are cheap #pause
- Highly data-dependent branches can stall pipelines #pause
- Table layout or data transformation can beat branch micro-tuning

== False Sharing
```rust
use std::sync::atomic::AtomicU64;

#[repr(align(64))]
struct PaddedCounter(AtomicU64);
```
- Independent variables on the same cache line can still fight each other #pause
- This is a hardware effect, not a logical data-race issue

= Concurrency Performance

== Lock Contention
- Lock-free is not automatically faster #pause
- Contention frequency and critical-section length matter more than slogans #pause
- Sharding often beats global locking and also beats overcomplicated lock-free code

== Sharded Counter Shape
```rust
use std::sync::atomic::{AtomicU64, Ordering};

struct Sharded {
    shards: Vec<AtomicU64>,
}

impl Sharded {
    fn inc(&self, shard: usize) {
        self.shards[shard].fetch_add(1, Ordering::Relaxed);
    }
}
```

== Queue Bounds
- Unbounded queues can convert bursty load into memory blow-up #pause
- Bounded queues impose backpressure explicitly #pause
- Queue policy is part of system correctness, not just throughput tuning

== Static vs Dynamic Work Distribution
- Static chunking has low scheduling overhead #pause
- Dynamic stealing handles imbalance better #pause
- Which one wins depends on task variance

= Instrumentation

== Metrics
- Benchmarks tell you what happens in controlled runs #pause
- Metrics tell you what happens in long-running real systems #pause
- These are different tools for different questions

== Prometheus Counter
```rust
use prometheus::{Counter, Opts, Registry};

let counter_opts = Opts::new("my_counter", "A counter for tracking events");
let counter = Counter::with_opts(counter_opts).unwrap();
let registry = Registry::new();
registry.register(Box::new(counter.clone())).unwrap();
counter.inc();
```

== Prometheus Gauge
```rust
use prometheus::{Gauge, Opts, Registry};

let gauge_opts = Opts::new("my_gauge", "A gauge for tracking a value");
let gauge = Gauge::with_opts(gauge_opts).unwrap();
let registry = Registry::new();
registry.register(Box::new(gauge.clone())).unwrap();
gauge.set(10.0);
```

== Histograms and Labels
- Histograms are the usual tool for latency distributions #pause
- Labels are useful and dangerous #pause
- High-cardinality labels can destroy your metrics backend

== Instrumentation Discipline
- Instrument critical paths, not everything that moves #pause
- Avoid emitting metrics whose meaning is unclear #pause
- A cheap, stable metric is better than an expensive decorative one

= Reporting

== Reproducibility
- Commit benchmark code #pause
- Keep inputs or generators under version control #pause
- Write down command lines and environment assumptions #pause
- Otherwise nobody can tell whether the speedup is real

== Comparison Table
- Baseline #pause
- Changed version #pause
- Absolute numbers #pause
- Relative change #pause
- Confidence or variance estimate

== Common Failure Modes
- Only one run #pause
- No warmup #pause
- Debug build #pause
- No correctness check after optimization #pause
- Benchmark optimized away

== Summary
- Measure before and after, not only after #pause
- Profile before guessing #pause
- Treat cache, allocation, contention, and queueing as first-class effects #pause
- Production metrics and offline benchmarks complement each other

= Questions?
