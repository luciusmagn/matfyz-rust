#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 6],
  config-info(
    title: [Advanced Rust (2026): Systems Interfaces],
    subtitle: [Lecture 6],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Systems Boundary Model

== What "Systems Programming" Means Here
- Explicit ownership of OS resources.
- Explicit failure policy for each boundary.
- Explicit concurrency and shutdown semantics.
- No hidden magic in runtime behavior.

== Boundary Layers
- Pure domain layer.
- Resource abstraction layer (files, sockets, processes).
- Adapter/CLI/service layer.
- Each layer narrows error vocabulary.

== Resource Lifetime = Safety + Reliability
- FD leak is both correctness and ops issue.
- RAII + Drop provide default cleanup path.
- Explicit handoff required for cross-boundary ownership transfer.

= File and Descriptor Semantics

== `File` Ownership
- `File` owns descriptor and closes in Drop.
- `AsRawFd` borrows descriptor.
- `IntoRawFd` transfers ownership out.

```rust
use std::fs::File;
use std::os::fd::{AsRawFd, IntoRawFd};

let f = File::open("/etc/hosts").unwrap();
let borrowed = f.as_raw_fd();
println!("borrowed fd = {borrowed}");
```
==
```rust
let f2 = File::open("/etc/hosts").unwrap();
let owned_fd = f2.into_raw_fd();
println!("owned fd = {owned_fd}");
```

== Buffered vs Unbuffered I/O
- Syscalls are expensive.
- Buffering strategy changes throughput and latency.
- Flush policy is part of protocol correctness.

```rust
use std::io::{BufRead, BufReader};

let f = std::fs::File::open("data.txt").unwrap();
let r = BufReader::new(f);
for line in r.lines() {
    println!("{}", line.unwrap());
}
```

== Memory-Mapped I/O Tradeoff
- Can reduce copy overhead.
- Introduces lifetime/consistency subtleties.
- File mutation/truncation hazards become correctness concerns.

= Process and Command Interfaces

== Process Execution Contract
- Command line construction must be explicit.
- Exit status and stderr are first-class outputs.
- Timeout and cancellation policy must be defined.

```rust
use std::process::Command;

let out = Command::new("rustc")
    .arg("--version")
    .output()
    .expect("spawn failed");
println!("status: {}", out.status);
```

== Pipeline Ownership Rules
- Parent owns child handles unless transferred.
- Read all pipes or risk deadlock on full buffers.
- Avoid shell-string concatenation, pass args structurally.

== Exit Status Mapping
- OS status codes are transport-level facts.
- Domain API should map them to semantic failures.
- Preserve raw status for diagnostics.

= Networking and Async Runtime Boundaries

== Blocking vs Async Socket Paths
- Blocking APIs simpler but thread-heavy at scale.
- Async APIs shift complexity to runtime and cancellation semantics.
- Choose model based on workload shape.

== Reactor Concept
- OS readiness events feed user tasks.
- Task is polled when IO likely to progress.
- Readiness is hint, not completion guarantee.

== Backpressure
- Producer faster than consumer must be controlled.
- Queue bounds are correctness knobs, not just performance knobs.
- Unbounded channels can become memory failure mode.

```rust
use tokio::sync::mpsc;

let (tx, mut rx) = mpsc::channel::<i64>(128);
```

== Cancellation Semantics
- Dropping future does not roll back external effects.
- API should define cancellation safety explicitly.
- Idempotent operations simplify restart/retry logic.

= Data Representation Boundaries

== Binary Layout Discipline
- `repr(C)` for C interoperability.
- Endianness must be explicit in protocol design.
- Avoid transmute-based parsing of untrusted input.

== Serialization Contracts
- Version fields should be explicit.
- Unknown fields policy (ignore/fail) must be defined.
- Deterministic encoding aids reproducibility tests.

== Zero-Copy Parsing Tradeoff
- Saves allocations.
- Increases lifetime coupling and borrow complexity.
- Good for hot paths with stable buffer ownership.

= Error Architecture

== Error Layering
- Low-level: OS / libc / transport details.
- Mid-level: adapter context and retries.
- High-level: domain failure semantics.

== Context Preservation
- Include operation + target in errors.
- Preserve source chain where useful.
- Keep displayed message concise but specific.

```rust
fn read_cfg(path: &std::path::Path) -> Result<String, String> {
    std::fs::read_to_string(path)
        .map_err(|e| format!("read {} failed: {e}", path.display()))
}
```

== Retry Policy
- Retry only transient failures.
- Bound retry count and delay strategy.
- Add jitter in distributed systems contexts.

= Time, Signals, Shutdown

== Clock Semantics
- Monotonic clock for durations/timeouts.
- Wall clock for user-facing timestamps.
- Never mix these concepts in timeout logic.

== Signal Handling Contract
- Signal callback context is constrained.
- Often best to flip atomic flag and handle in main loop.
- Shutdown path must be idempotent.

== Graceful Shutdown Sequence
- Stop accepting new work.
- Drain inflight tasks with deadline.
- Persist final state.
- Close resources in deterministic order.

= Case Studies

== Case: Deterministic Log Processor
- Stable parse rules.
- Stable output ordering.
- Explicit malformed-input policy.
- Resource usage bounded by streaming strategy.

== Case: Command Runner Library
- Typed command specification.
- Captured stdout/stderr.
- Timeout support.
- Domain-mapped exit failures.

```rust
struct RunResult {
    status: i32,
    stdout: String,
    stderr: String,
}
```

== Case: File Watcher Service
- Event coalescing policy matters.
- Debounce window affects correctness and latency.
- Shutdown race handling required.

= Verification and Operations

== Systems Test Matrix
- Linux/macOS differences.
- Different filesystem semantics.
- Slow disk and high-latency network simulation.
- Resource exhaustion paths.

== Observability by Design
- Structured logs with request/task IDs.
- Metrics: queue depth, error class counts, latency percentiles.
- Trace boundaries around OS interaction points.

== Benchmarks with Realistic Workload
- Synthetic microbenchmarks are necessary but insufficient.
- Include end-to-end benchmark with realistic data shape.
- Track variance and tail latency, not only mean.

== Final Rules
- Define boundary contracts before implementation details.
- Treat shutdown and failure as primary flows.
- Prefer explicit ownership/resource handoff.
- If ops behavior is unclear, API is incomplete.
