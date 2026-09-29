#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
 aspect-ratio: "16-9",
 footer: self => "",
 config-info(
   title: [Rust Concurrency],
   author: [Lukáš Hozda],
   date: datetime.today(),
   logo: emoji.gear,
   institution: [Braiins Systems s.r.o],
 ),
)

#title-slide()

= Concurrency in Rust

== Data Race Definition
- Multiple threads accessing shared data
- At least one thread is writing
- No synchronization between accesses

== Rust's Memory Model
- Single writer XOR multiple readers
- Enforced at compile time
- Extends to concurrent code
- Built around preventing data races

```rust
// This won't compile - potential data race
fn wrong(data: &mut Vec<i32>) {
   std::thread::spawn(|| {
       data.push(42); // <- borrowed data in thread
   });
   data.push(43);     // <- also using data here
}
```

== Thread Basics
- OS-level threads
- Guaranteed preemptive multitasking
- Heavier than async tasks
- Can use all CPU cores

```rust
use std::thread;

let handle = thread::spawn(|| {
   // New thread
   println!("Hello from thread!");
});

handle.join().unwrap(); // Wait for completion
```

== Moving Values Into Threads
- Closure must be 'static
- Use move keyword
- Values are transferred between threads

```rust
let v = vec![1, 2, 3];

let handle = thread::spawn(move || {
   println!("Vector: {:?}", v);
}); // v is moved into thread

// Cannot use v here anymore
handle.join().unwrap();
```

== Scoped Threads
- Allows borrowing from parent scope
- Guarantees threads finish before scope ends
- More flexible than regular threads

```rust
let mut v = vec![1, 2, 3];
thread::scope(|s| {
   s.spawn(|| {
       // Can borrow v here!
       println!("First: {:?}", &v);
   });
   s.spawn(|| {
       v.push(4); // But we cannot mutate here!
   });
});
v.push(5); // We can mutate here :)
```

== Sharing Immutable Data
- Arc = Atomic Reference Counting
- Thread-safe shared ownership
- Clone = increment reference count
- Drop = decrement reference count

```rust
use std::sync::Arc;

let data = Arc::new(vec![1, 2, 3]);

let data_clone = Arc::clone(&data);
thread::spawn(move || {
   println!("Thread sees: {:?}", *data_clone);
});
```

== Arc vs Rc
- Rc = single-threaded reference counting
- Arc = atomic (thread-safe) reference counting
- Arc has small performance overhead
- Cannot convert between them

```rust
use std::rc::Rc;      // Wrong!
use std::sync::Arc;   // Correct!

let bad = Rc::new(42);
// thread::spawn(move || {  // Compile error!
//     println!("{}", bad);
// });

let good = Arc::new(42);
thread::spawn(move || {     // Works!
   println!("{}", good);
});
```

= Shared Mutable State

== Mutex\<T\>
- Mutual exclusion
- Only one thread can access data at once
- Others must wait
- RAII-style locking

```rust
use std::sync::{Arc, Mutex};

let counter = Arc::new(Mutex::new(0));
let counter2 = Arc::clone(&counter);

thread::spawn(move || {
   let mut num = counter2.lock().unwrap();
   *num += 1;
}); // lock automatically released here
```

== parking_lot::Mutex
- Smaller and faster than std::sync::Mutex
- No poisoning on panic
- More efficient spinning
- Platform-specific optimizations

```rust
use parking_lot::Mutex;

let mutex = Mutex::new(0);
{
   let mut guard = mutex.lock(); // No unwrap needed!
   *guard += 1;
} // lock released here
```

== RwLock - Reader-Writer Lock
- Multiple readers OR single writer
- Good for read-heavy workloads
- Higher overhead than Mutex

```rust
use std::sync::RwLock;

let data = RwLock::new(vec![1, 2, 3]);

// Multiple readers
let r1 = data.read().unwrap();
let r2 = data.read().unwrap();

// Only one writer
let mut w = data.write().unwrap();
w.push(4);
```

== Deadlock Prevention
- Lock ordering matters
- Always acquire locks in same order
- Use scope-based RAII locks
- Avoid nested locks when possible

```rust
let mutex1 = Mutex::new(0);
let mutex2 = Mutex::new(0);

// Potential deadlock!
let _guard1 = mutex1.lock().unwrap();
let _guard2 = mutex2.lock().unwrap();

// Better: always lock in same order
// across all code paths
```

= Message Passing

== Channel Basics
- MPSC = Multiple Producer, Single Consumer
- Thread-safe message queue
- Send values between threads
- Built into standard library

```rust
use std::sync::mpsc::channel;

let (tx, rx) = channel();

thread::spawn(move || {
   tx.send(42).unwrap();
});

println!("Got: {}", rx.recv().unwrap());
```

== Channel Types
- channel() - Unbounded async
- sync_channel(n) - Bounded sync
- Bounded channels block when full
- Choose based on backpressure needs

```rust
use std::sync::mpsc::{sync_channel, channel};

// Unbounded
let (tx1, rx1) = channel();

// Bounded to 5 messages
let (tx2, rx2) = sync_channel(5);

// Will block if buffer full
tx2.send(42).unwrap();
```

== Modern Channel Libraries
=== Flume
- Drop-in replacement for std::sync::mpsc
- Better performance
- Bounded/unbounded variants
- Select operation support

```rust
use flume::unbounded;

let (tx, rx) = unbounded();

thread::spawn(move || {
   tx.send("Hello from flume!").unwrap();
});

assert_eq!(rx.recv().unwrap(), "Hello from flume!");
```

=== Kanal
- High performance
- Async/sync support
- Multiple channel types
- Rich API

```rust
use kanal;

let (tx, rx) = kanal::bounded(10);

tx.send(42).unwrap();
assert_eq!(rx.recv().unwrap(), 42);
```

== Select Operations
- Wait on multiple channels
- First ready channel wins
- Available in flume and kanal
- Not in std::sync::mpsc

```rust
use flume::{bounded, select};

let (tx1, rx1) = bounded(1);
let (tx2, rx2) = bounded(1);

select! {
   recv(rx1) -> msg => println!("ch1: {:?}", msg),
   recv(rx2) -> msg => println!("ch2: {:?}", msg),
}
```

= Real World Patterns

== Producer-Consumer
- Common concurrent pattern
- Multiple producers feed queue
- Single consumer processes items

```rust
let (tx, rx) = flume::unbounded();
// Spawn multiple producers
for i in 0..3 {
   let tx = tx.clone();
   thread::spawn(move || {
       tx.send(format!("msg from {}", i)).unwrap();
   });
}
// Single consumer
for msg in rx.iter() {
   println!("Got: {}", msg);
}
```

== Work Stealing
- Dynamic task distribution
- Threads steal work when idle
- Good for variable workloads

```rust
use crossbeam_deque::{Worker, Stealer};
let w = Worker::new_fifo();
let s = w.stealer();
// Main thread produces work
w.push(42);
// Other thread steals work
thread::spawn(move || {
   while let Some(work) = s.steal() {
       println!("Stolen: {}", work);
   }
});
```

== Thread Pools
- Reuse threads instead of creating new
- Better performance
- Control system resource usage
- Common in real applications

```rust
use rayon::prelude::*;

let data: Vec<_> = (0..100).collect();

// Parallel iterator using thread pool
let sum: i32 = data.par_iter()
   .map(|x| x * x)
   .sum();
```

= Advanced Patterns

== Memory Ordering
- Atomic operations have ordering guarantees
- Different levels of synchronization
- Performance vs. strictness tradeoff

```rust
use std::sync::atomic::{AtomicBool, Ordering};

static STOP: AtomicBool = AtomicBool::new(false);

// Relaxed - fastest, weakest guarantees
STOP.store(true, Ordering::Relaxed);

// Release-Acquire - synchronizes data
STOP.store(true, Ordering::Release);
let must_stop = STOP.load(Ordering::Acquire);

// SeqCst - strongest, slowest
STOP.store(true, Ordering::SeqCst);
```

== Barrier Synchronization
- Coordinate multiple threads
- Wait for all threads to reach point
- Useful for phased computations

```rust
use std::sync::{Arc, Barrier};

let barrier = Arc::new(Barrier::new(3));

for _ in 0..3 {
   let b = Arc::clone(&barrier);
   thread::spawn(move || {
       println!("before barrier");
       b.wait();  // Wait for others
       println!("after barrier");
   });
}
```

== Lock-Free Data Structures
- No mutex required
- Uses atomic operations
- Higher performance potential
- More complex to implement

```rust
use crossbeam_queue::ArrayQueue;

let queue = ArrayQueue::new(100);

// Producer
queue.push(42).unwrap();

// Consumer
while let Some(item) = queue.pop() {
   println!("Got: {}", item);
}
```

== Custom Sync Types
- Implement Send and Sync traits
- Make your types thread-safe
- Careful with unsafe code

```rust
struct MyThreadSafeType {
   data: Arc<Mutex<Vec<i32>>>,
}

// Automatically Send + Sync because
// all fields are Send + Sync
impl MyThreadSafeType {
   fn push(&self, value: i32) {
       self.data.lock().unwrap().push(value);
   }
}
```

= Best Practices

== Choose Right Tool
- Channels for message passing
- Mutex for shared state
- Atomic for simple values
- Thread pools for CPU work

```rust
// Bad: Mutex for single counter
let counter = Arc::new(Mutex::new(0));

// Better: Atomic for counter
let counter = Arc::new(AtomicUsize::new(0));
```

== Error Handling
- Handle thread panics
- Propagate errors properly
- Use Result in channels

```rust
let handle = thread::spawn(|| {
   if let Err(e) = do_work() {
       eprintln!("Worker error: {}", e);
       return Err(e);
   }
   Ok(())
});

// Main thread handles error
if let Err(e) = handle.join().unwrap() {
   eprintln!("Thread failed: {}", e);
}
```
