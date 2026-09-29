#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => "",
  config-info(
    title: [Async Programming in Rust],
    author: [Lukáš Hozda, Luukas Portfors],
    date: datetime.today(),
    logo: emoji.gear,
    institution: [Braiins Systems s.r.o],
  ),
)

#title-slide()

= Foundations

== Why Async?
- Threads have a pretty high overhead #pause
  - Especially if you were to spawn them for short ephemeral tasks #pause
- Context switches are expensive #pause
- Many I/O-bound tasks wait most of the time #pause
- Async: cooperative multitasking within threads #pause
- Single thread can handle thousands of connections #pause
- A lot of programs spend most of the time waiting for IO #pause
  - Which is why businesses can thrive on Python and Ruby lol

== The Core Idea
- Don't block the thread while waiting #pause
- Yield control, let other tasks run (cooperative multitasking) #pause
- Resume when ready #pause
- All within same thread (or few threads)

```rust
// blocking - thread does nothing while waiting
let data = std::fs::read("file.txt")?;

// async - thread can do other work
let data = tokio::fs::read("file.txt").await?;
```

== Future Trait
- Core abstraction of async Rust #pause
- Represents a value that may not exist yet #pause
- Must be polled to make progress

```rust
pub trait Future {
    type Output;
    
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) 
        -> Poll<Self::Output>;
}

pub enum Poll<T> {
    Ready(T),
    Pending,
}
```

== State Machine Transformation
- Compiler transforms async fn into state machine #pause
- Each `.await` is a yield point #pause
- State stored in the `Future` itself #pause

```rust
async fn example() -> u32 {
    let a = step_one().await;
    let b = step_two(a).await;
    a + b
}
// compiler generates enum with states:
// - Start
// - AfterStepOne { a: u32 }
// - Complete
```

== Lazy Futures
- Futures in Rust do nothing until polled #pause
  - Luckily we get a helpful error, same as iterators #pause
- Creating a future doesn't start work #pause
- This differs from JavaScript/C\# promises, which start on creation #pause

```rust
async fn greet() {
    println!("Hello!");
}

fn main() {
    let future = greet(); // nothing happens
    drop(future);         // still nothing printed
}
```

Dropping futurres is how we cancel them, but not all futures are cancellation safe, more on that later...

= Syntax and Usage

== async fn
- Returns `impl Future<Output = T>` #pause
- Body can use .await #pause
- Cannot be called from sync context directly

```rust
async fn fetch_data(url: &str) -> Result<String, Error> {
    let response = client.get(url).send().await?;
    let text = response.text().await?;
    Ok(text)
}
```

== async Blocks
- Create futures inline #pause
- Capture variables from scope #pause
- Useful for closures and combinators

```rust
let name = "world".to_string();

let future = async {
    println!("Hello, {}!", name);
    42
};

let result = future.await; // 42
```

== async move
- Move captured variables into future #pause
- Same semantics as move closures

```rust
let data = vec![1, 2, 3];

let future = async move {
    // data is moved here
    println!("{:?}", data);
};

// data no longer accessible here
```

== The .await Syntax
- Suspends current async fn #pause
- Yields to executor #pause
- Resumes when inner future ready #pause
- Only valid inside async context

```rust
async fn process() {
    let a = fetch_a().await;  // suspend here
    let b = fetch_b().await;  // then here
    combine(a, b)
}
```

= Runtimes and Executors

== Runtime
- Polls futures to completion #pause
- Manages task scheduling #pause
- Provides async I/O primitives #pause
- Not part of std - external crate

== Tokio
- Most popular async runtime #pause
- It's what we use in Braiins for everything #pause
- Multi-threaded by default #pause
- Full-featured: timers, I/O, sync primitives... #pause

```rust
#[tokio::main]
async fn main() {
    println!("Running on tokio!");
    tokio::time::sleep(Duration::from_secs(1)).await;
}
```

== Tokio Runtime Options
- Multi-threaded: `#[tokio::main]` #pause
- Single-threaded: `#[tokio::main(flavor = "current_thread")]` #pause
- Manual construction for control

```rust
let rt = tokio::runtime::Builder::new_multi_thread()
    .worker_threads(4)
    .enable_all()
    .build()
    .unwrap();

rt.block_on(async {
    // your async code
});
```

== async-std
- API mirrors std library #pause
- Simpler than tokio #pause
- No. 2 top used framework

```rust
use async_std::task;
use async_std::fs;

fn main() {
    task::block_on(async {
        let contents = fs::read_to_string("file.txt").await?;
        Ok::<_, std::io::Error>(())
    });
}
```

== smol
- Minimal async runtime, somewhat Tokio compatible #pause
- Best name award #pause
- Composable components #pause
- Good for understanding internals

```rust
fn main() -> std::io::Result<()> {
    smol::block_on(async {
        smol::Timer::after(Duration::from_secs(1)).await;
        println!("Done!");
        Ok(())
    })
}
```

== Executor-Agnostic Code
- futures crate provides common abstractions #pause
- Some ecosystem fragmentation exists #pause
  - Notably tokio world vs async-std world

```rust
// works with any runtime
async fn process<R: AsyncRead + Unpin>(reader: R) {
    // ...
}
```

= Spawning Tasks

== Task vs Future
- Future: inert, needs polling #pause
- Task: future submitted to runtime #pause
- Tasks run independently, concurrently #pause
- General pattern: Get input for a request, shove it into a task you spawn, letting it
  handle the request concurrently, while you already wait for a new request

```rust
#[tokio::main]
async fn main() {
    let handle = tokio::spawn(async {
        // runs concurrently
        expensive_computation().await
    });
    
    let result = handle.await.unwrap();
}
```

== spawn Requirements
- Future must be Send (usually, unless your runtime is strictly single-thread) #pause
- Future must be 'static #pause

```rust
let data = Arc::new(vec![1, 2, 3]);
let data_clone = Arc::clone(&data);

tokio::spawn(async move {
    // must own or Arc-share data
    println!("{:?}", data_clone);
});
```

== spawn_local
- For !Send futures #pause
- Runs on current thread only #pause
- Useful for thread-local data

```rust
use tokio::task::spawn_local;

let local_data = Rc::new(42); // Rc is !Send

let local = tokio::task::LocalSet::new();
local.run_until(async {
    spawn_local(async move {
        println!("{}", local_data);
    }).await.unwrap();
}).await;
```

== spawn_blocking
- Run blocking code without blocking executor #pause
- Offloads to separate thread pool #pause
- Essential for CPU-heavy or legacy sync code

```rust
let result = tokio::task::spawn_blocking(|| {
    // this would block the async runtime
    std::thread::sleep(Duration::from_secs(5));
    heavy_cpu_work()
}).await.unwrap();
```

== JoinHandle
- Handle to spawned task #pause
- Can await for result #pause
- Can abort task

== JoinHandle
```rust
let handle = tokio::spawn(async {
    loop {
        do_work().await;
    }
});

// later...
handle.abort();

// or wait for completion
match handle.await {
    Ok(result) => println!("Done: {:?}", result),
    Err(e) if e.is_cancelled() => println!("Aborted"),
    Err(e) => println!("Panicked: {:?}", e),
}
```

= Concurrent Operations

== join! Macro
- Run multiple futures concurrently #pause
- Wait for all to complete #pause
- Returns tuple of results

```rust
let (user, posts, comments) = tokio::join!(
    fetch_user(id),
    fetch_posts(id),
    fetch_comments(id)
);
```

== try_join!
- Like join!, but short-circuits on error #pause
- Returns Result #pause
- Remaining futures cancelled on error

```rust
let result = tokio::try_join!(
    fetch_user(id),
    fetch_posts(id),
    fetch_comments(id)
);

match result {
    Ok((user, posts, comments)) => { /* all succeeded */ }
    Err(e) => { /* at least one failed */ }
}
```

== select! Macro
- Race multiple futures #pause
- First to complete wins #pause
- Others are dropped (cancelled)

```rust
tokio::select! {
    result = fetch_data() => {
        println!("Got data: {:?}", result);
    }
    _ = tokio::time::sleep(Duration::from_secs(5)) => {
        println!("Timeout!");
    }
}
```

== select! with Loops
- Common pattern for event loops #pause
- biased; for deterministic priority

```rust
loop {
    tokio::select! {
        biased; // check in order
        
        _ = shutdown.recv() => {
            break;
        }
        msg = receiver.recv() => {
            handle(msg).await;
        }
        _ = interval.tick() => {
            heartbeat().await;
        }
    }
}
```

= Pin and Self-Referential Futures

== The Problem
- Async state machines can be self-referential #pause
- Moving self-referential data invalidates pointers #pause
- Rust must prevent this

```rust
async fn example() {
    let data = vec![1, 2, 3];
    let reference = &data[0]; // points into data
    yield_point().await;       // state saved
    println!("{}", reference); // reference must still be valid
}
```

== Pin\<P\>
- Wrapper that guarantees pointee won't move #pause
- Required for polling futures #pause
- Most users don't interact directly

```rust
pub trait Future {
    type Output;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) 
        -> Poll<Self::Output>;
    //       ^^^^ pinned!
}
```

== Unpin Trait
- Marker trait: "safe to move even when pinned" #pause
- Most types are Unpin #pause
- Self-referential types are !Unpin #pause
- async fn returns are !Unpin

```rust
// these are Unpin - no self-references
struct Simple { x: i32 }          // Unpin
struct WithVec { v: Vec<i32> }    // Unpin

// compiler-generated futures are !Unpin
let future = async { };           // !Unpin
```

== Box::pin
- Heap allocation makes pinning easy #pause
- Common way to store futures

```rust
use std::pin::Pin;
use std::future::Future;

// store arbitrary futures
let future: Pin<Box<dyn Future<Output = i32>>> = 
    Box::pin(async { 42 });
```

== pin! Macro
- Stack pinning without allocation #pause
- From tokio or std (nightly)

```rust
use tokio::pin;

async fn example() {
    let future = some_async_fn();
    pin!(future);
    
    // future is now Pin<&mut impl Future>
    // can be polled, used in select!, etc.
}
```

== When You Need Pin
- Storing futures in structs #pause
- Implementing Future manually #pause
- Using select! with named futures
  -

```rust
struct MyStruct {
    // must box for object safety
    pending: Option<Pin<Box<dyn Future<Output = i32> + Send>>>,
}
```

= Async Traits

== Historical Problem
- async fn in traits was hard #pause
- Return type is anonymous impl Future #pause
- Traits need concrete types or dyn

```rust
// didn't work before Rust 1.75
trait Service {
    async fn call(&self) -> Response;
}
```

== Native async fn in Traits (1.75+)
- Now works directly, yeehaw #pause
- Some limitations with dyn #pause
- Send bounds can be tricky

```rust
trait DataStore {
    async fn get(&self, key: &str) -> Option<Vec<u8>>;
    async fn set(&self, key: &str, value: Vec<u8>);
}

impl DataStore for MyStore {
    async fn get(&self, key: &str) -> Option<Vec<u8>> {
        self.inner.get(key).await
    }
    // ...
}
```

== async_trait Crate (Legacy...?)
- Pre-1.75 solution #pause
- Uses `Box<dyn Future>` internally #pause
- Still useful for dyn compatibility

```rust
use async_trait::async_trait;

#[async_trait]
trait Handler {
    async fn handle(&self, req: Request) -> Response;
}

#[async_trait]
impl Handler for MyHandler {
    async fn handle(&self, req: Request) -> Response {
        // ...
    }
}
```

= Streams

== What is a Stream?
- Async iterator #pause
- Yields multiple values over time #pause
- Core abstraction for async sequences

```rust
pub trait Stream {
    type Item;
    
    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>)
        -> Poll<Option<Self::Item>>;
}
```

== Using Streams
- StreamExt provides combinators #pause
- Similar to Iterator methods

```rust
use tokio_stream::StreamExt;

let mut stream = tokio_stream::iter(vec![1, 2, 3]);

while let Some(value) = stream.next().await {
    println!("{}", value);
}
```

== Stream Combinators
```rust
use tokio_stream::StreamExt;

let stream = tokio_stream::iter(1..=10);

let result: Vec<_> = stream
    .filter(|x| x % 2 == 0)
    .map(|x| x * 2)
    .take(3)
    .collect()
    .await;
// [4, 8, 12]
```

== async_stream Crate
- Create streams with async/await syntax #pause
- Much more ergonomic than manual impl

```rust
use async_stream::stream;

fn countdown(from: u32) -> impl Stream<Item = u32> {
    stream! {
        for i in (0..=from).rev() {
            tokio::time::sleep(Duration::from_secs(1)).await;
            yield i;
        }
    }
}
```

== Channel as Stream
- Receivers implement Stream #pause
- Natural fit for message processing

```rust
let (tx, rx) = tokio::sync::mpsc::channel(32);

// producer
tokio::spawn(async move {
    for i in 0..10 {
        tx.send(i).await.unwrap();
    }
});

// consumer using stream
let mut stream = tokio_stream::wrappers::ReceiverStream::new(rx);
while let Some(value) = stream.next().await {
    process(value).await;
}
```

= Async Synchronization

== Why Different Primitives?
- std::sync::Mutex blocks the thread #pause
- Async mutex yields to executor #pause
- Holding std mutex across await is problematic

```rust
// bad - blocks executor thread
let guard = std_mutex.lock().unwrap();
some_async_fn().await; // other tasks can't run!

// good - yields while waiting
let guard = async_mutex.lock().await;
some_async_fn().await; // other tasks run fine
```

== tokio::sync::Mutex
- Async-aware mutex #pause
- lock() returns a future #pause
- Fair scheduling

```rust
use tokio::sync::Mutex;

let mutex = Arc::new(Mutex::new(0));

let m = mutex.clone();
tokio::spawn(async move {
    let mut lock = m.lock().await;
    *lock += 1;
});
```

== When to Use std Mutex
- Lock held for short time #pause
- No await while holding lock #pause
- Better performance for quick operations

```rust
use std::sync::Mutex;

// fine - no await while locked
let data = {
    let guard = mutex.lock().unwrap();
    guard.clone()
}; // lock released before await

process(data).await;
```

= Cancellation

== Drop = Cancel
- Dropping a future cancels it #pause
- No special cancellation token needed #pause
- Code after await point doesn't run

```rust
let future = async {
    step_one().await;
    step_two().await; // might never run
    step_three().await;
};

drop(future); // cancelled
```

== Cancellation Safety
- Some operations are not cancellation-safe #pause
- Partial completion can leave inconsistent state #pause
- select! cancels losing branches

```rust
// not cancellation safe!
async fn read_exact(buf: &mut [u8]) {
    let mut filled = 0;
    while filled < buf.len() {
        // if cancelled here, partial data lost
        filled += read_some(&mut buf[filled..]).await;
    }
}
```

== CancellationToken
- Explicit cancellation signal #pause
- Useful for graceful shutdown

```rust
use tokio_util::sync::CancellationToken;

let token = CancellationToken::new();

let t = token.clone();
tokio::spawn(async move {
    loop {
        tokio::select! {
            _ = t.cancelled() => break,
            _ = do_work() => {}
        }
    }
});

token.cancel(); // signal shutdown
```

= Async I/O

== AsyncRead and AsyncWrite
- Async versions of std::io traits #pause
- Non-blocking I/O operations

```rust
use tokio::io::{AsyncReadExt, AsyncWriteExt};

async fn copy_data(
    reader: &mut (impl AsyncRead + Unpin),
    writer: &mut (impl AsyncWrite + Unpin),
) -> std::io::Result<()> {
    let mut buf = [0u8; 1024];
    loop {
        let n = reader.read(&mut buf).await?;
        if n == 0 { break; }
        writer.write_all(&buf[..n]).await?;
    }
    Ok(())
}
```

== TCP Example
```rust
use tokio::net::{TcpListener, TcpStream};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

async fn handle(mut socket: TcpStream) {
    let mut buf = [0u8; 1024];
    loop {
        let n = socket.read(&mut buf).await.unwrap();
        if n == 0 { return; }
        socket.write_all(&buf[..n]).await.unwrap();
    }
}

#[tokio::main]
async fn main() {
    let listener = TcpListener::bind("127.0.0.1:8080").await.unwrap();
    loop {
        let (socket, _) = listener.accept().await.unwrap();
        tokio::spawn(handle(socket));
    }
}
```

== Timeouts
```rust
use tokio::time::{timeout, Duration};

match timeout(Duration::from_secs(5), fetch_data()).await {
    Ok(result) => println!("Got: {:?}", result),
    Err(_) => println!("Timed out!"),
}
```

= Problems

== Blocking the Executor
```rust
// bad - blocks the whole runtime thread
async fn bad() {
    std::thread::sleep(Duration::from_secs(10));
    std::fs::read_to_string("file.txt"); // blocking!
}

// good - use async versions or spawn_blocking
async fn good() {
    tokio::time::sleep(Duration::from_secs(10)).await;
    tokio::fs::read_to_string("file.txt").await;
}
```

== Holding Locks Across Await
```rust
// bad - lock held across await, other tasks starve
let guard = mutex.lock().await;
slow_operation().await;
drop(guard);

// good - release lock before await
let data = {
    let guard = mutex.lock().await;
    guard.clone()
};
slow_operation_with(data).await;
```

== Large Futures
- Each await point adds to future size #pause
- Deeply nested async can create huge futures #pause
- Box large futures if needed #pause
  - Necessary if you for some reason need async recursion (generally a bad idea)

```rust
// can result in large future
async fn deep() {
    a().await;
    b().await;
    c().await;
    // ... 100 more awaits
}

// if size is problem, box it
let future: Pin<Box<_>> = Box::pin(deep());
```

== Forgetting to Spawn
```rust
// wrong - these run sequentially!
async fn wrong() {
    let a = fetch_a(); // not spawned
    let b = fetch_b(); // not spawned
    a.await;
    b.await;
}

// right - concurrent execution
async fn right() {
    let (a, b) = tokio::join!(fetch_a(), fetch_b());
}
```

== Send Bounds in Async
- Spawned futures must be Send #pause
- Holding !Send values across await breaks this
- The errors from the compiler used to be quite bad, but
  improved recently

```rust
// won't compile - Rc is !Send
async fn bad() {
    let rc = Rc::new(42);
    some_async_fn().await; // rc held across await
    println!("{}", rc);
}

// use Arc instead
async fn good() {
    let arc = Arc::new(42);
    some_async_fn().await;
    println!("{}", arc);
}
```
