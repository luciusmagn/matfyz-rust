#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#import "@preview/numbly:0.1.0": numbly

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => self.info.institution,
  config-info(
    title: [Objects],
    subtitle: [Ownership and life cycle],
    author: [Lukáš Hozda, Luukas Pörtfors],
    date: datetime.today(),
    logo: emoji.crab,
  ),
)

#set heading(numbering: numbly("{1}.", default: "1.1"))

#title-slide()

= Everything is an Expression (Redux)

== Remember: Expressions Everywhere

- Last time we saw `if`, `match`, loops as expressions
- Blocks are expressions too!


```rust
let x = {
    let a = compute_something();
    let b = compute_more();
    a + b  // last expression is the block's value

};

// Useful for scoping and temporary calculations
let result = {
    let temp = expensive_operation();
    process(temp) // temp is dropped here
};
```


== Block Labels

- Blocks can have labels for early returns
- Not just loops - any block can be labeled
- Useful for complex initialization logic

```rust
let value = 'init: {
    if !precondition() {
        break 'init DEFAULT_VALUE;
    }

    
    let data = load_data()?;
    if data.is_empty() {
        break 'init EMPTY_VALUE;
    }

    process(data) // normal return
};

```

= Object Construction

== Struct Construction

- Fields can be initialized directly
- Update syntax (`..`) for partial changes
- Field init shorthand when variable matches field name

```rust
struct Point {
    x: i32,
    y: i32,
}

let x = 5;

let p = Point { x, y: 10 }; // x: x shorthand
let p2 = Point { x: 3, ..p }; // take other fields from p
```

== Builder Pattern
- Rust does not have default parameters and method overloading
- For complex objects with many optional fields
- Alternative to having 20 different constructors
- Provides fluent API for configuration

```rust
let server = Server::builder()
    .host("localhost")
    .port(8080)
    .workers(4)
    .timeout(Duration::from_secs(30))
    .build();
```

== Builder Implementation


```rust
struct Server {
    host: String,
    port: u16,
    workers: u32,
    timeout: Duration,
}

impl Server {
    fn builder() -> ServerBuilder {
        ServerBuilder::default()
    }

}
```

==

```rust
#[derive(Default)]
struct ServerBuilder {
    host: Option<String>,
    port: Option<u16>,
    workers: Option<u32>,
    timeout: Option<Duration>,
}
```

==

```rust
impl ServerBuilder {
    fn host(mut self, host: impl Into<String>) -> Self {
        self.host = Some(host.into());
        self
    }

    fn port(mut self, port: u16) -> Self {
        self.port = Some(port);
        self
    }

    // ... other setters
}

```

==

```rust
impl ServerBuilder {
    fn build(self) -> Result<Server, BuildError> {
        Ok(Server {
            host: self.host.ok_or(BuildError::MissingHost)?,
            port: self.port.unwrap_or(8080),
            workers: self.workers.unwrap_or(4),
            timeout: self.timeout.unwrap_or(Duration::from_secs(30)),
        })
    }
}
```


= Ownership

== Ownership rules
1. Each value has exactly one owner
2. When the owner goes out of scope, the value is dropped
   - Drop is Rust vernacular for deleted/deallocated/destroyed
3. There can be many immutable borrows OR one mutable borrow
   - But never both at the same time!

```rust
{
    let s = String::from("hello"); // s owns the String
    // ... use s
} // s goes out of scope, String is dropped
```

== Stack vs Heap
- Stack: Fixed size, fast, LIFO
  - Primitives, arrays, tuples live here
  - The default in Rust 
- Heap: Dynamic size, slower, arbitrary access
  - String, Vec, Box
  

```rust
// Stack allocated
let x = 42;
let point = (1, 2);
let array = [1, 2, 3];

// Heap allocated (with stack pointer)
let s = String::from("hello");
let v = vec![1, 2, 3];
let boxed = Box::new(42);
```


== Moving: Transfer of Ownership
- Assignment moves ownership for non-Copy types
- Previous owner can't be used anymore
- Prevents use-after-free at compile time

```rust
let s1 = String::from("hello");
let s2 = s1; // s1's ownership moved to s2

// println!("{}", s1); // ERROR: value moved

let s3 = s2.clone(); // explicit deep copy
println!("{} {}", s2, s3); // OK: s2 still owns its data
```


== What Gets Moved?

```rust
fn take_ownership(s: String) {
    println!("{}", s);
} // s is dropped here

fn main() {
    let s = String::from("hello");
    take_ownership(s);
    // println!("{}", s); // ERROR: s was moved
    
    let x = 5;
    makes_copy(x);
    println!("{}", x); // OK: i32 is Copy
}

fn makes_copy(n: i32) {
    println!("{}", n);
}
```


== Copy vs Clone

- `Copy`: Implicit, cheap, bitwise copy
  - Stack-only types
  - Only allowed for types that do not implement Drop
    - Or do not contain things that implement Drop
  - All primitives, tuples of Copy types
- `Clone`: Explicit, potentially expensive
  - Deep copy of heap data
  - Must call `.clone()` explicitly

```rust
// Types that are Copy
let x = 5;
let y = x; // copied automatically

// Types that need Clone
let s1 = String::from("hello");
let s2 = s1.clone(); // explicit clone required

// you can derive both
#[derive(Copy, Clone)]
struct Point { x: i32, y: i32 }
```


== Drop

- `Drop` trait runs destructor code
- Called automatically when owner goes out of scope
- RAII pattern - Resource Acquisition Is Initialization

```rust
struct FileWrapper {
    handle: File,
}

impl Drop for FileWrapper {
    fn drop(&mut self) {
        println!("Closing file!");
        // cleanup code here
    }
}
```

==
```rust
{
    let f = FileWrapper { handle: File::open("foo.txt")? };
    // use file
} // drop() called automatically here
```

= References


== Borrowing Basics
- `&T` - immutable reference (read-only)
  - Also called shared reference
- `&mut T` - mutable reference (read-write)
  - Also called exclusive reference
- References don't take ownership
- Stacked borrows model (check out the paper)

```rust
fn calculate_length(s: &String) -> usize {
    s.len()
} // s goes out of scope but doesn't drop anything

fn main() {
    let s1 = String::from("hello");
    let len = calculate_length(&s1); // borrow s1
    println!("Length of '{}' is {}", s1, len); // can still use s1!
}
```

== Mutable References
- Only one mutable reference at a time, while no other reference exists
- Cannot have immutable refs while mutable ref exists
- Prevents data races at compile time

```rust
fn append_world(s: &mut String) {
    s.push_str(", world!");
}

let mut s = String::from("hello");
append_world(&mut s);
println!("{}", s); // "hello, world!"
```
==


```rust
let mut s = String::from("hello");
let r1 = &s; // OK
let r2 = &s; // OK
// let r3 = &mut s; // ERROR: cannot borrow as mutable

println!("{} {}", r1, r2); // last use of immutable refs
let r3 = &mut s; // OK: r1, r2 no longer used
r3.push_str(" world");
```


== The Borrow Checker
- Ensures references are valid
- No dangling pointers, no use-after-free
- Enemy of new Rust programmers


```rust
fn dangle() -> &String { // ERROR!
    let s = String::from("hello");
    &s // s is dropped, reference would dangle
}

fn no_dangle() -> String { // OK
    let s = String::from("hello");
    s // ownership moved out
}
```


== Lifetimes
- Every reference has a lifetime
- Usually inferred by compiler
- Sometimes need explicit annotation with `'a`
  - We use this annotations to tell the compiler about relationships between references and types

```rust
// Lifetime elision - compiler figures it out
fn first_word(s: &str) -> &str {
    &s[..s.find(' ').unwrap_or(s.len())]
}

// Explicit lifetimes needed here
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```


== Lifetime Annotations in Structs

- Structs storing references need lifetime parameters

- Ensures struct doesn't outlive its references


```rust
struct ImportantExcerpt<'a> {
    part: &'a str,
}

impl<'a> ImportantExcerpt<'a> {
    fn level(&self) -> i32 {
        3
    }
}

let novel = String::from("Call me Ishmael. Some years ago...");
let first = novel.split('.').next().expect("Could not find '.'");
let i = ImportantExcerpt { part: first };
```


== Static Lifetime

- `'static` means reference lives for entire program
- String literals have `'static` lifetime
- Can be coerced to shorter lifetimes
- Non-borrowed types also coerce to static


```rust
let s: &'static str = "I have a static lifetime.";
fn returns_static() -> &'static str {
    "This string lives forever!"
}

// Common with error messages
impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "An error occurred") // string literal is 'static
    }
}
```


= Smart Pointers


== Box: Heap Allocation

- Allocates values on the heap
- Stack contains pointer to heap data
- Zero-cost abstraction over raw pointers


```rust
let b = Box::new(5);
println!("b = {}", b); // deref coercion

// fequired for recursive types
enum List {
    Cons(i32, Box<List>),
    Nil,
}
```

```
let list = List::Cons(1, 
    Box::new(List::Cons(2, 
        Box::new(List::Nil))));
```


== Box usecases
- Recursive types (unknown size at compile time)
- Large data to avoid stack copies
- Trait objects (dynamic dispatch)


```rust
// nuh uh - infinitely sized type
enum List {
     Cons(i32, List), // how big is List?
     Nil,
}
```


== Deref Coercion
- Smart pointers automatically dereference
- Can call methods directly on Box<T>
- Implements `Deref` trait


```rust
let x = Box::new(String::from("hello"));
// These are equivalent
let len1 = (*x).len();
let len2 = x.len(); // deref coercion

fn hello(name: &str) {
    println!("Hello, {}!", name);
}

let boxed_str = Box::new(String::from("Rust"));
hello(&boxed_str); // Box<String> -> String -> &str
```


= Performance: Aliasing and Optimization


== No Aliasing = Better Optimization
- Rust's borrowing rules prevent aliasing
- Compiler can optimize more aggressively
- Similar to `restrict` in C


```rust
// Compiler knows a and b don't alias
fn add_twice(a: &mut i32, b: &i32) {
    *a += *b;
    *a += *b; // can optimize to *a += 2 * *b
}

// In C/C++ without restrict, compiler must assume
// a and b might point to same location
```

==


```rs
fn example(a: &i32, b: &mut i32) {
    if *a > 10 {
        *b += 1;
    }
    if *a > 20 {
        *b += 2;
    }
}
```

== to 

```rs
fn example(a: &i32, b: &mut i32) {
    let a_val = *a;  // load just once
    if a_val > 20 {
        *b += 3;     // combines both increments
    } else if a_val > 10 {
        *b += 1;
    }
}
```

== Zero Cost Abstractions
- "What you don't use, you don't pay for"
- Abstractions compile to optimal code
- Box, references, iterators - all zero-cost

==
..
```rust
// This high-level code
let sum: i32 = vec![1, 2, 3, 4, 5]
    .iter()
    .map(|x| x * 2)
    .filter(|x| x > &5)
    .sum();

// Compiles to same assembly as hand-written loop
let mut sum = 0;
for x in [1, 2, 3, 4, 5] {
    let doubled = x * 2;
    if doubled > 5 {
        sum += doubled;
    }
}
```