#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => "",
  config-info(
    title: [Rust generics],
    subtitle: [],
    author: [Lukáš Hozda],
    date: datetime.today(),
    logo: emoji.peach,
    institution: [Braiins Systems s.r.o],
  ),
)

#title-slide()

= Generics and Traits in Rust

== What are Generics?
- Allows writing code that works with multiple types
- Similar to templates in C++
- Zero-cost abstraction
- Compiler generates specialized code for each type

```rust
// Generic function
fn identity<T>(x: T) -> T {
    x
}

// Generic struct
struct Point<T> {
    x: T,
    y: T,
}
```

== Generic Constraints
- Traits define behavior for types
- Constrain generic types with trait bounds
- Specify what capabilities a type must have

```rust
// T must implement Display trait
fn print_value<T: Display>(value: T) {
    println!("{}", value);
}

// Multiple trait bounds
fn compare<T: PartialOrd + Clone>(a: T, b: T) -> T {
    if a > b { a } else { b }
}
```

== Trait Objects
- Dynamic dispatch - available for object-safe traits
- Runtime polymorphism
- Use `dyn` keyword
- Allows storing different types in same collection

```rust
trait Animal {
    fn make_sound(&self);
}

fn animal_sounds(animals: &[Box<dyn Animal>]) {
    for animal in animals {
        animal.make_sound();
    }
}
```

== Default Trait Implementations
- Traits can provide default method implementations
- Types can override or use default behavior

```rust
trait Logger {
    fn log(&self, message: &str) {
        println!("Default log: {}", message);
    }
}

struct FileLogger;
impl Logger for FileLogger {
    // Uses default implementation
}
```

== Derive Macros
- Automatically implement common traits
- Reduces boilerplate code
- Many standard library traits supported

```rust
#[derive(Debug, Clone, PartialEq)]
struct Person {
    name: String,
    age: u32,
}
\```

== Generic Associated Types
- Allows defining associated types in traits
- Provides more flexibility in generic code

```rust
trait Container {
    type Item;
    fn contains(&self, item: &Self::Item) -> bool;
}

impl Container for Vec<i32> {
    type Item = i32;
    fn contains(&self, item: &i32) -> bool {
        self.contains(item)
    }
}
```

== Trait Bounds in Method Definitions
- Constrain method type parameters
- Ensure methods only work with types that meet requirements

```rust
impl<T: Display + Clone> MyStruct<T> {
    fn print_value(&self, value: T) {
        println!("{}", value.clone());
    }
}
```

== Where Clauses
- Alternative syntax for trait bounds
- More readable for complex constraints
- Can refer to anything, not just Self or params

```rust
fn process<T>(value: T)
where
    T: Display + Clone,
    T: PartialOrd
{
    // Method body
}
```

= Object Safety in Rust

== What is Object Safety?
- Defines which traits can be used with trait objects
- Enables dynamic dispatch
- Requires specific rules to be followed

== Object Safety Rules
- Method must not have any type parameters
- Method must not use `Self` type except as return value
- Method must not have static method requirements
- We can use the Self: Sized bound to include methods that don't  match above requirements

==

```rust
// Not object safe
trait NotSafe<T> {
    fn generic_method<U>(x: T, y: U);
    fn uses_self(self: Self) -> Self;
}

// Object safe
trait Safe {
    fn concrete_method(&self, x: i32);
    fn returns_self(&self) -> Box<dyn Safe>;
}
```

== Example of Object Safety

```rust
// this works
trait Animal {
    fn make_sound(&self);
}

// this doesn't
trait Comparable<T> {
    fn compare(&self, other: &T) -> bool;
}

// can create trait objects for Animal
let animals: Vec<Box<dyn Animal>> = vec![];
```
