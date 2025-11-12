#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => self.info.institution,
  config-info(
    title: [Data Structures],
    author: [Lukáš Hozda, Luukas Pörtfors],
    date: datetime.today(),
    logo: emoji.crab,
  ),
)

#title-slide()

= Basic Control Flow

== if and else
- Conditions must be `bool` type only
- It's an expression
  - You will hear this often because Rust is an expression language

```rust
let number = 3;

if number < 5 {
    println!("less than 5");
} else if number == 5 {
    println!("equal to 5");
} else {
    println!("greater than 5");
}

// as expression
let status = if number > 0 { "positive" } else { "non-positive" };
```

== Loops - loop
- Infinite loop unless explicitly broken
- Can return values with `break`
  - It's an expression!
- Can have labels for nested loops

```rust
let mut counter = 0;
let result = loop {
    counter += 1;
    if counter == 10 {
        break counter * 2;
    }
}; // result is 20

```
==
```rust
'outer: loop {
    'inner: loop {
        break 'outer; // breaks outer loop
    }
}
```

== Loops - while
- Conditional loop
- Also an expression, but not particularly useful (always evaluates to `()`)

```rust
let mut number = 3;

while number != 0 {
    println!("{}", number);
    number -= 1;
}
```

== Loops - for
- Iterate over collections
- Most idiomatic for many cases
- Works with iterators (more on that soon(tm)!)

```rust
let a = [10, 20, 30, 40, 50];

for element in a {
    println!("value: {}", element);
}

for number in 1..4 {
    println!("{}", number); // 1, 2, 3
}
```

= Patterns

== Patterns
- A pattern is a *shape* or *template* we match against data
- Essentially stencils - describe structure and extract parts
- Patterns appear in far too many places:
  - `let` statements
  - Function parameters
  - `match` arms
  - `if let`, `while let`, `for` loops

```rust
let (x, y) = (1, 2);

let Some(value) = optional;

let z = 12;
```

== Irrefutable vs Refutable
- *Irrefutable patterns* will match any value of the type
  - E.g. every pair can be decomposed into two elements
- *Refutable patterns* might fail to match some values of a type
  - Not every integer pair has 2 as the first element

```rust
// irrefutable
let (x, y) = (1, 2);

// refutable
if let Some(x) = val {
    // use x
}
```

== Pattern examples

```rust
let Point { x, y } = point;

fn print_coordinates(&Point { x, y }: &Point) {
    println!("({}, {})", x, y);
}

match value {
    0 => println!("zero"),
    n => println!("number: {}", n),
}

for (index, value) in vec.iter().enumerate() {
    println!("{}: {}", index, value);
}
```

= Basic Data Structures

== Tuples
- Fixed size, heterogeneous collections
- Access by position or destructuring
- Unit type `()` is empty tuple
  - As we mentioned last lesson, it serves the role of *void* in other langs

==
```rust
let tup: (i32, f64, u8) = (1337, 420.0, 1);

// destructuring
let (x, y, z) = tup;

// positional access
let first = tup.0;
let second = tup.1;

// pattern matching
match tup {
    (0, y, z) => println!("first element is zero"),
    (x, 0.0, z) => println!("second element is zero"),
    _ => println!("catchall"),
}
```

== Arrays
- Fixed size, homogeneous collections
- Stack allocated
- Type includes size: `[T; N]`

```rust
let arr: [i32; 5] = [1, 2, 3, 4, 5];

// repetition syntax
let zeros = [0; 100]; // 100 zeros

let arr = [1, 2, 3];
match arr {
    [1, second, third] => println!("Starts with 1"),
    [first, 2, third] => println!("Middle is 2"),
    _ => println!("Something else"),
}
```

== Slices
- Views into contiguous sequences
  - Can be a view into arrays, vecs, other slices...
- Don't own the data
- Size not known at compile time

```rust
let arr = [1, 2, 3, 4, 5];

// create slice from array, exclusive range
let slice: &[i32] = &arr[1..3]; // [2, 3]

// pattern matching slices
let slice = &[1, 2, 3, 4];
match slice {
    [] => println!("Empty"),
    [single] => println!("One element: {}", single),
    [first, rest @ ..] => println!("First: {}, rest: {:?}", first, rest),
}
```

= Structures

#focus-slide()[
  #image("structs-good.jpg", width: 100%)
]

== Struct Definition
- Named fields with types
- Create custom types
- Can derive common traits

```rust
struct User {
    username: String,
    email: String,
    sign_in_count: u64,
    active: bool,
}

let user1 = User {
    email: String::from("user@example.com"),
    username: String::from("someusername"),
    active: true,
    sign_in_count: 1,
};
```

== Field Init Shorthand
- Use variable names as field names
- Update syntax with `..`

```rust
fn build_user(email: String, username: String) -> User {
    User {
        email,     // shorthand for email: email
        username,  // shorthand for username: username
        active: true,
        sign_in_count: 1,
    }
}

// Struct update syntax
let user2 = User {
    email: String::from("another\@example.com"),
    ..user1  // use remaining fields from user1
};
```

== Tuple Structs
- Structs without named fields
- Useful for simple wrappers
  - Used fairly often in the *newtype* pattern
- Create distinct types

```rust
struct Color(i32, i32, i32);
struct Point(i32, i32, i32);

let black = Color(0, 0, 0);
let origin = Point(0, 0, 0);

let Color(r, g, b) = black;
```

== Pattern Matching Structs

```rust
struct Point {
    x: i32,
    y: i32,
}

let p = Point { x: 0, y: 7 };

// destructuring
let Point { x, y } = p;

// partial destructuring with ..
let Point { x, .. } = p;

// Rrnaming
let Point { x: x_coord, y: y_coord } = p;
```

= Enumerations

== Basic Enums
- Type that can be one of several variants
- More powerful than enums in C et al.
  - C/C++/C\# essentially has enums as semantically grouped constants
- Can have data attached to variants

==

```rust
enum IpAddrKind {
    V4,
    V6,
}

let four = IpAddrKind::V4;
let six = IpAddrKind::V6;

fn route(ip_kind: IpAddrKind) {
    match ip_kind {
        IpAddrKind::V4 => println!("IPv4"),
        IpAddrKind::V6 => println!("IPv6"),
    }
}
```

== Enums with Data
- Each variant can have data attched

```rust
enum IpAddr {
    V4(String),
    V6(String),
}

// can (and usually is) different types per variant
enum IpAddr {
    V4(u8, u8, u8, u8),
    V6(String),
}

let home = IpAddr::V4(127, 0, 0, 1);
let loopback = IpAddr::V6(String::from("::1"));
```

== Complex Enum Variants

```rust
enum Message {
    Quit,                       // No data
    Move { x: i32, y: i32 },    // Struct-like
    Write(String),              // Single value    | tuple-like
    ChangeColor(i32, i32, i32), // Multiple values |
}
```
==
```rust
impl Message {
    fn call(&self) {
        match self {
            Message::Quit => println!("Quit"),
            Message::Move { x, y } => println!("Move to {}, {}", x, y),
            Message::Write(text) => println!("Text: {}", text),
            Message::ChangeColor(r, g, b) => {
                println!("Color: rgb({}, {}, {})", r, g, b)
            }
        }
    }
}
```

== Option Type
- Rust has no null
  - C. A. R. Hoare's billion dollar mistake
- `Option<T>` represents optional values
  - Since it is an actual type, you are forced to "convert it" to the inner type (forces explicit handling)

```rust
enum Option<T> {
    Some(T),
    None,
}
let some_number = Some(5);
let absent_number: Option<i32> = None;

match some_number {
    Some(n) => println!("Got number: {}", n),
    None => println!("No number"),
}
```

= More control flow structures

== match Expressions
- Must be exhaustive (an arm must exist for every value)
- Is also an expression
- Patterns matched top to bottom
  - So put the least specific arms to the bottom

```rust
enum Coin {
    Penny, Nickel, Dime, Quarter,
}

fn value_in_cents(coin: Coin) -> u8 {
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter => 25,
    }
}
```

== Patterns with Guards
- Add extra conditions to patterns with *if*

```rust
let num = Some(4);

match num {
    Some(x) if x < 5 => println!("less than five: {}", x),
    Some(x) => println!("{}", x),
    None => (),
}

// Multiple patterns
let x = 1;
match x {
    1 | 2 => println!("one or two"),
    3..=5 => println!("three through five"),
    _ => println!("anything else"),
}
```

== if let
- Concise way to handle one pattern
- Good when you only care about one case
- Can have `else` branch (which makes it an expression)

```rust
let config_max = Some(3u8);

if let Some(max) = config_max {
    println!("Maximum is {}", max);
}
```

== while let
- Loop while pattern matches
- Useful with iterators and collections

```rust
let mut stack = Vec::new();
stack.push(1);
stack.push(2);
stack.push(3);

while let Some(top) = stack.pop() {
    println!("{}", top);
}
```

== let else
- Handle refutable patterns in `let`
- Else branch must diverge
  - Diverge = return, break block/loop, kill thread, end process

```rust
fn process_data(input: Option<String>) {
    let Some(data) = input else {
        println!("L rizz, bozo");
        return;
    };

    // in this scope, data is just a binding as any other
    println!("Processing: {}", data);
}
```

= Error Handling

== Result Type
- For operations that can fail
- Must handle both success and error
- More explicit than exceptions

```rust
enum Result<T, E> {
    Ok(T),
    Err(E),
}

let f = File::open("hello.txt");
let f = match f {
    Ok(file) => file,
    Err(error) => {
        panic!("Error opening file: {:?}", error);
    }
};
```

== The ? Operator
- Shorthand for propagating errors
- Returns early if `Err`
- Can only use in functions returning `Result`

```rust
fn read_username_from_file() -> Result<String, io::Error> {
    let mut f = File::open("username.txt")?;
    let mut s = String::new();
    f.read_to_string(&mut s)?;
    Ok(s)
}
```
==
```rust
fn read_username_from_file() -> Result<String, io::Error> {
    let mut s = String::new();
    File::open("username.txt")?.read_to_string(&mut s)?;
    Ok(s)
}
```

== Panics and errors
- *panic!* for unrecoverable errors
  - We very rarely handle panics in application code
- *Result* for recoverable errors
  - You handle results all the time
  - You can promote Errs you don't want to handle into panics with *.unwrap()* and *.expect(msg)*

```rust
fn divide(a: i32, b: i32) -> i32 {
    if b == 0 {
        panic!("division by zero");
    }
    a / b
}
```
==
```rs
fn divide_safe(a: i32, b: i32) -> Result<i32, String> {
    if b == 0 {
        Err(String::from("division by zero"))
    } else {
        Ok(a / b)
    }
}
```
