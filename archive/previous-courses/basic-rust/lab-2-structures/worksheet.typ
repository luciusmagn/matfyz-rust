#set page(background: place(dy: -1em, bottom + center)[#sys.inputs.at("store-path", default: "non-deterministic")])
#show link: underline

#align(
  center,
  {
    text(20pt, weight: "bold")[Lab 2 handout -- Programming in Rust]
    v(-1em)
    [#datetime(year: 2025, month: 10, day: 23, hour: 14, minute: 0, second: 0).display("[year]-[month]-[day] [hour]:[minute]") -- Luukas Pörtfors & Lukáš Hozda]
  },
)

#v(15%)

#set heading(numbering: "1.")

= Number game

In this classic example, you are to create a program that chooses a (random) number, and makes the user guess the number, while giving them some hints.

Your program is to work roughly in the following way:

+ Pick a (random) number, and print out `"Welcome to the number game!"`
+ Read a number from stdin: <read_line>
  - if that number is *equal to* the chosen number, print `"The number was indeed {number}!"`
  - if that number is *less than* the chosen number, print `"Wrong, try guessing higher!"`
    - go back to step 2
  - if that number is *more than* the chosen number, print `"Wrong, try guessing lower!"`
    - go back to step 2

== Tip: Picking a number

You may use the following snippet to choose a random number:
```rust
let number: u8 = rand::random();
```

NOTE: You'll also need to ```sh cargo add rand```

== Tip: Reading input

To find out how to read input, look for example in the standard library #link("https://doc.rust-lang.org/std/io/index.html")[io module]

== Tip: Parsing numbers

In order to parse the input string into a number, you'll probably want to use the #link("https://doc.rust-lang.org/std/primitive.str.html#method.parse")[parse] method from the standard library.
Remember to also *handle the case where parsing fails*.

= Quadratic equation solver

Implement a program that solves quadratic equations. Use the famous quadratic equation formula below:

$
  x = (-b plus.minus sqrt(b^2-4a c)) / (2a)
$

Your program is expected to work with real roots only ($x in RR$), for complex roots, use the ```rust Roots::Complex``` variant. You won't have to compute the values.

== Tip: program skeleton

#box(```rust
enum Roots {
  One(f32),
  Two(f32, f32),
  Complex,
}

fn solve_quadratic(a: f32, b: f32, c: f32) -> Roots {
  todo!("Your solution here")
}

fn main() {
  // 2x^2+3x-5 = 0
  // Solution: x = 1 or x = -2.5
  let (a, b, c) = (2.0, 3.0, -5.0);

  match solve_quadratic(a, b, c) {
    _ => todo!("Match on Roots and print the output accordingly")
  }
}
```)

== Tip: Docs

You may want to take a look at the #link("https://doc.rust-lang.org/std/primitive.f32.html")[f32] documentation in in the standard library.

The documentation for #link("https://doc.rust-lang.org/std/vec/struct.Vec.html")[`Vec`] might also be useful.

= Calculator (RPN)

In this example you are to implement a calculator that works with the #link("https://en.wikipedia.org/wiki/Reverse_Polish_notation")[Reverse Polish notation].

Here's some example behavior of the program:
```
> 1 2 +
3
> 1 2 3 + -
4
> 3 +
Error: wrong amount of operands
> a 2 +
Error: 'a' is not a number or an operator
```

Your calculator should implement at least the `+` and `-` operators.

== Tip: Stack

For this exercise, you'll probably want to use a stack data structure. For this purpose, I recommend using a #link("https://doc.rust-lang.org/std/vec/struct.Vec.html")[`Vec`] from the standard library.
