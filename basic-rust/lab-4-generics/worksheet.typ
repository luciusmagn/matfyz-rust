#set page(background: place(dy: -1em, bottom + center)[#sys.inputs.at("store-path", default: "non-deterministic")])
#show link: underline

#align(
  center,
  {
    text(20pt, weight: "bold")[Lab 4 handout -- Programming in Rust]
    v(-1em)
    [#datetime(year: 2025, month: 11, day: 27, hour: 14, minute: 0, second: 0).display("[year]-[month]-[day] [hour]:[minute]") -- Luukas Pörtfors & Lukáš Hozda]
  },
)

#set heading(numbering: "1.")

#v(7%)

= Pair

Implement a structure ```rust Pair``` that stores a pair of values of potentially different types.
Your structure should implement the ```rust Debug``` and ```rust Clone``` traits.

Implement the following methods for pair:
- `fn fst(&self)` to get the first element of the pair
- `fn snd(&self)` to get the second element of the pair
- `fn swap(self)` to create a new pair with the order of elements swapped

Additionally implement a function `compose` that takes in two ```rust Pair```s and composes them into a ```rust Pair```.
This function should only work with ```rust Pair```s!

```rust
#[test]
fn fst_and_snd_work() {
  let p = Pair::new(42usize, true);
  assert_eq!(p.fst(), 42);
  assert!(p.snd());
}

#[test]
fn swap_works() {
  let p = Pair::new(42usize, false);
  let swapped = p.swap();

  assert_eq!(swapped.fst(), false);
  assert_eq!(swapped.snd(), 42);
}

#[test]
fn compose_works() {
  let p1 = Pair::new(42usize, false);
  let p2 = Pair::new(String::from("hi"), 42.0);

  let comp = compose(p1, p2);
  assert_eq!(comp.snd().fst(), String::from("hi"));
}

fn main() {
  // let p = Pair::new();
  // NOTE: pair should be Debug and Clone
  // dbg!(p.clone());

  // this shouldn't compile:
  // let p2 = compose(1, 2);
}
```

== Tips

For implementing Debug and Clone, it's easiest to #link("https://doc.rust-lang.org/reference/attributes/derive.html")[_derive_] them. Run tests with ```sh cargo test```.

= "Duck typing"

#quote(block: true)[If walks like a duck and quacks like a duck, then it must be a duck.]

We don't really have duck typing in Rust (thank god) since everything is statically checked, but let's mimic something like that with trait objects.

In the code block below, there's an implementation of ```trait Duck``` and various structs (ducks) that implement it, some of them override the methods so that they don't behave like a duck.
Your task is to implement a function `duck_filter` that takes in a ```rust Vec``` of ```rust Duck``` trait objects (in a ```rust Box```) and checks that each of them behave like a duck.

The function returns a tuple of ```rust Vec```s, where the first one contains the objects that behaved like a duck and the second one the rest of the objects.

```rust
trait Duck {
    fn quack(&self) -> &str {
      "quack"
    }
    fn walk(&self) -> &str {
      "waddle, waddle"
    }
}

struct Human {}
struct Dog {}
struct Cat {}
struct Goose {}
struct Rock {}

impl Duck for Human {}
impl Duck for Dog {}
impl Duck for Cat {}
impl Duck for Goose {
  fn quack(&self) -> &str {
    "goose sounds"
  }
}
impl Duck for Rock {
  fn walk(&self) -> &str {
    "refuses to move"
  }
}

#[test]
fn duck_filter_works() {
    let ducks: Vec<Box<dyn Duck>> = vec![Box::new(Human {}), Box::new(Dog {}), Box::new(Goose {}), Box::new(Cat {}), Box::new(Rock {})];
    let (yes, no) = duck_filter(ducks);
    assert!(yes.iter().all(|d| d.quack() == "quack"
      && d.walk() == "waddle, waddle"));
    assert!(!no.iter().any(|d| d.quack() == "quack"
      && d.walk() == "waddle, waddle"));
}
```

= Merge: Iterator

Finish the implementation of a ```rust Iterator``` called ```rust Merge```.
It is given two iterators and produces an iterator that alternates between them.
If either side runs out of elements, the iterator will continue with elements from the other side.

Starter code:

```rust
pub struct Merge<I1, I2> {
    left: I1,
    right: I2,
    // TODO: add more elements for logic
}

impl<T, I1, I2> From<(I1, I2)> for Merge<I1::IntoIter, I2::IntoIter>
where
    I1: IntoIterator<Item = T>,
    I2: IntoIterator<Item = T>,
{
    fn from((t1, t2): (I1, I2)) -> Self {
        todo!()
    }
}

impl<T, I1, I2> Iterator for Merge<I1, I2>
where
    I1: Iterator<Item = T>,
    I2: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

#[test]
fn left_longer() {
    let merged: Vec<_> = Merge::from((vec![1, 3, 5, 7], vec![2, 4])).collect();
    assert_eq!(merged, vec![1, 2, 3, 4, 5, 7]);
}

#[test]
fn right_longer() {
    let merged: Vec<_> = Merge::from((vec![1, 3], vec![2, 4, 6, 8])).collect();
    assert_eq!(merged, vec![1, 2, 3, 4, 6, 8]);
}

#[test]
fn left_empty() {
    let merged: Vec<_> = Merge::from((vec![], vec![1, 2, 3])).collect();
    assert_eq!(merged, vec![1, 2, 3]);
}
```

== Tips

Ensure that your logic is _lazy_ and doesn't unintentionally consume elements from an iterator.
You may want to look into #link("https://doc.rust-lang.org/std/option/enum.Option.html#method.or_else")[`Option::or_else`] for example.

