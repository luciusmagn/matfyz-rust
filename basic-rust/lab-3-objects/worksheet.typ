#set page(background: place(dy: -1em, bottom + center)[#sys.inputs.at("store-path", default: "non-deterministic")])
#show link: underline

#align(
  center,
  {
    text(20pt, weight: "bold")[Lab 3 handout -- Programming in Rust]
    v(-1em)
    [#datetime(year: 2025, month: 11, day: 12, hour: 14, minute: 0, second: 0).display("[year]-[month]-[day] [hour]:[minute]") -- Luukas Pörtfors & Lukáš Hozda]
  },
)

#set heading(numbering: "1.")

#v(20%)

= Builder pattern

In this exercise we're practicing the implementation of a builder pattern for an ```rust Invoice``` struct.
In your implementation, missing or invalid fields should return a `BuilderError` of some sort. Don't bother with validating IBAN-numbers.

```rust
use chrono::NaiveDate;

struct Invoice {
  title: String,
  description: String,
  account_number: String,
  seller: String,
  buyer: String,
  due_date: NaiveDate,
  // Defaults to today
  invoice_date: NaiveDate,
  rows: Vec<InvoiceRow>
  total_price: usize,
}

struct InvoiceRow {
  item: String,
  price: usize,
}

// TODO
struct InvoiceBuilder {}

impl Invoice {
  fn builder() -> InvoiceBuilder {
    InvoiceBuilder::default()
  }
}

// Example
// Invoice::builder()
//    .title("Food purchase")
//    .description("This is what was bought")
//    .account_number("DE03201202006676642478")
//    .seller("John")
//    .buyer("Jack")
//    .add_row("1kg Corn", 100)
//    .add_row("1l Milk", 200)
//    .due_date(2026, 1, 5)
//    .build()
//
//  => Ok(Invoice {
//    title: "Food purchase",
//    description: "This is what was bought",
//    account_number: "DE03201202006676642478",
//    seller: "John",
//    buyer: "Jack",
//    invoice_date: 2025-11-13,
//    due_date: 2026-01-05,
//    rows: [
//      InvoiceRow { item: "1kg Corn", price: 100 },
//      InvoiceRow { item: "1l Milk", price: 200 },
//    ],
//    total_price: 300,
//  })
```


= A calculator with an Abstract Syntax Tree

Your friend has built a calculator, to avoid plagiarism claims they've used the prefix notation instead of RPN.
They claim that it's better than our RPN calculator from a few weeks ago, it even parses the expressions to an #link("https://en.wikipedia.org/wiki/Abstract_syntax_tree")[Abstract syntax tree] #emoji.face.explode!
Fortunately for you, the friend has chosen a poor language for the implementation, that being #link("https://gleam.run/")[Gleam].
Let's re-implement their calculator in Rust.

Here's a #link("https://playground.gleam.run/#N4IgbgpgTgzglgewHYgFwEYA0IDGyAuES+aIcAtgA4JT4AEA5gDYQCG5A9ApfokgHTB8AT0oQ6AeR59MdAHLIIsgMoJyEAL4AdJBWq1GLdhxj4ocJAx07KAVwBGdEWLoAFVrAhTeyABSsASjoAXklpPwBiX2UzCwZZQIDrJA5UugAVAAtxAEFldJsHJ1FxdOhyOmAdOnlbcl8ASWIkpBqcgBN23zKoclke8haa5Qdu8v7yoboAWVsmMd6J3pbtJELHADNWyg8YCAB9GB2cCBhfGFQ6GPNLIIBaAD43Xa9wpF85OCYgqta6HFYezoMEq1RqdC0IAhUIAPE8QY9QX9wf9AeIdp5DsdTucfmCUcM1BBfFEoKd8LJ9gF7k9VOoSb4yaZZJ9vlMCfJFHREXTiVEYCyvtT8eDViixeD9tyngokBAwatVus6Fs6Bi9vt7BYPMJ9txzpdrnEac9PN4+L4tgs+hlJtLbcskTUAUCQb8UZCANSQuhw4H23kMgV0DrtYXI6F3H1+hG0olBlQOcMekAAKmj8ID8f5slmbJFUsRsvlf0VyTsm1a7TgDDg+H2+AQ+ws+F8OENsVu9vNfia+Dxfxd4lMNwY/Eb+1s+A2+zw7Qg1BbZxwA5RAG0kABde3ugkseitUIjuL8KczucLhAthtNlu+JDs8FDugAFgAHL7Qq0AGTfuitGE6AAVg/XcOXSKBbHEHl41aO5XzfR8UQAMVYJggSLRQRRqCUcILe1iwVHQlRSNJ3E8EFWGBOIWDoata3oRs6FYVoWwgBhoGVVV1QOei6wNK5OwYE1yL2Ht3j7VdUVdIT+GoSh9gYKBWEobJ6RgKSagkABrBlslYdpZHwVghRpbDpPEPj6wnO99LDJ0OSueN7Jg+komMr5ZDDJDJQIrCIzwiNCxlfzApIisVW2F59iQOp9iYBB9QuQTRwSHB2zoST7VE14fAk5oHOfHj9is3EHMJNzGXJLyazral7WK2LyHixLKCq5k6HQAAGOhU2Y9K6E9Ojav7EViyzSrg1YdLkzoMs1iQCLuOipqBKNLtERy8TGgK3dbCBUlquGhighheDuDy8dsneYrSo0qZGrihL9SZCljrq4jyyKZbMUIXo1qEkSXm2gYpKK6KtSQHU9TajTyqcyrXtkbh6sRMDwX28RDo6qlfXOt4rqIXxiqOaacVe2aUUxuhsbev7yHQU78cu/BruJ6L6fa/sfOp2nKSZugLr4Qmbui0mTjOCmeYOrmjPKAAmAWheQEX2d+8oufDczAz5wW2vprAnAV6lH1wuhgvhmpedl/86iVgnWaJx76nu7XsxtuQ6nvO3KbmoikHC76os8AHRxNAAlU45lbAYVEBwq0TVDmNbh9GdZt+nUaedHnUT16QlCSEfRz8EIKg7tdMz8yalQ9DoKeABRKAoBoXxIVsOUAA8xBwQh2joCw7HwS4AHIMzofO/UhMeQB8s2zfGxEm5bqA25AdU4hVEyWHaSEpnmrjWggMA0N8YeHUGe0+wToF6HRz36gfe0FpRUMz8N/BFftY/T/wRnBroD/eYn9HwjHsO/IyX9ERAPfvcQBJ9gGKxFHmCBTgoFPBgX/IIfVMFINLJ9RaQc6DkBMu8KSEAcCZAQEnEOXpOoAPlr1YCdAABse9kgaBABoIAA=")[link]
to their implementation.

#set raw(syntaxes: "gleam.sublime-syntax")

== Implementation plan

+ Start with the types. First translate the AST (Term). It is recursive, which means that we don't know it's size. How do we deal with `!Sized` types in Rust?
+ Then move on to translating the ```rust ParseOption``` type. We don't want to allocate a new ```rust String``` for each time we
  invoke a parser so you should use a slice instead. You can use a type alias.
+ Some of the parsers are easier to implement in Rust, look for standard library methods.
+ It probably makes sense to test the parsers individually, you can do this in the main function, with explicit tests or more conveniently, with #link("https://doc.rust-lang.org/rustdoc/write-documentation/documentation-tests.html")[Doctests].
