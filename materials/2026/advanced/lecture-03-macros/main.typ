#import "@preview/touying:0.5.3": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => [Advanced Rust 2026 · Lecture 3],
  config-info(
    title: [Advanced Rust (2026): Macros],
    subtitle: [Lecture 3],
    author: [Lukáš Hozda],
    institution: [MFF CUNI],
    date: [Spring 2026],
  ),
)

#title-slide()

= Metaprogramming

== Metaprogramming
- Program manipulates program structure #pause
- Transformation may happen before execution or during execution #pause
- Macros are one compile-time metaprogramming technique #pause
- Rust uses them heavily in the standard library

== Compile-Time vs Runtime
- Compile-time: macros, code generators, templating #pause
- Runtime: reflection, dynamic evaluation, metaclasses #pause
- Rust is much stronger on the compile-time side #pause
- This is consistent with its zero-cost bias

== Kinds of Macros in Rust
- Declarative macros: `macro_rules!` pattern matching over token trees #pause
- Procedural macros: Rust code transforms token streams #pause
- Attributes are macros too #pause
- These two families behave quite differently

== Macro Invocation Syntax
- Invocation is always `name!(...)`, `name![...]`, or `name!{...}` #pause
- Delimiter choice is usually stylistic, not semantic #pause
- Parsing of the inside is up to the macro matcher or procedural macro code

== Standard Library Macros
#set text(size: 0.88em)
#table(
  columns: 2,
  inset: 8pt,
  align: horizon,
  table.header([*Macro*], [*Purpose*]),
  [`assert!`, `assert_eq!`, `assert_ne!`], [test assumptions at runtime],
  [`format!`, `println!`, `write!`], [format-string frontends],
  [`vec!`], [collection construction shorthand],
  [`matches!`], [pattern test shorthand],
  [`stringify!`, `concat!`, `compile_error!`], [compile-time helpers],
  [`file!`, `line!`, `column!`, `module_path!`], [source-location information],
  [`include!`, `include_str!`, `include_bytes!`], [embed external file contents],
)
#set text(size: 1em)

== Built-In Macros
- Some macros are compiler built-ins pretending to be ordinary `macro_rules!` items #pause
- Example: `concat!`, `stringify!`, `format_args!` #pause
- Their surface syntax looks ordinary even though expansion is compiler-supported

= Declarative Macros

== `macro_rules!`
- Matcher on the left, expansion on the right #pause
- Arms are tried top to bottom #pause
- Matching is done on token trees, not typed AST nodes #pause
- Expansion happens before type checking

== Expansion Phases
- Parse source into tokens and token trees #pause
- Expand macro invocations #pause
- Parse expanded output as ordinary Rust #pause
- Only then run name resolution, type checking, borrow checking, and code generation

== Minimal Example
```rust
macro_rules! twice {
    ($x:expr) => {
        2 * ($x)
    };
}

let y = twice!(21);
println!("{y}");
```

== Expansion Is Separate from Typing
- Macro can expand to syntactically valid but type-incorrect code #pause
- Macro can also fail before type checking due to matcher problems #pause
- Macro reasoning and type reasoning are separate tasks

== Hygiene
- Rust declarative macros are not textual substitution #pause
- Macro-created locals do not accidentally capture caller locals #pause
- Metavariables keep the caller's bindings

== Hygiene Example
```rust
macro_rules! using_a {
    ($e:expr) => {{
        let a = 42;
        $e
    }};
}

let four = using_a!(a / 10);
```

== Why This Fails
```rust
let four = {
    let a = 42;
    /* macro-created binding */
    a / 10
};
```
- The `a` introduced by the macro has different syntax context #pause
- The `a` inside `$e` still refers to the caller context #pause
- Hygiene prevents accidental capture

== Hygiene Fix
```rust
macro_rules! using_a {
    ($a:ident, $e:expr) => {{
        let $a = 42;
        $e
    }};
}

let four = using_a!(a, a / 10);
```

== Fragment Specifiers I
- `expr`: expression #pause
- `ident`: identifier or keyword-like identifier #pause
- `ty`: type #pause
- `path`: path in type-like syntax #pause
- `pat` / `pat_param`: pattern forms

== Fragment Specifiers II
- `item`: item #pause
- `stmt`: statement #pause
- `block`: block expression #pause
- `tt`: one token tree #pause
- `meta`, `literal`, `vis`, `lifetime`: specialized fragments

== `tt`
- `tt` is the escape hatch fragment #pause
- It matches one token or one delimited token tree #pause
- Recursive macros often rely on it #pause
- It is powerful and easy to misuse

== Repetition
- `$()*` means zero or more #pause
- `$()+` means one or more #pause
- `$()?` means optional single occurrence #pause
- Put the separator inside the repetition form

== `vec!`-Style Pattern
```rust
macro_rules! vec_of_stringified {
    ($($element:expr),* $(,)?) => {{
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push(stringify!($element));
        )*
        temp_vec
    }};
}
```

== Repetition Notes
- `$(,)?` is the standard way to allow an optional trailing comma #pause
- Nested repetition is possible, but metavariable alignment gets harder #pause
- Error messages get worse as grammar becomes more clever

== Counting Arguments with a Macro
```rust
macro_rules! assert_min_args {
    (@count) => { 0 };
    (@count $first:expr $(, $rest:expr)*) => {
        1 + assert_min_args!(@count $($rest),*)
    };
```
==
```rust
    ($min_count:expr, $($args:expr),+ $(,)?) => {
        const _: () = {
            let count: usize = assert_min_args!(@count $($args),+);
            assert!(count >= $min_count, "Not enough arguments provided");
            ()
        };
    };
}
```

== Matcher Restrictions
- Not every token may follow every fragment kind #pause
- This is the follow-set restriction system #pause
- It exists so the parser can keep macro matching unambiguous

== Follow-Set Rules
#set text(size: 0.9em)
- `expr` and `stmt` may only be followed by `=>`, `,`, or `;` #pause
- `pat` and `pat_param` have their own allowed follower sets #pause
- `ty` and `path` are also restricted #pause
- `tt` has no such restriction
#set text(size: 1em)

= Procedural Macros

== Procedural Macros
- Procedural macros are Rust programs that receive tokens and return tokens #pause
- They live in a dedicated proc-macro crate #pause
- They are much less constrained than `macro_rules!`

== Why a Separate Crate
- Proc-macro crates compile to a dynamic library loaded by the compiler #pause
- That is why the macro cannot usually live in the same crate as its use site #pause
- Common pattern: proc-macro crate plus ordinary crate re-exporting macros

== Typical Dependencies
- `proc_macro`: compiler token stream API #pause
- `syn`: parser for Rust syntax #pause
- `quote`: quasi-quoting tokens back out #pause
- `proc_macro2`: library-friendly substitute for compiler token stream types

== Proc-Macro Crate Setup
```toml
[package]
name = "my_proc_macro"
version = "0.1.0"
edition = "2021"

[lib]
proc-macro = true

[dependencies]
syn = "1.0"
quote = "1.0"
```

== Function-Like Proc Macro
```rust
use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn make_function(input: TokenStream) -> TokenStream {
    let name = syn::parse::<syn::Ident>(input).unwrap();
```
==
```rust
    let output = quote! {
        fn #name() -> String {
            String::from("Hello, world!")
        }
    };
    output.into()
}
```

== Derive Macro
```rust
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(SimpleDebug)]
pub fn simple_debug_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
```
==
```rust
    let output = quote! {
        impl std::fmt::Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, stringify!(#name))
            }
        }
    };
    output.into()
}
```

== Attribute Macro
```rust
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, ItemFn};

#[proc_macro_attribute]
pub fn log_execution(args: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as ItemFn);
    let _args = parse_macro_input!(args as AttributeArgs);
    let fn_name = &input_fn.sig.ident;
```
==
```rust
    let output = quote! {
        #input_fn
        fn modified_#fn_name() {
            println!("Executing function: {}", stringify!(#fn_name));
            #fn_name();
            println!("Finished executing: {}", stringify!(#fn_name));
        }
    };
    output.into()
}
```

== Proc-Macro Notes
- Proc macros are effectively unhygienic compared with declarative local-variable hygiene #pause
- Use absolute paths in generated code when appropriate #pause
- Generated code is ordinary Rust and should be read as such

= Resolution and Scope

== Definition Order
- Declarative macros are generally available only after their definition #pause
- This differs from items like functions #pause
- Imported macros also follow macro-specific resolution rules

== Accessibility Example
```rust
mod a {
    // X!(); // undefined
}

mod b {
    macro_rules! X { () => {}; }
    X!();
}
```
==
```rust
mod c {
    // X!(); // undefined
}
```

== Shadowing
```rust
macro_rules! X { (1) => {}; }
X!(1);

macro_rules! X { (2) => {}; }
// X!(1); // no rule matches `1`
X!(2);
```
==
```rust
mod a {
    macro_rules! X { (3) => {}; }
    X!(3);
}

X!(2);
```

== `#[macro_export]`
- `#[macro_export]` exports a declarative macro from the crate root #pause
- Legacy `#[macro_use]` still exists but is older style #pause
- Visibility keywords on declarative macros do not behave like ordinary item visibility

== `$crate`
- `$crate` refers to the defining crate root #pause
- Use it when one exported macro calls helpers from the same crate #pause
- This avoids breakage at external call sites

== `$crate` Example
```rust
macro_rules! call_helper {
    () => {
        $crate::helper()
    };
}
```

== Keyword Matching Oddity
```rust
macro_rules! what_is {
    (self) => { "the keyword `self`" };
    ($i:ident) => { concat!("the identifier `", stringify!($i), "`") };
}

macro_rules! call_with_ident {
    ($c:ident($i:ident)) => { $c!($i) };
}
```
==
```rust
fn main() {
    println!("{}", what_is!(self));
    println!("{}", call_with_ident!(what_is(self)));
}
```
==
```text
the keyword `self`
the keyword `self`
```

= Tooling and Limits

== Development Tools
- `cargo expand` shows expansion result #pause
- `trace_macros!()` prints declarative expansion steps on nightly #pause
- `log_syntax!()` dumps passed tokens on nightly #pause
- `macro_railroad` can visualize declarative macro grammars

== What Macros Are Bad At
- Complex grammar quickly destroys diagnostics #pause
- Clever mini-languages are hard to maintain #pause
- Type-directed behavior is limited in declarative macros #pause
- Sometimes a function, trait, build script, or code generator is the better tool

== Reading List
- The Little Book of Rust Macros #pause
- Rust Reference: macros by example #pause
- Rust Reference: procedural macros

= Questions?
