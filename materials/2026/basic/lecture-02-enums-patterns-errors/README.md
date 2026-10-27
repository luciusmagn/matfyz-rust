# Enums, patterns and error handling

Draft of the third lecture, after the introduction and ownership/borrowing. 60 slides with 43 self-contained, compiler-checked examples.

- [HTML slides](index.html)
- [PDF slides](main.pdf)
- [Technical references](sources.md)

The introduction already showed `Option`, `Result`, simple `match` expressions and exhaustive matching. This lecture develops the underlying model and its consequences: variant-specific data, recursive enums, representation, pattern binding modes, ownership in matches, optional state, structured errors and propagation boundaries.

## Scope

Planned for 90 minutes, including the examples:

- Data modelling, recursive types and representation: 15 minutes
- Pattern syntax and control flow: 25 minutes
- Borrowed patterns and `Option` operations: 20 minutes
- Error types, conversion and propagation: 20 minutes
- Panics, cleanup and API choices: 10 minutes

Small independent examples accompany each topic. Closures appear only as short arguments to `map`, `and_then` and fallback methods; their type system and capture rules belong with the later generics/traits material.

## Editing and building

Edit `lesson.md`. Use `##` headings, bullet lists and short Rust examples. Shared typography, syntax highlighting, keyboard navigation and Playground links come from `../introduction/`.

Run `./build.sh` here, or `./build.sh enums` from the repository root. Pandoc and Chromium produce `index.html` and `main.pdf`.

## Verification

Run from this directory:

```sh
sbcl --script ../lecture-01-ownership-borrowing/verify-examples.lisp lesson.md
node ../introduction/verify-browser.mjs index.html --no-shrink --screenshots
```

The shared example verifier compiles with Rust 1.88.0, edition 2021. Successful examples have checked stdout; intentional compilation failures name their diagnostic code. Override the compiler with `--toolchain=VERSION`.

The browser checks cover four viewport sizes, full-size text, content bounds, Playground links, incremental bullets and print visibility. Preview screenshots are ignored by Git.
