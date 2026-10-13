# Ownership lecture

- [HTML slides](index.html)
- [PDF slides](main.pdf)

The 49 slides cover memory and pointers, stack and heap, structs, Box, ownership, borrowing, methods and lifetime elision.

## Editing and building

Edit `lesson.md`, using `##` headings, bullet lists, and short Rust examples. The SVG diagrams cover pointers, owned storage and moves.

Run `./build.sh` in this directory, or `./build.sh ownership` from the repository root. Install Pandoc and Chromium to generate `index.html` and `main.pdf`. The shared styles, fonts, and presentation script are in `../introduction/`. Lecture-specific styles are in `ownership.css`.

Present `index.html` in browser fullscreen at 100% zoom. Use Right, Space, or Page Down to reveal bullets and advance. Use Left or Page Up to go back, and Home or End to reach the first or last slide. Use ▶ to open a complete example in Rust Playground. The Playground compiler may be newer than the course compiler.

## Verification

Run from this directory:

```sh
sbcl --script verify-examples.lisp
node ../introduction/verify-browser.mjs index.html --screenshots
```

The example verifier uses SBCL, `mktemp`, and Rustup with Rust 1.88.0. Override the compiler with `--toolchain=VERSION`. A runnable block has the attributes `{.rust .runnable}`. Add `data-error="E0382"` for an intentional compiler error or `data-output="text"` for expected stdout. Output comparison trims trailing newlines and preserves spaces. Build files are temporary.

The browser checks cover four projector sizes, scale-independent text fitting, content bounds, diagram loading, Playground links, incremental bullets, and printing. Preview images are saved beside the HTML. Mark a slide with `data-preview="true"` to include it.
