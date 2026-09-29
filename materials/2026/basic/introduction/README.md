# Introduction lecture

Edit `lesson.md`. Its `##` headings mark slides. Mark a Rust code block with `{.rust .runnable}` to add an **Edit and run in Rust Playground** button to the HTML. The PDF contains the original code and the explanation on the following slide.

Run `./build.sh` to rebuild `index.html` and `introduction.pdf`. You need Pandoc and Chromium. Open `index.html` in a browser; use the arrow keys or the on-screen buttons to move between slides.

The HTML opens runnable examples in the Rust Playground, which uses its own current toolchain and requires internet access. For a local demonstration, paste an example into `main.rs` and run `rustup run 1.88.0 rustc --edition=2021 main.rs && ./main`. The two examples that demonstrate rejected programs are meant to produce compiler errors.

The course details here come from the 2026/27 course page and `course-admin/rust-toolchain-policy.md`. Check the Discord invite and the current assignment deadlines before presenting. The 2025 introduction contains dates and grading details that should not be carried over without confirmation.
