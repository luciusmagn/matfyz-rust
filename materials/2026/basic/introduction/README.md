# Introduction lecture

Edit `lesson.md`. Use `##` for slide headings, short bullet lists, and fenced code. Keep a blank line before each heading. A Rust block with attributes `{.rust .runnable}` gets a ▶ link at its upper-right corner. Add `data-error="E0502"` (with the appropriate error code) to identify an intentional compile failure in the source.

Run `./build.sh` to rebuild `index.html` and `introduction.pdf`. You need Pandoc and Chromium.

## Presenting

Open `index.html`, reset browser zoom to 100%, and enter browser fullscreen. Navigate with the arrow keys, Space, Page Up/Down, Home, or End. Slides scale as a whole to fit the window, preserving the 16:9 composition on other screen shapes. At 1920×1080, body text is 60px and code is 48px; the relative sizes also apply to the PDF.

Advance with Right, Space, or Page Down to reveal one bullet at a time, then move to the next slide. Left or Page Up reverses a reveal; going back to the preceding slide shows its complete list. Home returns to the title, and End opens the last slide with all bullets visible. PDF pages contain every bullet.

Select ▶ to open the example in Rust Playground in a separate tab. Internet access is required there, and its stable compiler may be newer than the course compiler. For a local demonstration, save the code as `main.rs`, then compile with `rustup run 1.88.0 rustc --edition=2021 main.rs` and run the resulting executable.

The Contacts slide includes a QR code for the course Discord. To change its destination, update the links in `lesson.md` and regenerate the image with `qrencode -o discord-qr.png -s 12 -m 4 -l Q --background=fffefa 'https://discord.gg/E4XpeWbJpx'`.

## Checks

Run `node verify-browser.mjs` after building. With Node 22+ and Chromium, it checks every slide for overflow at 1024×768, 1280×720, 1920×1080, and 3840×2160, plus print bounds, bullet reveals and reversal, full bullet visibility in print, and example links. Add `--screenshots` to save title, contacts, and course-organization previews as `.preview-*.png` files.

The 15 runnable examples were checked with Rust 1.88.0: ten compile and run; five produce their annotated error codes. The data-modelling fragment is a type declaration, not a standalone executable.

## Timing and course details

See `teaching-notes.md` for a 90-minute allocation and the mapping from the original introduction.
