# Session Context Dump (Do Not Commit Yet)

Last updated: 2026-02-12

This file is a continuity note for interrupted sessions. It captures what is already established in `/root/project/matfyz`.

## 1) Git/Workspace State

- Main repo: `matfyz` at `master` = `7c33ff4` (`Unify`), synced with `origin/master`.
- Safety tag created before sync: `backup/pre-upstream-sync-2026-02-11` -> `bf3ba29`.
- Integration branch kept: `integration/upstream-sync-2026-02-11` at `7c33ff4`.
- Untracked local items (intentionally preserved):
  - `Typst Data Takeout.zip`
  - `d3s-web-main/`
  - `intro-rust/`
  - `typst-sources/`
  - `braiins-university/`

## 2) Core Repositories in This Workspace

### A) `matfyz` (Common Lisp course server)

- Server code in `source/` (`server.lisp`, `html.lisp`, `filesystem.lisp`, etc.).
- Serves local directories such as `advanced-rust/`, `intro-rust/`, `basic-rust/`, `static/`.
- Recent upstream import added `basic-rust/*` and `typst.nix`.

### B) `d3s-web-main` (department Hugo website clone)

- Repo is up to date with its upstream at clone time (`master` tracking `origin/master`).
- Rust course pages of interest:
  - `content/teaching/nprg082/*` (basic Rust, winter)
  - `content/teaching/nprg074/*` (advanced Rust, spring)
  - `content/teaching/nprg073/_index.md` (legacy/redirect-like page)
- Some file links resolve via external `filesBaseURL`, so not all downloadable assets are in repo.

### C) `typst-sources/typst-data-takeout` (user export)

- Extracted from `Typst Data Takeout.zip`.
- Contains 27 Typst `main.typ` projects.
- Rust-relevant projects include:
  - `Rust intro`
  - `Data structures`
  - `Object ownership and life cycle`
  - `Generics`
  - `Multithreading`
  - `Async`
  - `Borrow Checker`
  - `Macros` + `New Macros`
  - `Libraries & testing`
  - `Unsafe Rust` + `New Unsafe`
  - `Systems programming in Rust` + `New Systems programming in Rust`
  - `Parallel programming (advanced)`
- No exact byte-identical `main.typ` duplicates by SHA-256; "New *" variants are revised versions.

### D) `braiins-university` (newly cloned)

- mdBook-based internal content repo.
- Top-level docs indicate `mdbook serve` workflow.
- Material includes broad Rust course content under `src/course/` and references under `src/reference/`.
- Not fully analyzed yet for reuse mapping.

## 3) Subject Model Agreed with User

- There are two separate subjects:
  1. Basic Rust (winter semester)
  2. Advanced Rust (spring semester)
- Teaching rhythm:
  - Lecture length: 1h30
  - Lab length: 1h30
  - About 14 teaching weeks in spring and similar amount in winter.
- Recordings are considered low-priority/irrelevant for redesign.

## 4) Lesson Mapping (Current Canonical Understanding)

### Basic (NPRG082)

Order from `d3s-web-main/content/teaching/nprg082/_index.md`:
1. Introduction
2. Data Structures
3. Object Ownership and Life Cycle
4. Polymorphic Programming
5. Concurrency and Parallelism
6. Coroutine Based Multitasking

Coverage currently available:
- Markdown lectures: `d3s-web-main/content/teaching/nprg082/lecture_1.md` ... `lecture_6.md`
- Rich Typst lecture sources: see list in section 2C
- Served local `intro-rust/` assets are currently sparse compared to source materials.

### Advanced (NPRG074)

Order from `d3s-web-main/content/teaching/nprg074/_index.md`:
1. Parallel Programming
2. Borrow Checker
3. Macros
4. Libraries and Testing
5. Unsafe Programming
6. Systems Programming

Coverage currently available:
- Markdown lectures: `lecture_1.md`, `lecture_2.md`, `lecture_3.md`, `lecture_5.md`, `lecture_6.md`
  (lecture 4 topic is in schedule, not as separate markdown file)
- Rich Typst lecture sources for all major topics
- Labs:
  - Existing advanced labs in `advanced-rust/lab-*`
  - New imported basic labs and extra materials in `basic-rust/lab-*`

## 5) Homework System Notes (from user)

- Two homework types exist:
  1. Executable homeworks
  2. Library homeworks
- Executable homework model:
  - Student uploads `main.rs` + optional extra files.
  - Judge runs program against input `.txt` files.
  - Output compared against expected `.txt` files.
  - "Lenient judge" at least ignores trailing whitespace and missing final newline.
  - It may also ignore empty lines (uncertain; needs confirmation).

## 6) Redesign Context (problem statement)

User goals/issues raised:
- Redesign scope is broader than only tweaking homework counts.
- Students dislike small numbers of homeworks.
- Web content is incomplete.
- Feedback says introductory Rust subject is too easy.
- More materials from other courses/internal Braiins content will be integrated into redesign.

## 7) Pending Immediate Work

1. Fully analyze `braiins-university` content and map reusable components to the two subjects.
2. Capture the exact "library homework" workflow from user.
3. Build a canonical source-of-truth structure:
   - Which lecture/lab assets are authoritative (Typst vs markdown vs PDFs).
4. Propose a staged redesign:
   - syllabus depth,
   - homework quantity and progression,
   - lab difficulty calibration,
   - publication/serving pipeline consistency.

## 8) Guardrails for Future Steps

- Do not clobber user-provided untracked materials.
- Do not commit this file yet unless explicitly requested.
- Prefer preserving old/new variants until user selects canonical ones.

## 9) ReCodEx Builder Work In Progress (2026-02-12)

- Location: `recodex-envs-and-utils/rust-cargo-builder`
- Branch created in nested repo:
  - `lho/rust-cargo-builder-deps-refresh`
- Commit created in nested repo:
  - `f978fc7` (`rust-cargo-builder: expand available crate set for assignments`)
  - `b9d5832` (`rust-cargo-builder: add kanal and crabtime crates`)
  - `f6279dc` (`gitignore: ignore rust-cargo-builder Cargo.lock and vendor`)
  - `2b7c73d` (`rust-cargo-builder: pin crabtime to 1.1`)
  - `d5fec67` (`rust-cargo-builder: refresh dependency versions to current stable`)
  - `8e940b0` (`gitignore: ignore rust-cargo-builder target dir`)
  - `afc20eb` (`update Rust environment`) -- branch squashed to one commit for PR
- Purpose:
  - Expand available crates in builder template for homework tasks.
  - Add missing channels/lib quality-of-life crates (e.g., `flume`).
- Current `Cargo.toml` additions:
  - `flume`, `anyhow`, `thiserror`, `async-trait`, `tokio-util`, `tokio-stream`,
    `futures-util`, `bytes`, `itertools`, `parking_lot`, `ring`,
    `regex`, `uuid`, `url`, `sha2`, `base64`, `csv`, `toml`, `serde_yaml`,
    `indexmap`, `dashmap`, `crossbeam-channel`, `async-channel`, `once_cell`,
    `num-traits`, `derive_more`, `proptest`, `approx`, `bincode`
  - `serde` adjusted to include `derive` feature.
- Not yet done:
  - Rebuild/refresh vendored project bundle (`project.zip`) via builder workflow.
  - Validate against target rustc/toolchain used in judge runtime.

## 10) Toolchain Decision (2026)

- Initial idea was `1.93.1`, but environment only supports up to `1.88`.
- Official course target changed to stable Rust `1.88`.
- Draft policy/announcement files updated accordingly:
  - `course-admin/rust-toolchain-policy.md`
  - `course-admin/discord-announcement-rust-toolchain.md`

## 11) Website Updates In Progress

### A) CL server (`matfyz/source/html.lisp`)

- Index updated to:
  - show legacy courses as:
    - Introductory Rust (old) -> `basic-rust`
    - Advanced Rust (old) -> `advanced-rust`
  - include 2026 resource links and `matfyz.lho.sh`.
- Course pages updated to:
  - show human-friendly old course labels
  - list all unit directories as lecture/lab material units
  - list root files (if any)

### B) D3S website (`d3s-web-main`)

- Branch: `lho/rust-2026-categories`
- Commit: `b34825ed`
- Changes:
  - existing Rust pages relabeled as old:
    - `content/teaching/nprg082/_index.md` -> Introductory Rust (old)
    - `content/teaching/nprg074/_index.md` -> Advanced Rust (old)
  - old pages link to archived materials at `https://matfyz.lho.sh/...`
  - new 2026 placeholders added:
    - `content/teaching/rust-intro-2026/_index.md` alias `/teaching/rust`
    - `content/teaching/rust-advanced-2026/_index.md` alias `/teaching/morerust`

## 12) Homework 2026 Scaffold Added

- New tree: `homeworks/2026/`
- Includes:
  - `homeworks/2026/PLAN.md` (pair-by-pair lecture-linked elaboration)
  - `homeworks/2026/README.md`
  - Introductory course pairs I1-I6 with `hw-a-executable` + `hw-b-library`
  - Advanced course pairs A1-A6 with one executable and one library homework each
  - each homework scaffold contains at least 4 test placeholders
- Local runner added:
  - `homeworks/2026/executor/run_homework.py`
  - supports both executable and library formats
  - lenient output comparison (trailing whitespace + final newline handling)
