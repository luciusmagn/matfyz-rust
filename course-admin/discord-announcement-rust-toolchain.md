Rust courses update (NPRG082 + NPRG074):

From now on, please use stable Rust `1.88` for all coursework and homework submissions.

Why:
1. This is the version aligned with our ReCodEx evaluation environment.
2. It reduces avoidable "works locally but fails in ReCodEx" issues.

Quick setup:
```bash
rustup toolchain install 1.88
rustup default 1.88
rustc --version
```

Important submission note:
1. Executable homework: submit `main.rs` (unless assignment says otherwise).
2. Library homework: submit `solution.rs` (this will always be stated in assignment text).

If anything in your local setup conflicts with this, ping us in this channel.
