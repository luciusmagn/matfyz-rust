#!/usr/bin/env python3
from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


def normalize_output(text: str, ignore_empty_lines: bool) -> str:
    lines = text.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    lines = [line.rstrip(" \t") for line in lines]

    while lines and lines[-1] == "":
        lines.pop()

    if ignore_empty_lines:
        lines = [line for line in lines if line != ""]

    return "\n".join(lines)


def run_cmd(
    cmd: list[str],
    cwd: Path,
    timeout_seconds: float,
    stdin_data: bytes | None = None,
) -> tuple[subprocess.CompletedProcess[bytes], bool]:
    try:
        completed = subprocess.run(
            cmd,
            cwd=cwd,
            input=stdin_data,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
            timeout=timeout_seconds,
        )
        return completed, False
    except subprocess.TimeoutExpired as exc:
        timeout_msg = f"\nTimed out after {timeout_seconds:.1f}s.\n".encode("utf-8")
        completed = subprocess.CompletedProcess(
            args=cmd,
            returncode=124,
            stdout=exc.stdout or b"",
            stderr=(exc.stderr or b"") + timeout_msg,
        )
        return completed, True


def prepare_project(tmpdir: Path, cargo_template: Path) -> Path:
    if not cargo_template.exists():
        raise FileNotFoundError(f"Cargo template not found: {cargo_template}")

    project = tmpdir / "project"
    src = project / "src"
    src.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(cargo_template, project / "Cargo.toml")

    cargo_lock_src = cargo_template.parent / "Cargo.lock"
    if cargo_lock_src.exists() and cargo_lock_src.is_file():
        shutil.copyfile(cargo_lock_src, project / "Cargo.lock")

    cargo_config_src = cargo_template.parent / ".cargo"
    if cargo_config_src.exists() and cargo_config_src.is_dir():
        shutil.copytree(cargo_config_src, project / ".cargo", dirs_exist_ok=True)

    vendor_src = cargo_template.parent / "vendor"
    if vendor_src.exists() and vendor_src.is_dir():
        vendor_dst = project / "vendor"
        try:
            os.symlink(vendor_src, vendor_dst, target_is_directory=True)
        except OSError:
            shutil.copytree(vendor_src, vendor_dst, dirs_exist_ok=True)

    return project


def copy_student_sources(src_dir: Path, main_or_solution: Path, extra_files: list[Path], target_name: str) -> None:
    if not main_or_solution.exists() or not main_or_solution.is_file():
        raise FileNotFoundError(f"Student source not found: {main_or_solution}")

    shutil.copyfile(main_or_solution, src_dir / target_name)
    for extra in extra_files:
        if not extra.exists() or not extra.is_file():
            raise FileNotFoundError(f"Extra source not found: {extra}")
        shutil.copyfile(extra, src_dir / extra.name)


def compare_case(case_name: str, actual: str, expected: str, ignore_empty_lines: bool) -> bool:
    actual_norm = normalize_output(actual, ignore_empty_lines)
    expected_norm = normalize_output(expected, ignore_empty_lines)
    ok = actual_norm == expected_norm
    status = "PASS" if ok else "FAIL"
    print(f"[{status}] {case_name}")
    if not ok:
        print("--- expected ---")
        print(expected_norm)
        print("--- actual ---")
        print(actual_norm)
        print("--------------")
    return ok


def cargo_command(subcommand: str, release: bool) -> list[str]:
    cmd = ["cargo", subcommand, "--quiet"]
    if release:
        cmd.append("--release")
    return cmd


def run_executable_mode(args: argparse.Namespace) -> int:
    tests_dir = Path(args.tests).resolve()
    student = Path(args.student).resolve()
    cargo_template = Path(args.cargo_template).resolve()
    extra_files = [Path(p).resolve() for p in args.extra]

    inputs = sorted(tests_dir.glob("*.in.txt"))
    if not inputs:
        print("No '*.in.txt' files found in tests directory.", file=sys.stderr)
        return 2

    with tempfile.TemporaryDirectory(prefix="hw-exec-") as td:
        try:
            project = prepare_project(Path(td), cargo_template)
            src = project / "src"
            copy_student_sources(src, student, extra_files, "main.rs")
        except FileNotFoundError as err:
            print(str(err), file=sys.stderr)
            return 2

        build, timed_out = run_cmd(cargo_command("build", args.release), project, args.timeout_seconds)
        if timed_out:
            print("Build timed out.", file=sys.stderr)
            sys.stderr.write(build.stderr.decode("utf-8", errors="replace"))
            return build.returncode
        if build.returncode != 0:
            print("Build failed.", file=sys.stderr)
            sys.stderr.write(build.stderr.decode("utf-8", errors="replace"))
            return build.returncode

        failures = 0
        for in_file in inputs:
            out_file = in_file.with_suffix("").with_suffix(".out.txt")
            if not out_file.exists():
                print(f"Missing expected output file: {out_file}", file=sys.stderr)
                failures += 1
                continue

            proc, timed_out = run_cmd(
                cargo_command("run", args.release),
                project,
                args.timeout_seconds,
                stdin_data=in_file.read_bytes(),
            )
            if timed_out:
                print(f"[FAIL] {in_file.name} (timed out)")
                sys.stderr.write(proc.stderr.decode("utf-8", errors="replace"))
                failures += 1
                continue
            if proc.returncode != 0:
                print(f"[FAIL] {in_file.name} (program exited with {proc.returncode})")
                sys.stderr.write(proc.stderr.decode("utf-8", errors="replace"))
                failures += 1
                continue

            ok = compare_case(
                in_file.stem.replace(".in", ""),
                proc.stdout.decode("utf-8", errors="replace"),
                out_file.read_text(encoding="utf-8", errors="replace"),
                args.ignore_empty_lines,
            )
            if not ok:
                failures += 1

        print(f"Executable mode done: {len(inputs) - failures}/{len(inputs)} passed")
        return 0 if failures == 0 else 1


def run_library_mode(args: argparse.Namespace) -> int:
    tests_dir = Path(args.tests).resolve()
    student = Path(args.student).resolve()
    cargo_template = Path(args.cargo_template).resolve()
    extra_files = [Path(p).resolve() for p in args.extra]

    tests = sorted(tests_dir.glob("test-*.rs"))
    if not tests:
        print("No 'test-*.rs' files found in tests directory.", file=sys.stderr)
        return 2

    with tempfile.TemporaryDirectory(prefix="hw-lib-") as td:
        try:
            project = prepare_project(Path(td), cargo_template)
            src = project / "src"
            copy_student_sources(src, student, extra_files, "solution.rs")
        except FileNotFoundError as err:
            print(str(err), file=sys.stderr)
            return 2

        failures = 0
        for test_file in tests:
            out_file = test_file.with_suffix(".out.txt")
            if not out_file.exists():
                print(f"Missing expected output file: {out_file}", file=sys.stderr)
                failures += 1
                continue

            main_rs = src / "main.rs"
            shutil.copyfile(test_file, main_rs)
            main_rs.touch()
            proc, timed_out = run_cmd(cargo_command("run", args.release), project, args.timeout_seconds)
            if timed_out:
                print(f"[FAIL] {test_file.name} (timed out)")
                sys.stderr.write(proc.stderr.decode("utf-8", errors="replace"))
                failures += 1
                continue
            if proc.returncode != 0:
                print(f"[FAIL] {test_file.name} (program exited with {proc.returncode})")
                sys.stderr.write(proc.stderr.decode("utf-8", errors="replace"))
                failures += 1
                continue

            ok = compare_case(
                test_file.stem,
                proc.stdout.decode("utf-8", errors="replace"),
                out_file.read_text(encoding="utf-8", errors="replace"),
                args.ignore_empty_lines,
            )
            if not ok:
                failures += 1

        print(f"Library mode done: {len(tests) - failures}/{len(tests)} passed")
        return 0 if failures == 0 else 1


def main() -> int:
    repo_root = Path(__file__).resolve().parents[3]
    default_cargo_template = repo_root / "recodex-envs-and-utils" / "rust-cargo-builder" / "Cargo.toml"

    parser = argparse.ArgumentParser(description="Local runner for 2026 Rust homework formats.")
    parser.add_argument(
        "--cargo-template",
        default=str(default_cargo_template),
        help="Path to Cargo.toml template (default: recodex-envs-and-utils/rust-cargo-builder/Cargo.toml)",
    )
    parser.add_argument(
        "--timeout-seconds",
        type=float,
        default=8.0,
        help="Timeout per cargo command in seconds (default: 8.0)",
    )
    parser.add_argument("--release", action="store_true", help="Use cargo --release for build/run")
    parser.add_argument("--ignore-empty-lines", action="store_true", help="Ignore empty lines in output comparison")
    parser.add_argument("--extra", action="append", default=[], help="Extra student .rs file to copy (repeatable)")

    subparsers = parser.add_subparsers(dest="mode", required=True)

    p_exec = subparsers.add_parser("executable", help="Run executable homework checks")
    p_exec.add_argument("--student", required=True, help="Path to student main.rs")
    p_exec.add_argument("--tests", required=True, help="Path to tests dir with *.in.txt/*.out.txt")

    p_lib = subparsers.add_parser("library", help="Run library homework checks")
    p_lib.add_argument("--student", required=True, help="Path to student solution.rs")
    p_lib.add_argument("--tests", required=True, help="Path to tests dir with test-*.rs/test-*.out.txt")

    args = parser.parse_args()

    if args.timeout_seconds <= 0:
        print("--timeout-seconds must be > 0", file=sys.stderr)
        return 2

    if args.mode == "executable":
        return run_executable_mode(args)
    if args.mode == "library":
        return run_library_mode(args)

    parser.print_help()
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
