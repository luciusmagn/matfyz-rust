#!/bin/sh
set -eu
cd "$(dirname "$0")"
pandoc lesson.md -f markdown -t html5 --section-divs --template=template.html \
  --metadata title='Programming in Rust · Enums, patterns and errors' -o index.html
profile=$(mktemp -d)
trap 'rm -rf "$profile"' EXIT HUP INT TERM
chromium --headless --no-sandbox --disable-gpu --disable-dev-shm-usage \
  --user-data-dir="$profile" --no-pdf-header-footer --virtual-time-budget=2000 \
  --print-to-pdf="$(pwd)/main.pdf" "file://$(pwd)/index.html" >/dev/null 2>&1
test -s main.pdf
