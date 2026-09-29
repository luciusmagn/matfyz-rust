#!/bin/sh
set -eu
cd "$(dirname "$0")"
pandoc lesson.md -f markdown -t html5 --section-divs --template=template.html \
  --metadata title='Programming in Rust · Introduction' -o index.html
if command -v chromium >/dev/null 2>&1; then
    chromium --headless --no-sandbox --disable-gpu --disable-dev-shm-usage \
      --no-pdf-header-footer --virtual-time-budget=2000 \
      --print-to-pdf="$(pwd)/introduction.pdf" "file://$(pwd)/index.html" >/dev/null 2>&1
    test -s introduction.pdf
else
    printf '%s\n' 'Chromium is unavailable. Open index.html, print in landscape, and save as PDF.' >&2
fi
