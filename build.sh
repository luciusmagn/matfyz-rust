#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
case "${1:-introduction}" in
    introduction) lecture=introduction ;;
    ownership) lecture=lecture-01-ownership-borrowing ;;
    enums) lecture=lecture-02-enums-patterns-errors ;;
    *) printf '%s\n' 'Usage: ./build.sh [introduction|ownership|enums]' >&2; exit 2 ;;
esac
exec sh "$ROOT/materials/2026/basic/$lecture/build.sh"
