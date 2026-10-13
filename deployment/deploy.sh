#!/bin/sh
# Publish a new immutable release; Caddy uses the atomically switched symlink.
set -eu
cd "$(dirname "$0")/.."
remote=${1:-root@10.0.0.1}
release="$(date -u +%Y%m%dT%H%M%SZ)-$(git rev-parse --short HEAD)"
staging=$(mktemp -d)
trap 'rm -rf "$staging"' EXIT HUP INT TERM
sbcl --script deployment/package.lisp "$staging/site/"
{
    printf '%s\n' index.html
    cat deployment/public-files.txt
} | while IFS= read -r file; do
    (cd "$staging/site"; sha256sum "$file")
done > "$staging/SHA256SUMS"

ssh -o BatchMode=yes "$remote" "sh -s -- '$release'" <<'REMOTE'
set -eu
mkdir -p /srv/matfyz/releases /srv/matfyz/checksums
mkdir "/srv/matfyz/releases/$1"
REMOTE
rsync -a --chmod=D755,F644 -e 'ssh -o BatchMode=yes' \
    "$staging/site/" "$remote:/srv/matfyz/releases/$release/"
rsync -a -e 'ssh -o BatchMode=yes' \
    "$staging/SHA256SUMS" "$remote:/srv/matfyz/checksums/$release"
ssh -o BatchMode=yes "$remote" "sh -s -- '$release'" <<'REMOTE'
set -eu
release=$1
cd "/srv/matfyz/releases/$release"
sha256sum -c "/srv/matfyz/checksums/$release" >/dev/null
cd /srv/matfyz
previous=$(readlink current || true)
ln -s "releases/$release" ".current-$release"
mv -Tf ".current-$release" current
if [ -n "$previous" ]; then
    probe=$(mktemp)
    trap 'rm -f "$probe"' EXIT HUP INT TERM
    if ! curl --fail --silent --show-error --max-time 20 \
        --resolve matfyz.lho.sh:443:127.0.0.1 https://matfyz.lho.sh/ -o "$probe" \
        || ! cmp -s "$probe" current/index.html; then
        ln -s "$previous" ".rollback-$release"
        mv -Tf ".rollback-$release" current
        echo 'Health check failed; restored previous release.' >&2
        exit 1
    fi
fi
printf 'Current release: %s\n' "$release"
REMOTE
