# Course site deployment

- URL: https://matfyz.lho.sh
- Host: `root@10.0.0.1` over WireGuard, hostname `velho`, Artix/s6.
- Caddy configuration: `/etc/caddy/Caddyfile`; the site's block is in `deployment/Caddyfile`.
- Document root: `/srv/matfyz/current`, a symlink to an immutable directory under `/srv/matfyz/releases/`.
- Public content: `deployment/index.html` and the exact paths in `deployment/public-files.txt`.

## Publish

Build the edited lectures first, then run from the repository root:

\```sh
./build.sh introduction
./build.sh ownership
./deployment/deploy.sh
\```

Local requirements: SBCL, Git, rsync, SSH, and sha256sum. The remote host needs rsync, sha256sum, curl, and Caddy. An alternate SSH destination may be supplied as the first argument.

To inspect a bundle without uploading it:

\```sh
sbcl --script deployment/package.lisp /path/to/new-public-directory/
\```

Add individual student-facing files to the manifest when adding material. Preserve relative paths for shared slide assets. Each upload is checksum-verified before switching the symlink. On subsequent deployments, an HTTPS homepage check precedes success; on failure, the previous symlink is restored. Content updates need no Caddy reload.

## Roll back a course release

List `/srv/matfyz/releases/` and select a previous release. On the server, replace `RELEASE` below:

\```sh
cd /srv/matfyz
ln -s releases/RELEASE current.rollback
mv -Tf current.rollback current
\```

## Initial Caddy switch

Only the `https://matfyz.lho.sh` block in `/etc/caddy/Caddyfile` was replaced. Validate and reload after configuration edits:

\```sh
caddy validate --config /etc/caddy/Caddyfile --adapter caddyfile
caddy reload --config /etc/caddy/Caddyfile --adapter caddyfile
\```

Pre-deployment backup: `/root/backups/matfyz/20261013-pre-course/`, accessible only to root. It contains the original Caddyfile, the original site block, and a saved movie homepage response. The movie backend listens on `127.0.0.1:27777`; its process was left running. Its `/root/asterix-serve` directory had already been deleted, so the saved response is not a runnable application backup.

To restore the movie hostname while that backend is available, replace only the site's Caddy block with `matfyz.caddy` from the backup, then validate and reload. Avoid restoring the entire old Caddyfile over later unrelated site changes.
