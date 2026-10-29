#!/bin/sh
# Run a throwaway Radicale for the CalDAV suite, in the foreground.
#
# Radicale comes from nixpkgs, so no container is needed. One htpasswd user
# (`test`, password `test`) over plain HTTP on 127.0.0.1, its collections
# kept under the directory given as first argument (a fresh temporary one
# by default).
#
# It is served by waitress rather than by Radicale's built-in server, which
# answers HTTP/1.0 and closes the connection after every response: the
# CalDAV client keeps one connection per run and does not reconnect.
#
# Stop it with Ctrl-C. While it runs, the suite runs with:
#
#   UNFOG_E2E_URL=http://127.0.0.1:5243/ cargo test --test caldav -- --ignored

set -eu

DIR=${1:-$(mktemp -d)}
PORT=${UNFOG_RADICALE_PORT:-5243}

mkdir -p "$DIR/collections"
printf 'test:test\n' > "$DIR/users"

cat > "$DIR/config" <<CONF
[auth]
type = htpasswd
htpasswd_filename = $DIR/users
htpasswd_encryption = plain

[rights]
type = owner_only

[storage]
filesystem_folder = $DIR/collections
CONF

echo "radicale on http://test:test@127.0.0.1:$PORT/, data in $DIR" >&2
exec nix-shell -p radicale python3Packages.waitress --run \
    "RADICALE_CONFIG='$DIR/config' waitress-serve --listen=127.0.0.1:$PORT radicale:application"
