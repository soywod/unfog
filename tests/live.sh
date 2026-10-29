#!/usr/bin/env bash
# Run the CalDAV suite (tests/caldav.rs) against a live provider, through
# the guard proxy (tests/live-guard.py).
#
#   tests/live.sh fastmail|posteo [cargo test filter...]
#
# The provider is reached only through a local reverse proxy that refuses
# every request outside the run's scratch collections (`unfog-e2e-<run>-<n>`)
# and hides every other collection from a listing of the calendar home. The
# suite creates those collections, deletes them however a test ends, and a
# sweep removes the leftovers of older runs; the guard's refusals fail the
# run. Credentials come from pass into variables and files of mode 0600 in a
# private temporary directory, never onto a command line or into the output.
#
# A provider refusing MKCALENDAR (Posteo) lends one calendar instead,
# UNFOG_E2E_CALENDAR naming its id under the home: the suite runs inside it
# the tests one calendar serves and skips the others, each test removes the
# items it added, and the guard keeps the calendar itself and every item it
# held before read only.
#
# Run from the repository root, outside the devshell (it enters it).

set -euo pipefail

provider=${1:?usage: tests/live.sh fastmail|posteo [filter...]}
shift

case $provider in
fastmail)
    upstream=https://caldav.fastmail.com
    entry=/dav/
    user_command="printf pimalaya@fastmail.org"
    password_command="pass show pimalaya/fastmail-webdav | head -n1"
    ;;
posteo)
    upstream=https://posteo.de:8443
    entry=/
    user_command="pass show posteo/email | head -n1"
    password_command="pass show pimalaya/posteo | head -n1"
    # NOTE: Posteo refuses MKCALENDAR; the run borrows the one calendar the
    # account lends it, by its id under the home, and nothing else.
    : "${UNFOG_E2E_CALENDAR:?posteo needs UNFOG_E2E_CALENDAR, the lent calendar id}"
    ;;
*)
    echo "unknown provider $provider" >&2
    exit 2
    ;;
esac

port=${UNFOG_LIVE_PORT:-5244}
work=$(mktemp -d)
chmod 700 "$work"
proxy=

cleanup() {
    status=$?
    if [ -n "$proxy" ]; then
        kill "$proxy" 2>/dev/null || true
        wait "$proxy" 2>/dev/null || true
    fi
    # NOTE: nix-shell leaves mitmdump behind it; the listener is what
    # must go, so it is found by its port.
    for pid in $(ss -ltnpH "sport = :$port" 2>/dev/null | grep -oE 'pid=[0-9]+' | cut -d= -f2); do
        kill "$pid" 2>/dev/null || true
    done
    # NOTE: and waits for it to go, so nothing listens once this exits.
    for _ in $(seq 50); do
        ss -ltnH "sport = :$port" 2>/dev/null | grep -q . || break
        sleep 0.1
    done
    for pid in $(ss -ltnpH "sport = :$port" 2>/dev/null | grep -oE 'pid=[0-9]+' | cut -d= -f2); do
        kill -9 "$pid" 2>/dev/null || true
    done
    rm -rf "$work"
    exit "$status"
}
trap cleanup EXIT INT TERM

user=$(sh -c "$user_command")
(
    umask 077
    password=$(sh -c "$password_command")
    printf 'user = "%s:%s"\n' "$user" "$password" >"$work/curl.conf"
)

# PROPFIND of depth 0 asking `prop`, the first href of a multistatus
# response other than the request's own.
propfind() {
    nix-shell -p curl --run "curl -sS -K '$work/curl.conf' -X PROPFIND \
        -H 'Depth: 0' -H 'Content-Type: application/xml' \
        --data '<?xml version=\"1.0\"?><d:propfind xmlns:d=\"DAV:\" \
xmlns:c=\"urn:ietf:params:xml:ns:caldav\"><d:prop>$2</d:prop></d:propfind>' \
        '$upstream$1'" |
        tr -d '\n' | grep -oE '<[a-zA-Z]*:?href>[^<]*' | sed 's/.*>//' |
        grep -vx "$1" | head -n1
}

principal=$(propfind "$entry" '<d:current-user-principal/>')
home=$(propfind "$principal" '<c:calendar-home-set/>')
[ -n "$home" ] || { echo "no calendar home found" >&2; exit 1; }

export UNFOG_E2E_HOME=$home
export UNFOG_E2E_PRINCIPAL=$principal
export UNFOG_E2E_GUARD_LOG=$work/guard.log
export UNFOG_E2E_GUARD_ALLOW=$work/allow
export UNFOG_E2E_GUARD_PROTECT=$work/protect
export UNFOG_E2E_UPSTREAM_HOST=${upstream#https://}
: >"$UNFOG_E2E_GUARD_LOG"
: >"$UNFOG_E2E_GUARD_ALLOW"
: >"$UNFOG_E2E_GUARD_PROTECT"

# NOTE: a lent calendar is the only collection the run may write in, never
# to the collection itself nor to an item it held before: those are listed
# protected, read only.
if [ -n "${UNFOG_E2E_CALENDAR:-}" ]; then
    lent=$home$UNFOG_E2E_CALENDAR/
    printf '%s\n' "$lent" >"$UNFOG_E2E_GUARD_ALLOW"
    {
        printf '%s\n' "$lent"
        nix-shell -p curl --run "curl -sS -K '$work/curl.conf' -X PROPFIND -H 'Depth: 1' \
            -H 'Content-Type: application/xml' --data '<?xml version=\"1.0\"?><d:propfind \
xmlns:d=\"DAV:\"><d:prop><d:getetag/></d:prop></d:propfind>' '$upstream$lent'" |
            tr -d '\n' | grep -oE '<[a-zA-Z]*:?href>[^<]*' | sed 's/.*>//'
    } >"$UNFOG_E2E_GUARD_PROTECT"
    echo "lent calendar: $(grep -vc '/$' "$UNFOG_E2E_GUARD_PROTECT") item(s) it held, protected"
fi

# NOTE: a proxy left on the port would carry this run's credentials to its
# own provider: refuse to start rather than talk through it.
if ss -ltnH "sport = :$port" | grep -q .; then
    echo "port $port is taken: another proxy runs; stop it first" >&2
    exit 1
fi

nix-shell -p mitmproxy --run "mitmdump -q --listen-host 127.0.0.1 -p $port \
    --mode reverse:$upstream -s tests/live-guard.py" &
proxy=$!

for _ in $(seq 100); do
    (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null && break
    sleep 0.2
done

# NOTE: the listener must be the one just started, not one it lost the port
# to.
kill -0 "$proxy" 2>/dev/null || { echo "the guard proxy did not start" >&2; exit 1; }

export UNFOG_E2E_URL=http://127.0.0.1:$port/
export UNFOG_E2E_USER=$user
# NOTE: a script, so the command reads the same whether a shell runs it
# or not; it holds the pass command, never the password.
printf '#!/bin/sh\n%s\n' "$password_command" >"$work/password.sh"
chmod 700 "$work/password.sh"
export UNFOG_E2E_PASSWORD_COMMAND=$work/password.sh
export UNFOG_E2E_RUN=${UNFOG_E2E_RUN:-$(date +%s)}
mkdir -p target/tmp

set +e
TMPDIR=$PWD/target/tmp nix develop --command cargo test --test caldav -- \
    --ignored --nocapture "$@"
status=$?
TMPDIR=$PWD/target/tmp nix develop --command cargo test --test caldav -- \
    --ignored --exact zz_the_leftovers_are_swept >/dev/null 2>&1
set -e

if [ -s "$UNFOG_E2E_GUARD_LOG" ]; then
    echo "the guard refused:" >&2
    cat "$UNFOG_E2E_GUARD_LOG" >&2
    status=1
fi

exit "$status"
