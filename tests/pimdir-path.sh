#!/usr/bin/env bash
# The pimdir path end to end: neverest syncs scratch calendars into a
# scratch store, unfog clarifies and moves a task over the store, neverest
# pushes, and the server's bytes are checked.
#
#   tests/pimdir-path.sh [radicale|fastmail]
#
# Radicale (tests/radicale.sh, running) by default; fastmail goes through
# the guard proxy as tests/live.sh does. The store lives in a private
# temporary directory, never under ~/.local/state/neverest. neverest is the
# one built beside this checkout (../neverest/target/debug/neverest), on the
# same io-pimdir as unfog. Run from the repository root.

set -euo pipefail

target=${1:-radicale}
port=${UNFOG_LIVE_PORT:-5244}
here=$PWD
work=$(mktemp -d)
chmod 700 "$work"
proxy=
run=$(date +%s)
neverest=$here/../neverest/target/debug/neverest
unfog=$here/target/debug/unfog
created=()

case $target in
radicale)
    base=http://127.0.0.1:5243
    home=/test/
    user=test
    password_command="printf test"
    ;;
fastmail)
    upstream=https://caldav.fastmail.com
    base=http://127.0.0.1:$port
    home=/dav/calendars/user/pimalaya@fastmail.org/
    user=pimalaya@fastmail.org
    password_command="pass show pimalaya/fastmail-webdav | head -n1"
    ;;
*)
    echo "unknown target $target" >&2
    exit 2
    ;;
esac

printf '#!/bin/sh\n%s\n' "$password_command" >"$work/password.sh"
chmod 700 "$work/password.sh"
(
    umask 077
    printf 'user = "%s:%s"\n' "$user" "$("$work/password.sh")" >"$work/curl.conf"
)

http() {
    nix-shell -p curl --run "curl -sS -K '$work/curl.conf' $*"
}

cleanup() {
    status=$?
    for path in "${created[@]}"; do
        http "-X DELETE '$base$path' -o /dev/null" || true
    done
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
    if [ -s "$work/guard.log" ]; then
        echo "the guard refused:" >&2
        cat "$work/guard.log" >&2
        status=1
    fi
    rm -rf "$work"
    exit "$status"
}
trap cleanup EXIT INT TERM

if [ "$target" = fastmail ]; then
    if ss -ltnH "sport = :$port" | grep -q .; then
        echo "port $port is taken: another proxy runs" >&2
        exit 1
    fi
    : >"$work/guard.log"
    UNFOG_E2E_HOME=$home UNFOG_E2E_GUARD_LOG=$work/guard.log \
        UNFOG_E2E_PRINCIPAL=/dav/principals/user/$user/ \
        UNFOG_E2E_GUARD_EMPTY=${home}Inbox/ \
        UNFOG_E2E_UPSTREAM_HOST=caldav.fastmail.com \
        nix-shell -p mitmproxy --run "mitmdump -q --listen-host 127.0.0.1 -p $port \
            --mode reverse:$upstream -s tests/live-guard.py" &
    proxy=$!
    for _ in $(seq 100); do
        (exec 3<>"/dev/tcp/127.0.0.1/$port") 2>/dev/null && break
        sleep 0.2
    done
fi

# Creates the scratch calendar `unfog-e2e-<run>-<1>`, its id left in
# `$scratch_id` (no subshell, so the cleanup sees it).
scratch() {
    local id=unfog-e2e-$run-$1
    local path=$home$id/
    local status
    status=$(http "-X MKCALENDAR -H 'Content-Type: application/xml' -o /dev/null \
        -w '%{http_code}' --data '<?xml version=\"1.0\"?><C:mkcalendar xmlns:D=\"DAV:\" \
xmlns:C=\"urn:ietf:params:xml:ns:caldav\"><D:set><D:prop><D:displayname>$id</D:displayname>\
<C:supported-calendar-component-set><C:comp name=\"VTODO\"/></C:supported-calendar-component-set>\
</D:prop></D:set></C:mkcalendar>' '$base$path'")
    [ "$status" = 201 ] || { echo "MKCALENDAR $id answered $status" >&2; exit 1; }
    created+=("$path")
    scratch_id=$id
}

scratch p0
from=$scratch_id
scratch p1
to=$scratch_id

printf 'BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Other Client//Tasks 1.0//EN\r\nBEGIN:VTODO\r\nUID:pimdir-1@other.example\r\nDTSTAMP:20261001T080000Z\r\nSUMMARY:Call Jo about the lease\r\nCATEGORIES:work,Inbox\r\nX-OTHER-CLIENT-STATE:kept\r\nEND:VTODO\r\nEND:VCALENDAR\r\n' >"$work/task.ics"
printf 'BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:-//Other Client//Tasks 1.0//EN\r\nBEGIN:VTODO\r\nUID:pimdir-2@other.example\r\nDTSTAMP:20261001T080000Z\r\nSUMMARY:Move me\r\nCATEGORIES:Inbox\r\nEND:VTODO\r\nEND:VCALENDAR\r\n' >"$work/move.ics"
http "-X PUT -H 'Content-Type: text/calendar' --data-binary @$work/task.ics \
    '$base$home$from/task.ics' -o /dev/null"
http "-X PUT -H 'Content-Type: text/calendar' --data-binary @$work/move.ics \
    '$base$home$from/move.ics' -o /dev/null"

cat >"$work/neverest.toml" <<EOF
[accounts.e2e]
default = true
store.root = "$work/store"
caldav.server = "$base$home"
caldav.auth.basic.username = "$user"
caldav.auth.basic.password.command = "$work/password.sh"
EOF

cat >"$work/unfog.toml" <<EOF
[accounts.e2e]
default = true
pimdir.root = "$work/store"
EOF

nv() { "$neverest" -c "$work/neverest.toml" "$@"; }
uf() { "$unfog" -c "$work/unfog.toml" -b pimdir --json "$@"; }

echo "== init and first sync"
nv init >"$work/init.out" 2>&1 || { cat "$work/init.out" >&2; exit 1; }
nv sync >"$work/sync1.out" 2>&1 || { cat "$work/sync1.out" >&2; exit 1; }

echo "== unfog over the store"
has() { case "$1" in *"$2"*) ;; *) echo "missing $2 in: $1" >&2; exit 1 ;; esac; }
has "$(uf gtd inbox)" pimdir-1@other.example
uf gtd clarify pimdir-1 --next -C phone >/dev/null
has "$(uf gtd next)" pimdir-1@other.example
# NOTE: the store names a calendar after its source: `caldav/<calendar>`.
uf gtd clarify pimdir-2 --next --to "caldav/$to" >/dev/null

# How many resources of the target hold pimdir-2, by their bodies: a moved
# task keeps its source's resource name, a created one takes its UID's.
in_target() {
    http "-X REPORT -H 'Depth: 1' -H 'Content-Type: application/xml' --data \
'<c:calendar-query xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\"><d:prop>\
<c:calendar-data/></d:prop><c:filter><c:comp-filter name=\"VCALENDAR\"/></c:filter>\
</c:calendar-query>' '$base$home$to/'" | grep -c 'UID:pimdir-2@other.example' || true
}

echo "== second sync"
# NOTE: a warning makes neverest exit non-zero; the state is checked
# whatever it says.
nv sync >"$work/sync2.out" 2>&1 || echo "second sync exited $?"
after2=$(http "-o /dev/null -w '%{http_code}' '$base$home$from/move.ics'")
echo "move: source GET $after2 right after the second sync"

echo "== server state"
served=$(http "'$base$home$from/task.ics'" | tr -d '\r')
echo "$served" | grep -qx 'CATEGORIES:work' || { echo "$served" >&2; exit 1; }
echo "$served" | grep -qx 'CATEGORIES:phone' || { echo "$served" >&2; exit 1; }
echo "$served" | grep -qx 'X-OTHER-CLIENT-STATE:kept' || { echo "$served" >&2; exit 1; }
echo "clarified: CATEGORIES and the X- line as expected on the server"

source_left=$(http "-o /dev/null -w '%{http_code}' '$base$home$from/move.ics'")
target_listing=$(in_target)
echo "move: source GET $source_left, target holds it: $target_listing"
echo "move: unfog lists it in $(uf gtd next | tr '{' '\n' | grep pimdir-2 | grep -o '"tasklist":"[^"]*"')"
grep -iE 'park|refus|error|warn' "$work/sync2.out" | head -5 || true

echo "== third sync: the held delete goes once its add was pushed"
nv sync >"$work/sync3.out" 2>&1 || echo "third sync exited $?"
source_left=$(http "-o /dev/null -w '%{http_code}' '$base$home$from/move.ics'")
target_listing=$(in_target)
echo "move: source GET $source_left, target holds it: $target_listing, after the third sync"

# NOTE: either the add was delivered (by a MOVE where the server has one,
# so Fastmail, keeping one UID per account, takes it) and the held delete
# followed, or the server refused the add and the task stays where it was:
# never both gone, never the delete alone.
if [ "$target_listing" -ge 1 ]; then
    [ "$source_left" = 404 ] || { cat "$work/sync3.out" >&2; exit 1; }
else
    [ "$source_left" = 200 ] || { cat "$work/sync3.out" >&2; exit 1; }
    echo "move: the add was refused, the task kept where it was:"
    grep -iE 'park|refus|uid|403|409' "$work/sync2.out" "$work/sync3.out" | head -5 || true
fi
