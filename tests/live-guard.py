# mitmproxy addon guarding a live provider for the CalDAV suite.
#
# The suite (tests/caldav.rs) and unfog speak plain HTTP to this reverse
# proxy, which carries TLS to the provider. It forwards only what touches
# the run's scratch collections and refuses everything else with a 403,
# appending each refusal to the file UNFOG_E2E_GUARD_LOG names, so a run
# that reached for anything else fails loudly instead of touching it:
#
# - any method on a path inside the calendar home whose first segment
#   starts with `unfog-e2e-`, or on a path listed in the file
#   UNFOG_E2E_GUARD_ALLOW names (one prefix per line, for providers whose
#   scratch ids carry no prefix), but on a path listed in the file
#   UNFOG_E2E_GUARD_PROTECT names, exactly, only a read (GET, HEAD, OPTIONS,
#   PROPFIND, REPORT): a calendar a provider lends the run and the items it
#   held before, which the run must not change;
# - a PROPFIND of depth 0 or 1 on the home itself, whose multistatus is
#   cut down to the home and the scratch collections, so no client ever
#   sees another collection of the account;
# - a PROPFIND of depth 0 on the principal UNFOG_E2E_PRINCIPAL names.
# - a PROPFIND of depth 0 on the path UNFOG_E2E_GUARD_EMPTY names, answered
#   here with an empty multistatus, never forwarded: a probe a client takes
#   as stating nothing (neverest asks the scheduling inbox for a default
#   calendar), which must not hand it a collection outside the run's.
#
# Nothing here logs a header: credentials pass through, never written.

import os
import re
from urllib.parse import unquote, urlsplit

from mitmproxy import http

PREFIX = "unfog-e2e-"
HOME = "/" + os.environ["UNFOG_E2E_HOME"].strip("/") + "/"
PRINCIPAL = os.environ.get("UNFOG_E2E_PRINCIPAL", "")
LOG = os.environ["UNFOG_E2E_GUARD_LOG"]
ALLOW = os.environ.get("UNFOG_E2E_GUARD_ALLOW", "")
UPSTREAM = os.environ.get("UNFOG_E2E_UPSTREAM_HOST", "")
PROTECT = os.environ.get("UNFOG_E2E_GUARD_PROTECT", "")
EMPTY = os.environ.get("UNFOG_E2E_GUARD_EMPTY", "")
READS = {"GET", "HEAD", "OPTIONS", "PROPFIND", "REPORT"}

RESPONSE = re.compile(
    r"<(?P<ns>(?:[A-Za-z0-9_-]+:)?)response\b.*?</(?P=ns)response>", re.DOTALL
)
HREF = re.compile(r"<(?:[A-Za-z0-9_-]+:)?href>\s*([^<]*?)\s*</", re.DOTALL)


def path_of(url):
    path = unquote(urlsplit(url).path) if "://" in url else unquote(url.split("?")[0])
    return path


def allowed_prefixes():
    if not ALLOW or not os.path.exists(ALLOW):
        return []
    with open(ALLOW) as lines:
        return [line.strip() for line in lines if line.strip()]


def protected_paths():
    if not PROTECT or not os.path.exists(PROTECT):
        return set()
    with open(PROTECT) as lines:
        return {line.strip().rstrip("/") for line in lines if line.strip()}


def is_protected(path):
    return path.rstrip("/") in protected_paths()


def is_scratch(path):
    if path.startswith(HOME) and path[len(HOME):].startswith(PREFIX):
        return True
    # NOTE: a collection is asked for with or without its trailing slash.
    path = path.rstrip("/") + "/"
    return any(path.startswith(prefix.rstrip("/") + "/") for prefix in allowed_prefixes())


def is_home(path):
    return path.rstrip("/") + "/" == HOME


def refuse(flow, why):
    line = f"{flow.request.method} {flow.request.path} ({why})\n"
    # NOTE: the refusal first, so a log that cannot be written never lets
    # the request through.
    flow.response = http.Response.make(
        403, f"unfog-e2e guard refused {line}".encode(), {"Content-Type": "text/plain"}
    )
    try:
        with open(LOG, "a") as log:
            log.write(line)
    except OSError:
        pass


def request(flow: http.HTTPFlow) -> None:
    # NOTE: the provider's authority, its port included, which a reverse
    # proxy would otherwise drop from the Host header.
    if UPSTREAM:
        flow.request.headers["Host"] = UPSTREAM

    path = path_of(flow.request.path)
    method = flow.request.method.upper()
    depth = flow.request.headers.get("Depth", "0").strip()

    if EMPTY and path.rstrip("/") == EMPTY.rstrip("/") and method == "PROPFIND" and depth == "0":
        flow.response = http.Response.make(
            207, b'<?xml version="1.0"?><d:multistatus xmlns:d="DAV:"/>',
            {"Content-Type": "application/xml; charset=utf-8"},
        )
        return
    if is_protected(path) and method not in READS:
        refuse(flow, "a lent collection or an item it held before the run")
        return
    if is_scratch(path):
        return
    if is_home(path) and method == "PROPFIND" and depth in ("0", "1"):
        return
    if PRINCIPAL and path.rstrip("/") == PRINCIPAL.rstrip("/") and method == "PROPFIND" and depth == "0":
        return
    refuse(flow, "outside the scratch collections")


def response(flow: http.HTTPFlow) -> None:
    path = path_of(flow.request.path)
    if not (is_home(path) and flow.request.method.upper() == "PROPFIND"):
        return
    if flow.response is None or flow.response.status_code != 207:
        return

    body = flow.response.get_text()

    def keep(match):
        hrefs = HREF.findall(match.group(0))
        target = path_of(hrefs[0]) if hrefs else ""
        return match.group(0) if is_home(target) or is_scratch(target) else ""

    flow.response.set_text(RESPONSE.sub(keep, body))
