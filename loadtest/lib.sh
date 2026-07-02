#!/usr/bin/env bash
# Shared helpers for the espial load-test harness (loadtest/*.sh).

# Docker Desktop on Windows/Git-Bash mangles container-side paths in bind-mount
# arguments (e.g. the "/data" in "-v myvolume:/data") as if they were host
# paths, unless MSYS_NO_PATHCONV=1 is set. Use this for `docker run`/`exec`/
# `volume` etc. No-op on other platforms.
dk() {
  MSYS_NO_PATHCONV=1 docker "$@"
}

# `docker build <context>` takes a real host path and needs normal MSYS path
# translation (the opposite of dk() above) -- keep it separate.
dk_build() {
  docker build "$@"
}

# Converts a host path to a form Docker Desktop accepts unambiguously even
# under MSYS_NO_PATHCONV=1 (used for bind-mount sources passed to dk()).
# No-op where cygpath isn't available (non-Windows).
to_docker_path() {
  if command -v cygpath >/dev/null 2>&1; then
    cygpath -w "$1"
  else
    printf '%s' "$1"
  fi
}

log() { printf '%s\n' "$*" >&2; }

wait_for_http() {
  local url="$1" tries="${2:-30}" i=0
  until curl -s -o /dev/null "$url"; do
    i=$((i + 1))
    if [ "$i" -ge "$tries" ]; then
      log "error: $url did not become ready after $tries tries"
      return 1
    fi
    sleep 0.5
  done
}
