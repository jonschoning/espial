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

# The discard/null-device path for the current platform. Paths inside a curl
# -K config file aren't subject to MSYS's argv path translation, so a native
# Windows curl.exe treats "/dev/null" as a literal (nonexistent) relative
# path rather than the null device -- it needs the Windows null device name.
null_device() {
  if command -v cygpath >/dev/null 2>&1; then
    printf 'NUL'
  else
    printf '/dev/null'
  fi
}

log() { printf '%s\n' "$*" >&2; }

require_jq() {
  if ! command -v jq >/dev/null 2>&1; then
    log "error: jq is required by the load-test harness (https://jqlang.org) -- please install it"
    exit 1
  fi
}

require_yq() {
  if ! command -v yq >/dev/null 2>&1; then
    log "error: yq is required by the load-test harness (https://github.com/mikefarah/yq) -- please install it"
    exit 1
  fi
}

# ---- config.yaml accessors ---------------------------------------------------
# See loadtest/config.yaml for the schema. Config is authored as YAML (so
# entries can be commented out) but queried as JSON: load_config converts it
# once into a temp file and points $CONFIG_JSON at that, which all of these
# read from via jq. Callers must call load_config before using them, and
# clean up $CONFIG_JSON themselves on exit (it's a temp file).

load_config() {
  local yaml="$1"
  if [ ! -f "$yaml" ]; then
    log "error: config file not found: $yaml"
    exit 1
  fi
  CONFIG_JSON=$(mktemp)
  if ! yq -o=json '.' "$yaml" > "$CONFIG_JSON"; then
    log "error: failed to parse $yaml as YAML"
    exit 1
  fi
}

# Generic scalar/array lookup, e.g. cfg '.params.login.rounds'.
# Arrays are space-joined so the result drops straight into a bash word list.
# A missing/commented-out key resolves to "" (jq's null), same as an
# explicit "" in the YAML, so callers can treat both as "not set".
cfg() {
  jq -r "($1) | if type == \"array\" then join(\" \") elif . == null then \"\" else . end" "$CONFIG_JSON"
}

# Space-separated variant names to run when $LOADTEST_VARIANTS isn't overridden.
default_variants() {
  cfg '.defaultVariants'
}

# Space-separated list of every variant name defined in config.yaml.
variant_names() {
  jq -r '.variants | keys | join(" ")' "$CONFIG_JSON"
}

# True (exit 0) if variant "$1" is defined in config.yaml.
variant_defined() {
  [ "$(jq -r --arg n "$1" '.variants | has($n)' "$CONFIG_JSON")" = "true" ]
}

# The RTS flags string for variant "$1".
variant_rts_flags() {
  jq -r --arg n "$1" '.variants[$n].rtsFlags' "$CONFIG_JSON"
}

# The image override for variant "$1", or "" if it just uses $LOADTEST_IMAGE.
variant_image() {
  jq -r --arg n "$1" '.variants[$n].image // ""' "$CONFIG_JSON"
}

# One "KEY=VALUE" per line for variant "$1"'s env map (empty output if none).
# Piped through tr -d '\r': on Windows, jq's native binary writes CRLF line
# endings, and unlike $(...) command substitution, `readarray -t` (this
# function's caller) only strips the trailing \n, not \r -- left unstripped,
# that \r ends up baked into the container's actual env var value.
variant_env_kv() {
  jq -r --arg n "$1" '.variants[$n].env // {} | to_entries[] | "\(.key)=\(.value)"' "$CONFIG_JSON" | tr -d '\r'
}

# Space-separated route names to run when $LOADTEST_TEST_ROUTES isn't overridden.
default_test_routes() {
  cfg '.defaultTestRoutes'
}

# Space-separated list of every route name defined in config.yaml.
route_names() {
  jq -r '.routes | keys | join(" ")' "$CONFIG_JSON"
}

# True (exit 0) if route "$1" is defined in config.yaml.
route_defined() {
  [ "$(jq -r --arg n "$1" '.routes | has($n)' "$CONFIG_JSON")" = "true" ]
}

# A field of route "$1", e.g. route_field db path -> "/u:{username}".
route_field() {
  jq -r --arg n "$1" --arg f "$2" '.routes[$n][$f]' "$CONFIG_JSON"
}

# Substitutes "{username}" in a route path template with "$2".
route_expand_path() {
  local template="$1" username="$2"
  printf '%s' "${template//\{username\}/$username}"
}

# cgroup CPU-quota-throttling counters for container "$1", cumulative since
# container start. Echoes "nr_periods=N nr_throttled=N throttled_usec=N".
# Tries cgroup v2's single cpu.stat first, falling back to v1's path (whose
# throttled_time is nanoseconds, not microseconds -- converted here so
# callers only ever see usec). All-zero if neither file is reachable (e.g.
# container already stopped).
cgroup_cpu_stat() {
  local container="$1" raw
  raw=$(dk exec "$container" cat /sys/fs/cgroup/cpu.stat 2>/dev/null) \
    || raw=$(dk exec "$container" cat /sys/fs/cgroup/cpu/cpu.stat 2>/dev/null)
  awk '
    /^nr_periods/     { periods = $2 }
    /^nr_throttled/   { throttled = $2 }
    /^throttled_time/ { usec = int($2 / 1000) }
    /^throttled_usec/ { usec = $2 }
    END {
      printf "nr_periods=%d nr_throttled=%d throttled_usec=%d", periods + 0, throttled + 0, usec + 0
    }
  ' <<< "$raw"
}

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
