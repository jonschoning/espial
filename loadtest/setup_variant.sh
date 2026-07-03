#!/usr/bin/env bash
# Creates a volume + DB + test user + sample bookmarks, then starts an espial
# container configured for one load-test "variant" (a specific combination of
# RTS flags / extra env vars). Reads its remaining configuration
# (LOADTEST_IMAGE, LOADTEST_USERNAME, LOADTEST_PASSWORD,
# LOADTEST_BOOKMARK_FILE_ABS, LOADTEST_MEMORY, LOADTEST_MEMORY_SWAP,
# LOADTEST_CPUS, LOADTEST_CPUSET_CPUS) from the environment -- see run.sh.
# The last four are optional: if unset/empty, the corresponding docker flag
# is omitted entirely so the container gets Docker's own default limits.
#
# Usage: setup_variant.sh <variant_name> <port> <rts_flags> [env_kv ...]
# where each env_kv is a "KEY=VALUE" string to pass through as -e KEY=VALUE.
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$DIR/lib.sh"

NAME="$1"
PORT="$2"
RTS_FLAGS="$3"
shift 3
ENV_KV=("$@")

: "${LOADTEST_IMAGE:?LOADTEST_IMAGE must be set}"
: "${LOADTEST_USERNAME:?LOADTEST_USERNAME must be set}"
: "${LOADTEST_PASSWORD:?LOADTEST_PASSWORD must be set}"
: "${LOADTEST_BOOKMARK_FILE_ABS:?LOADTEST_BOOKMARK_FILE_ABS must be set}"

CONTAINER="espial-lt-$NAME"
VOLUME="espial-lt-$NAME-data"

dk volume rm "$VOLUME" >/dev/null 2>&1 || true
dk volume create "$VOLUME" >/dev/null

dk run --rm -v "$VOLUME:/data" -e SQLITE_DATABASE=/data/espial.sqlite3 \
  --entrypoint ./migration "$LOADTEST_IMAGE" createdb --silent True >/dev/null

dk run --rm -v "$VOLUME:/data" -e SQLITE_DATABASE=/data/espial.sqlite3 \
  --entrypoint ./migration "$LOADTEST_IMAGE" createuser \
  --userName "$LOADTEST_USERNAME" --userPassword "$LOADTEST_PASSWORD" >/dev/null

BOOKMARK_FILE_DOCKER="$(to_docker_path "$LOADTEST_BOOKMARK_FILE_ABS")"
dk run --rm -v "$VOLUME:/data" -v "$BOOKMARK_FILE_DOCKER:/tmp/sample-bookmarks.json" \
  -e SQLITE_DATABASE=/data/espial.sqlite3 \
  --entrypoint ./migration "$LOADTEST_IMAGE" importbookmarks \
  --userName "$LOADTEST_USERNAME" --bookmarkFile /tmp/sample-bookmarks.json >/dev/null

ENV_ARGS=()
for kv in "${ENV_KV[@]}"; do
  ENV_ARGS+=(-e "$kv")
done

RESOURCE_ARGS=()
[ -n "${LOADTEST_MEMORY:-}" ] && RESOURCE_ARGS+=(--memory="$LOADTEST_MEMORY")
[ -n "${LOADTEST_MEMORY_SWAP:-}" ] && RESOURCE_ARGS+=(--memory-swap="$LOADTEST_MEMORY_SWAP")
[ -n "${LOADTEST_CPUS:-}" ] && RESOURCE_ARGS+=(--cpus="$LOADTEST_CPUS")
[ -n "${LOADTEST_CPUSET_CPUS:-}" ] && RESOURCE_ARGS+=(--cpuset-cpus="$LOADTEST_CPUSET_CPUS")

dk rm -f "$CONTAINER" >/dev/null 2>&1 || true
# shellcheck disable=SC2206 # RTS_FLAGS is an intentionally word-split flag list
RTS_ARGS=($RTS_FLAGS)
dk run -d --name "$CONTAINER" \
  "${RESOURCE_ARGS[@]}" \
  -v "$VOLUME:/data" -e SQLITE_DATABASE=/data/espial.sqlite3 \
  "${ENV_ARGS[@]}" \
  -p "$PORT:3000" \
  "$LOADTEST_IMAGE" ./espial +RTS "${RTS_ARGS[@]}" >/dev/null

wait_for_http "http://localhost:$PORT/"
