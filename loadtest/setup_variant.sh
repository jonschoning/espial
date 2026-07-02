#!/usr/bin/env bash
# Creates a volume + DB + test user + sample bookmarks, then starts an espial
# container configured for one load-test "variant" (a specific combination of
# RTS flags / MALLOC_ARENA_MAX). Reads its remaining configuration (IMAGE,
# USERNAME, PASSWORD, BOOKMARK_FILE_ABS, MEMORY, MEMORY_SWAP, CPUS,
# CPUSET_CPUS) from the environment -- see run.sh.
#
# Usage: setup_variant.sh <variant_name> <port> <rts_flags> [malloc_arena_max]
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$DIR/lib.sh"

NAME="$1"
PORT="$2"
RTS_FLAGS="$3"
ARENA_MAX="${4:-}"

: "${IMAGE:?IMAGE must be set}"
: "${USERNAME:?USERNAME must be set}"
: "${PASSWORD:?PASSWORD must be set}"
: "${BOOKMARK_FILE_ABS:?BOOKMARK_FILE_ABS must be set}"
: "${MEMORY:?MEMORY must be set}"
: "${MEMORY_SWAP:?MEMORY_SWAP must be set}"
: "${CPUS:?CPUS must be set}"

CONTAINER="espial-lt-$NAME"
VOLUME="espial-lt-$NAME-data"

dk volume rm "$VOLUME" >/dev/null 2>&1 || true
dk volume create "$VOLUME" >/dev/null

dk run --rm -v "$VOLUME:/data" -e SQLITE_DATABASE=/data/espial.sqlite3 \
  --entrypoint ./migration "$IMAGE" createdb >/dev/null

dk run --rm -v "$VOLUME:/data" -e SQLITE_DATABASE=/data/espial.sqlite3 \
  --entrypoint ./migration "$IMAGE" createuser \
  --userName "$USERNAME" --userPassword "$PASSWORD" >/dev/null

BOOKMARK_FILE_DOCKER="$(to_docker_path "$BOOKMARK_FILE_ABS")"
dk run --rm -v "$VOLUME:/data" -v "$BOOKMARK_FILE_DOCKER:/tmp/sample-bookmarks.json" \
  -e SQLITE_DATABASE=/data/espial.sqlite3 \
  --entrypoint ./migration "$IMAGE" importbookmarks \
  --userName "$USERNAME" --bookmarkFile /tmp/sample-bookmarks.json >/dev/null

ENV_ARGS=()
if [ -n "$ARENA_MAX" ]; then
  ENV_ARGS+=(-e "MALLOC_ARENA_MAX=$ARENA_MAX")
fi

CPU_ARGS=(--cpus="$CPUS")
if [ -n "${CPUSET_CPUS:-}" ]; then
  CPU_ARGS+=(--cpuset-cpus="$CPUSET_CPUS")
fi

dk rm -f "$CONTAINER" >/dev/null 2>&1 || true
# shellcheck disable=SC2206 # RTS_FLAGS is an intentionally word-split flag list
RTS_ARGS=($RTS_FLAGS)
dk run -d --name "$CONTAINER" \
  --memory="$MEMORY" --memory-swap="$MEMORY_SWAP" "${CPU_ARGS[@]}" \
  -v "$VOLUME:/data" -e SQLITE_DATABASE=/data/espial.sqlite3 \
  "${ENV_ARGS[@]}" \
  -p "$PORT:3000" \
  "$IMAGE" ./espial +RTS "${RTS_ARGS[@]}" >/dev/null

wait_for_http "http://localhost:$PORT/"
