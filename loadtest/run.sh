#!/usr/bin/env bash
# Espial load-test harness: builds an image, spins up one container per
# "variant" (a combination of RTS flags / MALLOC_ARENA_MAX / CPU & memory
# limits), fires repeated concurrent-request bursts at each on the login page
# and/or the bookmark page, and reports a side-by-side memory + throughput
# comparison. Cleans up all containers/volumes it created on exit.
#
# All parameters are environment variables with defaults below; override any
# of them, e.g.:
#
#   VARIANTS="baseline arena" ROUNDS=4 loadtest/run.sh
#   CPUSET_CPUS=0 VARIANTS="baseline arena" loadtest/run.sh   # pin to 1 real core
#
# Built-in variants (RTS flags | MALLOC_ARENA_MAX):
#   baseline  -T                                              | (unset)
#   arena     -T                                              | 1
#   rts       -T -F1.3 -Fd2 --disable-delayed-os-memory-return | (unset)
#   both      -T -F1.3 -Fd2 --disable-delayed-os-memory-return | 1
#
# Add a custom one-off variant by naming it in VARIANTS and exporting a
# same-named env var as "RTS_FLAGS|MALLOC_ARENA_MAX", e.g.:
#
#   MYVAR="-T -A64m|1" VARIANTS="baseline MYVAR" loadtest/run.sh
set -uo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$DIR/.." && pwd)"
source "$DIR/lib.sh"

# ---- parameters (override via environment) --------------------------------
: "${IMAGE:=espial-loadtest:latest}"
: "${DOCKERFILE:=Dockerfile.buildkit}"
: "${SKIP_BUILD:=0}"
: "${VARIANTS:=baseline arena rts both}"
: "${TEST_ROUTES:=login db}"
: "${ROUNDS:=8}"                # login-route rounds
: "${CONCURRENCY:=10}"          # login-route concurrency per round
: "${ROUND_WAIT:=10}"           # seconds between login-route rounds
: "${DB_ROUNDS:=3}"             # bookmark-page rounds
: "${DB_CONCURRENCY:=20}"       # bookmark-page concurrency per round
: "${DB_ROUND_WAIT:=2}"
: "${MEMORY:=430m}"
: "${MEMORY_SWAP:=646m}"
: "${CPUS:=1}"                  # CFS CPU-time quota (doesn't limit visible core count)
: "${CPUSET_CPUS:=}"            # e.g. "0" to pin to one real core (affects glibc's
                                 # default arena-count heuristic; empty = unset)
: "${USERNAME:=loadtest}"
: "${PASSWORD:=loadtestpass123}"
: "${BOOKMARK_FILE:=sample-bookmarks.json}"
: "${RESULTS_DIR:=$DIR/results}"
: "${BASE_PORT:=3100}"
: "${KEEP:=0}"                  # 1 = leave containers/volumes running for inspection
export IMAGE USERNAME PASSWORD MEMORY MEMORY_SWAP CPUS CPUSET_CPUS

BOOKMARK_FILE_ABS="$REPO_ROOT/$BOOKMARK_FILE"
if [ ! -f "$BOOKMARK_FILE_ABS" ]; then
  log "error: bookmark file not found: $BOOKMARK_FILE_ABS"
  exit 1
fi
export BOOKMARK_FILE_ABS

# ---- variant presets -------------------------------------------------------
# Prints "RTS_FLAGS|MALLOC_ARENA_MAX" for a known preset, or for a custom
# variant looks up an env var of the same name holding that same format.
variant_spec() {
  case "$1" in
    baseline) echo "-T|" ;;
    arena) echo "-T|1" ;;
    rts) echo "-T -F1.3 -Fd2 --disable-delayed-os-memory-return|" ;;
    both) echo "-T -F1.3 -Fd2 --disable-delayed-os-memory-return|1" ;;
    *)
      local custom="${!1:-}"
      if [ -n "$custom" ]; then
        echo "$custom"
      else
        log "error: unknown variant '$1'."
        log "  known presets: baseline arena rts both"
        log "  for a custom variant, export $1=\"RTS_FLAGS|MALLOC_ARENA_MAX\" first"
        exit 1
      fi
      ;;
  esac
}

mkdir -p "$RESULTS_DIR"
rm -f "$RESULTS_DIR"/*.txt

if [ "$SKIP_BUILD" != "1" ]; then
  log "==> building $IMAGE from $DOCKERFILE"
  dk_build -f "$REPO_ROOT/$DOCKERFILE" -t "$IMAGE" "$REPO_ROOT"
else
  log "==> SKIP_BUILD=1, using existing image $IMAGE"
fi

read -r -a VARIANT_NAMES <<< "$VARIANTS"

cleanup() {
  if [ "$KEEP" = "1" ]; then
    log "==> KEEP=1, leaving containers/volumes in place (docker rm -f espial-lt-<variant>; docker volume rm espial-lt-<variant>-data to clean up later)"
    return
  fi
  log "==> cleaning up containers and volumes"
  for v in "${VARIANT_NAMES[@]}"; do
    dk rm -f "espial-lt-$v" >/dev/null 2>&1 || true
    dk volume rm "espial-lt-$v-data" >/dev/null 2>&1 || true
  done
}
trap cleanup EXIT

# ---- run each variant -------------------------------------------------------
PORT="$BASE_PORT"
for v in "${VARIANT_NAMES[@]}"; do
  SPEC=$(variant_spec "$v")
  RTS_FLAGS="${SPEC%%|*}"
  ARENA_MAX="${SPEC#*|}"
  PORT=$((PORT + 1))
  log ""
  log "==> [$v] RTS='$RTS_FLAGS' MALLOC_ARENA_MAX='${ARENA_MAX:-<unset>}' cpus=$CPUS cpuset_cpus='${CPUSET_CPUS:-<unset>}' port=$PORT"
  "$DIR/setup_variant.sh" "$v" "$PORT" "$RTS_FLAGS" "$ARENA_MAX"

  for route in $TEST_ROUTES; do
    case "$route" in
      login)
        log "==> [$v] login load test: $ROUNDS rounds x $CONCURRENCY concurrent"
        "$DIR/bench.sh" "$v" "espial-lt-$v" "$PORT" login "$ROUNDS" "$CONCURRENCY" "$ROUND_WAIT" \
          "$RESULTS_DIR/${v}-login.txt"
        ;;
      db)
        log "==> [$v] bookmark-page load test: $DB_ROUNDS rounds x $DB_CONCURRENCY concurrent"
        "$DIR/bench.sh" "$v" "espial-lt-$v" "$PORT" db "$DB_ROUNDS" "$DB_CONCURRENCY" "$DB_ROUND_WAIT" \
          "$RESULTS_DIR/${v}-db.txt"
        ;;
      *)
        log "warning: unknown route '$route' in TEST_ROUTES, skipping"
        ;;
    esac
  done
done

# ---- report -----------------------------------------------------------------
for route in $TEST_ROUTES; do
  case "$route" in
    login|db) ;;
    *) continue ;;
  esac
  echo
  echo "=========================================="
  if [ "$route" = "login" ]; then
    echo " login page load test (Argon2/BCrypt hashing path)"
  else
    echo " bookmark page load test (DB-fetch path, no hashing)"
  fi
  echo "=========================================="
  ARGS=()
  for v in "${VARIANT_NAMES[@]}"; do
    ARGS+=("$v:$RESULTS_DIR/${v}-${route}.txt")
  done
  "$DIR/report.sh" "${ARGS[@]}"
done

log ""
log "==> raw per-round results kept in $RESULTS_DIR"
