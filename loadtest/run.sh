#!/usr/bin/env bash
# Espial load-test harness: builds an image, spins up one container per
# "variant" (a combination of RTS flags / extra env vars / CPU & memory
# limits), fires repeated concurrent-request bursts at each configured
# route, and reports a side-by-side memory + throughput comparison. Cleans
# up all containers/volumes it created on exit.
#
# Variants, routes, and test parameters (concurrency, rounds, container
# resources, bookmark fixture, etc.) are defined in config.yaml (see that
# file) rather than hardcoded here -- add/edit a variant, route, or
# parameter there; no script changes needed for "get"-type routes (see
# bench.sh's header for what "type" means).
#
# Every config.yaml parameter can still be overridden per-run via a
# same-named LOADTEST_-prefixed environment variable (prefixed so these
# never collide with pre-set/reserved env vars like the OS's own USERNAME),
# e.g.:
#
#   LOADTEST_VARIANTS="baseline arena" LOADTEST_ROUNDS=4 loadtest/run.sh
#   LOADTEST_CPUSET_CPUS=0 LOADTEST_VARIANTS="baseline arena" loadtest/run.sh   # pin to 1 real core
#
# A variant can also pin its own image in config.yaml ("image:", see that
# file), overriding $LOADTEST_IMAGE just for that variant -- useful for
# comparing two espial builds side by side. The image build step itself only
# ever builds $LOADTEST_IMAGE (the default), and only runs when LOADTEST_BUILD=1
# is set (default is to skip the build and use whatever image already exists,
# so a variant's overridden image just needs to already be present locally).
#
# Each run's result files are written to $LOADTEST_RESULTS_DIR prefixed with
# LOADTEST_RUN_PREFIX (default: a timestamp + PID), so results from separate
# runs coexist there instead of overwriting each other; only the current
# run's own files are ever fed to report.sh.
#
# Per-route rounds/concurrency/roundWait are overridden via
# "LOADTEST_<ROUTE_NAME_UPPERCASE>_ROUNDS" etc, e.g. LOADTEST_DB_ROUNDS=1 for
# the "db" route (the "login" route keeps its unprefixed-by-route
# LOADTEST_ROUNDS/LOADTEST_CONCURRENCY/LOADTEST_ROUND_WAIT).
#
# Add a custom one-off variant, without editing config.yaml, by naming it in
# LOADTEST_VARIANTS and exporting a same-named env var formatted as
# "RTS_FLAGS|ENV_KEY=VALUE,ENV_KEY2=VALUE2" (the env part may be empty), e.g.:
#
#   MYVAR="-T -A64m|MALLOC_ARENA_MAX=1" LOADTEST_VARIANTS="baseline MYVAR" loadtest/run.sh
set -uo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$DIR/.." && pwd)"
source "$DIR/lib.sh"
require_jq
require_yq
: "${LOADTEST_CONFIG:=$DIR/config.yaml}"
load_config "$LOADTEST_CONFIG"

# ---- parameters (defaults from config.yaml; override via environment) -----
: "${LOADTEST_IMAGE:=espial-loadtest:latest}"
: "${LOADTEST_DOCKERFILE:=Dockerfile.buildkit}"
: "${LOADTEST_BUILD:=0}"        # 1 = (re)build $LOADTEST_IMAGE before running; default skips the build
: "${LOADTEST_VARIANTS:=$(default_variants)}"
: "${LOADTEST_TEST_ROUTES:=$(default_test_routes)}"
: "${LOADTEST_MEMORY:=$(cfg '.params.container.memory')}"
: "${LOADTEST_MEMORY_SWAP:=$(cfg '.params.container.memorySwap')}"
: "${LOADTEST_CPUS:=$(cfg '.params.container.cpus')}"           # CFS CPU-time quota (doesn't limit visible core count)
: "${LOADTEST_CPUSET_CPUS:=$(cfg '.params.container.cpusetCpus')}" # e.g. "0" to pin to one real core
                                                                     # (affects glibc's default arena-count heuristic; empty = unset)
: "${LOADTEST_USERNAME:=$(cfg '.params.user.username')}"
: "${LOADTEST_PASSWORD:=$(cfg '.params.user.password')}"
: "${LOADTEST_BOOKMARK_FILE:=$(cfg '.params.bookmarkFile')}"
: "${LOADTEST_RESULTS_DIR:=$DIR/results}"
: "${LOADTEST_BASE_PORT:=$(cfg '.params.basePort')}"
: "${LOADTEST_KEEP:=0}"         # 1 = leave containers/volumes running for inspection
export LOADTEST_IMAGE LOADTEST_USERNAME LOADTEST_PASSWORD LOADTEST_MEMORY LOADTEST_MEMORY_SWAP LOADTEST_CPUS LOADTEST_CPUSET_CPUS

LOADTEST_BOOKMARK_FILE_ABS="$REPO_ROOT/$LOADTEST_BOOKMARK_FILE"
if [ ! -f "$LOADTEST_BOOKMARK_FILE_ABS" ]; then
  log "error: bookmark file not found: $LOADTEST_BOOKMARK_FILE_ABS"
  exit 1
fi
export LOADTEST_BOOKMARK_FILE_ABS

# ---- variant presets -------------------------------------------------------
# Resolves variant "$1"'s RTS flags + env vars + image, either from
# config.yaml or (if not found there) from a same-named env var -- see the
# header comment for the env var format. Sets RTS_FLAGS and the ENV_KV array
# as a side effect ($ENV_KV holds zero or more "KEY=VALUE" strings), and
# VARIANT_IMAGE ($LOADTEST_IMAGE unless config.yaml's "image" for this
# variant overrides it; the one-off env var format has no image override).
resolve_variant() {
  local name="$1"
  if variant_defined "$name"; then
    RTS_FLAGS=$(variant_rts_flags "$name")
    readarray -t ENV_KV < <(variant_env_kv "$name")
    VARIANT_IMAGE=$(variant_image "$name")
    : "${VARIANT_IMAGE:=$LOADTEST_IMAGE}"
    return
  fi

  local custom="${!name:-}"
  if [ -z "$custom" ]; then
    log "error: unknown variant '$name'."
    log "  known presets: $(variant_names)"
    log "  add a preset by editing $LOADTEST_CONFIG"
    log "  or for a one-off, export $name=\"RTS_FLAGS|ENV_KEY=VALUE,...\" first"
    exit 1
  fi
  RTS_FLAGS="${custom%%|*}"
  VARIANT_IMAGE="$LOADTEST_IMAGE"
  local envpart="${custom#*|}"
  ENV_KV=()
  if [ "$envpart" != "$custom" ] && [ -n "$envpart" ]; then
    IFS=',' read -r -a ENV_KV <<< "$envpart"
  fi
}

# ---- route presets ----------------------------------------------------------
# Validates route "$1" is defined in config.yaml (exits with a helpful error
# otherwise), then resolves its rounds/concurrency/roundWait: an env var
# named "LOADTEST_<ROUTE_UPPER>_ROUNDS" etc if set (LOADTEST_ROUNDS/
# LOADTEST_CONCURRENCY/LOADTEST_ROUND_WAIT for "login"), else config.yaml's
# params.<route>.*. Sets ROUTE_ROUNDS/ROUTE_CONCURRENCY/ROUTE_ROUND_WAIT.
resolve_route_params() {
  local route="$1"
  if ! route_defined "$route"; then
    log "error: unknown route '$route'."
    log "  known routes: $(route_names)"
    log "  add one by editing $LOADTEST_CONFIG"
    exit 1
  fi

  local rounds_var conc_var wait_var
  if [ "$route" = "login" ]; then
    rounds_var=LOADTEST_ROUNDS conc_var=LOADTEST_CONCURRENCY wait_var=LOADTEST_ROUND_WAIT
  else
    local upper
    upper=$(printf '%s' "$route" | tr '[:lower:]' '[:upper:]')
    rounds_var="LOADTEST_${upper}_ROUNDS" conc_var="LOADTEST_${upper}_CONCURRENCY" wait_var="LOADTEST_${upper}_ROUND_WAIT"
  fi

  local -n rounds_ref="$rounds_var" conc_ref="$conc_var" wait_ref="$wait_var"
  : "${rounds_ref:=$(cfg ".params[\"$route\"].rounds")}"
  : "${conc_ref:=$(cfg ".params[\"$route\"].concurrency")}"
  : "${wait_ref:=$(cfg ".params[\"$route\"].roundWait")}"
  ROUTE_ROUNDS="$rounds_ref"
  ROUTE_CONCURRENCY="$conc_ref"
  ROUTE_ROUND_WAIT="$wait_ref"
}

# Route report heading: config.yaml's "description" for the route if set,
# else a generic fallback.
route_description() {
  local desc
  desc=$(route_field "$1" "description")
  if [ "$desc" = "null" ]; then
    printf '%s load test' "$1"
  else
    printf '%s' "$desc"
  fi
}

mkdir -p "$LOADTEST_RESULTS_DIR"
: "${LOADTEST_RUN_PREFIX:=$(date +%Y%m%d-%H%M%S)}"   # unique per run.sh invocation, so results
                                                          # from concurrent/past runs coexist in
                                                          # $LOADTEST_RESULTS_DIR without colliding

if [ "$LOADTEST_BUILD" = "1" ]; then
  log "==> building $LOADTEST_IMAGE from $LOADTEST_DOCKERFILE"
  dk_build -f "$REPO_ROOT/$LOADTEST_DOCKERFILE" -t "$LOADTEST_IMAGE" "$REPO_ROOT"
else
  log "==> using existing image $LOADTEST_IMAGE (set LOADTEST_BUILD=1 to rebuild it first)"
fi

read -r -a VARIANT_NAMES <<< "$LOADTEST_VARIANTS"

cleanup() {
  rm -f "$CONFIG_JSON"
  if [ "$LOADTEST_KEEP" = "1" ]; then
    log "==> LOADTEST_KEEP=1, leaving containers/volumes in place (docker rm -f espial-lt-<variant>; docker volume rm espial-lt-<variant>-data to clean up later)"
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
PORT="$LOADTEST_BASE_PORT"
for v in "${VARIANT_NAMES[@]}"; do
  resolve_variant "$v"
  PORT=$((PORT + 1))
  log ""
  log "==> [$v] image=$VARIANT_IMAGE RTS='$RTS_FLAGS' env='${ENV_KV[*]:-<none>}' memory='${LOADTEST_MEMORY:-<unset>}' memory_swap='${LOADTEST_MEMORY_SWAP:-<unset>}' cpus='${LOADTEST_CPUS:-<unset>}' cpuset_cpus='${LOADTEST_CPUSET_CPUS:-<unset>}' port=$PORT"
  LOADTEST_IMAGE="$VARIANT_IMAGE" "$DIR/setup_variant.sh" "$v" "$PORT" "$RTS_FLAGS" "${ENV_KV[@]}"

  for route in $LOADTEST_TEST_ROUTES; do
    resolve_route_params "$route"
    log "==> [$v] $(route_description "$route"): $ROUTE_ROUNDS rounds x $ROUTE_CONCURRENCY concurrent"
    "$DIR/bench.sh" "$v" "espial-lt-$v" "$PORT" "$route" "$ROUTE_ROUNDS" "$ROUTE_CONCURRENCY" "$ROUTE_ROUND_WAIT" \
      "$LOADTEST_RESULTS_DIR/${LOADTEST_RUN_PREFIX}-${v}-${route}.txt"
  done
done

# ---- report -----------------------------------------------------------------
for route in $LOADTEST_TEST_ROUTES; do
  echo
  echo "=========================================="
  echo " $(route_description "$route")"
  echo "=========================================="
  ARGS=()
  for v in "${VARIANT_NAMES[@]}"; do
    ARGS+=("$v:$LOADTEST_RESULTS_DIR/${LOADTEST_RUN_PREFIX}-${v}-${route}.txt")
  done
  "$DIR/report.sh" "${ARGS[@]}"
done

log ""
log "==> raw per-round results kept in $LOADTEST_RESULTS_DIR (this run's files prefixed '$LOADTEST_RUN_PREFIX-')"
