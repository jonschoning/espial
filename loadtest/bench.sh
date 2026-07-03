#!/usr/bin/env bash
# Fires repeated concurrent-request bursts against a running espial container
# and records memory + throughput + cgroup CPU-throttle counters per round to
# a results file. Stops early if the container gets OOM-killed or otherwise
# stops running.
#
# Usage: bench.sh <variant_name> <container_name> <port> <route_name> \
#                  <rounds> <concurrency> <round_wait_seconds> <results_file>
#
# <route_name> is looked up in config.yaml's "routes" map to decide *how* to
# hit it -- see that file. Two route types are supported:
#   "get"   a single GET request; "path" may contain a "{username}"
#           placeholder. Generic -- add as many "get" routes to config.yaml
#           as you like, no script changes needed.
#   "login" the CSRF-token-then-POST-credentials flow login requires. This
#           is intrinsically multi-step, so unlike "get" routes, adding
#           another route of this *shape* needs a new case here -- but its
#           paths/expected status are still configurable via config.yaml.
#
# Reads LOADTEST_USERNAME/LOADTEST_PASSWORD from the environment for "login"
# routes.
set -uo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$DIR/lib.sh"
require_jq
require_yq
: "${LOADTEST_CONFIG:=$DIR/config.yaml}"
load_config "$LOADTEST_CONFIG"
trap 'rm -f "$CONFIG_JSON"' EXIT

NAME="$1"
CONTAINER="$2"
PORT="$3"
ROUTE_NAME="$4"
ROUNDS="$5"
CONCURRENCY="$6"
ROUND_WAIT="$7"
RESULTS_FILE="$8"

if ! route_defined "$ROUTE_NAME"; then
  log "error: unknown route '$ROUTE_NAME'."
  log "  known routes: $(route_names)"
  log "  add one by editing $LOADTEST_CONFIG"
  exit 1
fi
ROUTE_TYPE=$(route_field "$ROUTE_NAME" "type")
EXPECTED_STATUS=$(route_field "$ROUTE_NAME" "expectedStatus")

BASE="http://localhost:$PORT"

# Set by fire_*_round below; read by the caller after each call.
RESULT=""

# Unlike fire_get_round, this still spawns one curl process per concurrent
# request rather than batching via `curl --parallel`: each simulated login
# needs its own isolated cookie jar, but curl's cookie engine is shared
# across all URLs in a single --parallel invocation (only the last
# --cookie-jar destination in a batch actually gets written), which would
# leak one "user"'s CSRF cookie into another's request and corrupt the test.
fire_login_round() {
  local n="$1" login_page="$2" login_post="$3" expected="$4" tmp
  tmp=$(mktemp -d)
  local start end
  start=$(date +%s.%N)
  for i in $(seq 1 "$n"); do
    (
      cj="$tmp/c$i"
      lp="$tmp/l$i"
      curl -s -c "$cj" -o "$lp" "$BASE$login_page"
      token=$(grep -o 'name="_token" *value="[^"]*"' "$lp" | sed 's/.*value="//;s/"$//')
      xsrf=$(grep XSRF-TOKEN "$cj" | awk '{print $7}')
      curl -s -o /dev/null -w "%{http_code}\n" -b "$cj" \
        -X POST "$BASE$login_post" \
        -H "X-XSRF-TOKEN: $xsrf" \
        --data-urlencode "_token=$token" \
        --data-urlencode "username=$LOADTEST_USERNAME" \
        --data-urlencode "password=$LOADTEST_PASSWORD" \
        > "$tmp/out_$i" 2>/dev/null
    ) &
  done
  wait
  end=$(date +%s.%N)
  # Each request writes its own file (concurrent appends to one shared file
  # aren't reliably atomic on every filesystem this runs on) -- concatenate
  # afterward to count outcomes.
  local ok total
  ok=$(cat "$tmp"/out_* 2>/dev/null | grep -c "^$expected" || true)
  total=$(ls "$tmp"/out_* 2>/dev/null | wc -l)
  rm -rf "$tmp"
  RESULT=$(awk -v s="$start" -v e="$end" -v n="$n" -v ok="${ok:-0}" -v tot="${total:-0}" \
    'BEGIN { printf "wall=%.3f req_per_s=%.2f ok=%d total=%d", (e-s), n/(e-s), ok, tot }')
}

fire_get_round() {
  local n="$1" path="$2" expected="$3" tmp cfgfile null_dev i
  tmp=$(mktemp -d)
  cfgfile="$tmp/curl.cfg"
  null_dev=$(null_device)
  : > "$cfgfile"
  for ((i = 1; i <= n; i++)); do
    printf 'url = "%s"\n' "$BASE$path" >>"$cfgfile"
    printf 'output = "%s"\n' "$null_dev" >>"$cfgfile"
    printf 'write-out = "%s"\n' '%{http_code}\n' >>"$cfgfile"
  done
  local start end
  start=$(date +%s.%N)
  # One curl process fires all $n requests concurrently (--parallel) instead
  # of one curl process per request -- on Windows/Git Bash, spawning dozens
  # of processes nearly simultaneously is itself slow enough to dominate the
  # measured wall time, making req_per_s reflect process-spawn overhead
  # rather than the server's actual throughput.
  curl -s --parallel --parallel-immediate --parallel-max "$n" -K "$cfgfile" >"$tmp/codes.txt" 2>/dev/null
  end=$(date +%s.%N)
  local ok total
  ok=$(grep -c "^$expected" "$tmp/codes.txt" 2>/dev/null || true)
  total=$(wc -l <"$tmp/codes.txt" 2>/dev/null || echo 0)
  rm -rf "$tmp"
  RESULT=$(awk -v s="$start" -v e="$end" -v n="$n" -v ok="${ok:-0}" -v tot="${total:-0}" \
    'BEGIN { printf "wall=%.3f req_per_s=%.2f ok=%d total=%d", (e-s), n/(e-s), ok, tot }')
}

: > "$RESULTS_FILE"
for round in $(seq 1 "$ROUNDS"); do
  case "$ROUTE_TYPE" in
    login)
      LOGIN_PAGE=$(route_field "$ROUTE_NAME" "loginPage")
      LOGIN_POST=$(route_field "$ROUTE_NAME" "loginPost")
      fire_login_round "$CONCURRENCY" "$LOGIN_PAGE" "$LOGIN_POST" "$EXPECTED_STATUS"
      ;;
    get)
      PATH_TEMPLATE=$(route_field "$ROUTE_NAME" "path")
      fire_get_round "$CONCURRENCY" "$(route_expand_path "$PATH_TEMPLATE" "$LOADTEST_USERNAME")" "$EXPECTED_STATUS"
      ;;
    *)
      log "error: route '$ROUTE_NAME' has unknown type '$ROUTE_TYPE' in $LOADTEST_CONFIG"
      exit 1
      ;;
  esac
  sleep 5
  # Strip spaces from "214.8MiB / 430MiB" -> "214.8MiB/430MiB" so it survives
  # as a single whitespace-delimited field in the results file.
  MEM=$(dk stats "$CONTAINER" --no-stream --format "{{.MemUsage}}" 2>/dev/null | tr -d ' ')
  [ -z "$MEM" ] && MEM="0B/0B"
  OOM=$(dk inspect "$CONTAINER" --format '{{.State.OOMKilled}}' 2>/dev/null || echo true)
  STATUS=$(dk inspect "$CONTAINER" --format '{{.State.Status}}' 2>/dev/null || echo missing)
  CPU_STAT=$(cgroup_cpu_stat "$CONTAINER")
  LINE="round=$round $RESULT mem=$MEM $CPU_STAT oom=$OOM status=$STATUS"
  echo "$LINE" | tee -a "$RESULTS_FILE" >&2
  if [ "$OOM" = "true" ] || [ "$STATUS" != "running" ]; then
    log "[$NAME] container is no longer healthy at round $round -- stopping this variant early"
    break
  fi
  if [ "$round" -lt "$ROUNDS" ]; then
    sleep "$ROUND_WAIT"
  fi
done
