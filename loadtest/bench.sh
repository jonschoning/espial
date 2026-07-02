#!/usr/bin/env bash
# Fires repeated concurrent-request bursts against a running espial container
# and records memory + throughput per round to a results file. Stops early if
# the container gets OOM-killed or otherwise stops running.
#
# Usage: bench.sh <variant_name> <container_name> <port> <login|db> <rounds> \
#                  <concurrency> <round_wait_seconds> <results_file>
#
# Reads USERNAME/PASSWORD from the environment for the login route.
set -uo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$DIR/lib.sh"

NAME="$1"
CONTAINER="$2"
PORT="$3"
ROUTE_TYPE="$4"
ROUNDS="$5"
CONCURRENCY="$6"
ROUND_WAIT="$7"
RESULTS_FILE="$8"

BASE="http://localhost:$PORT"

# Set by fire_*_round below; read by the caller after each call.
RESULT=""

fire_login_round() {
  local n="$1" tmp
  tmp=$(mktemp -d)
  local start end
  start=$(date +%s.%N)
  for i in $(seq 1 "$n"); do
    (
      cj="$tmp/c$i"
      lp="$tmp/l$i"
      curl -s -c "$cj" -o "$lp" "$BASE/auth/login"
      token=$(grep -o 'name="_token" *value="[^"]*"' "$lp" | sed 's/.*value="//;s/"$//')
      xsrf=$(grep XSRF-TOKEN "$cj" | awk '{print $7}')
      curl -s -o /dev/null -w "%{http_code}\n" -b "$cj" \
        -X POST "$BASE/auth/page/db/login" \
        -H "X-XSRF-TOKEN: $xsrf" \
        --data-urlencode "_token=$token" \
        --data-urlencode "username=$USERNAME" \
        --data-urlencode "password=$PASSWORD" \
        > "$tmp/out_$i" 2>/dev/null
    ) &
  done
  wait
  end=$(date +%s.%N)
  # Each request writes its own file (concurrent appends to one shared file
  # aren't reliably atomic on every filesystem this runs on) -- concatenate
  # afterward to count outcomes.
  local ok total
  ok=$(cat "$tmp"/out_* 2>/dev/null | grep -c '^303' || true)
  total=$(ls "$tmp"/out_* 2>/dev/null | wc -l)
  rm -rf "$tmp"
  RESULT=$(awk -v s="$start" -v e="$end" -v n="$n" -v ok="${ok:-0}" -v tot="${total:-0}" \
    'BEGIN { printf "wall=%.3f req_per_s=%.2f ok=%d total=%d", (e-s), n/(e-s), ok, tot }')
}

fire_db_round() {
  local n="$1" path="$2" tmp
  tmp=$(mktemp -d)
  local start end
  start=$(date +%s.%N)
  for i in $(seq 1 "$n"); do
    curl -s -o /dev/null -w "%{http_code}\n" "$BASE$path" > "$tmp/out_$i" 2>/dev/null &
  done
  wait
  end=$(date +%s.%N)
  local ok total
  ok=$(cat "$tmp"/out_* 2>/dev/null | grep -c '^200' || true)
  total=$(ls "$tmp"/out_* 2>/dev/null | wc -l)
  rm -rf "$tmp"
  RESULT=$(awk -v s="$start" -v e="$end" -v n="$n" -v ok="${ok:-0}" -v tot="${total:-0}" \
    'BEGIN { printf "wall=%.3f req_per_s=%.2f ok=%d total=%d", (e-s), n/(e-s), ok, tot }')
}

: > "$RESULTS_FILE"
for round in $(seq 1 "$ROUNDS"); do
  if [ "$ROUTE_TYPE" = "login" ]; then
    fire_login_round "$CONCURRENCY"
  else
    fire_db_round "$CONCURRENCY" "/u:$USERNAME"
  fi
  sleep 5
  # Strip spaces from "214.8MiB / 430MiB" -> "214.8MiB/430MiB" so it survives
  # as a single whitespace-delimited field in the results file.
  MEM=$(dk stats "$CONTAINER" --no-stream --format "{{.MemUsage}}" 2>/dev/null | tr -d ' ')
  [ -z "$MEM" ] && MEM="0B/0B"
  OOM=$(dk inspect "$CONTAINER" --format '{{.State.OOMKilled}}' 2>/dev/null || echo true)
  STATUS=$(dk inspect "$CONTAINER" --format '{{.State.Status}}' 2>/dev/null || echo missing)
  LINE="round=$round $RESULT mem=$MEM oom=$OOM status=$STATUS"
  echo "$LINE" | tee -a "$RESULTS_FILE" >&2
  if [ "$OOM" = "true" ] || [ "$STATUS" != "running" ]; then
    log "[$NAME] container is no longer healthy at round $round -- stopping this variant early"
    break
  fi
  if [ "$round" -lt "$ROUNDS" ]; then
    sleep "$ROUND_WAIT"
  fi
done
