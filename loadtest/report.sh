#!/usr/bin/env bash
# Prints a side-by-side comparison table from bench.sh result files.
# Usage: report.sh <name1>:<file1> [<name2>:<file2> ...]
set -uo pipefail

declare -a NAMES=()
declare -a FILES=()
for pair in "$@"; do
  NAMES+=("${pair%%:*}")
  FILES+=("${pair#*:}")
done

if [ "${#NAMES[@]}" -eq 0 ]; then
  echo "usage: report.sh <name1>:<file1> [<name2>:<file2> ...]" >&2
  exit 1
fi

MAX_ROUNDS=0
declare -A MEM_VAL REQ_VAL THR_PERIODS THR_COUNT THR_USEC LAST_OOM LAST_STATUS LAST_ROUND

# One awk pass per file -- not one per (file, round, field) cell -- parses
# every "round=N ... mem=X ... req_per_s=Y ... nr_periods=P nr_throttled=T
# throttled_usec=U oom=B status=S" line once and stashes the per-round
# fields plus the final line's oom/status. nr_periods/nr_throttled/
# throttled_usec are cumulative cgroup counters (since container start), not
# per-round deltas -- the last round's values are the run's totals. Table
# lookups below are then pure bash array reads, no further subprocesses.
for f in "${FILES[@]}"; do
  last_round=0
  if [ -f "$f" ]; then
    while IFS=$'\t' read -r round mem req periods throttled usec oom status; do
      [ -z "$round" ] && continue
      MEM_VAL["$f:$round"]="$mem"
      REQ_VAL["$f:$round"]="$req"
      THR_PERIODS["$f:$round"]="$periods"
      THR_COUNT["$f:$round"]="$throttled"
      THR_USEC["$f:$round"]="$usec"
      LAST_OOM["$f"]="$oom"
      LAST_STATUS["$f"]="$status"
      last_round="$round"
      [ "$round" -gt "$MAX_ROUNDS" ] && MAX_ROUNDS="$round"
    done < <(awk '{
      round=""; mem=""; req=""; periods=""; throttled=""; usec=""; oom=""; status="";
      for (i = 1; i <= NF; i++) {
        if ($i ~ /^round=/)           { round = substr($i, 7) }
        else if ($i ~ /^mem=/)        { mem = substr($i, 5) }
        else if ($i ~ /^req_per_s=/)  { req = substr($i, 11) }
        else if ($i ~ /^nr_periods=/) { periods = substr($i, 12) }
        else if ($i ~ /^nr_throttled=/) { throttled = substr($i, 14) }
        else if ($i ~ /^throttled_usec=/) { usec = substr($i, 16) }
        else if ($i ~ /^oom=/)        { oom = substr($i, 5) }
        else if ($i ~ /^status=/)     { status = substr($i, 8) }
      }
      if (round != "") printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", round, mem, req, periods, throttled, usec, oom, status
    }' "$f" 2>/dev/null)
  fi
  LAST_ROUND["$f"]="$last_round"
done

print_table() {
  local -n vals_ref="$1"
  local missing="$2"
  printf "%-8s" "round"
  for name in "${NAMES[@]}"; do printf "%16s" "$name"; done
  printf "\n"
  local round f val
  for ((round = 1; round <= MAX_ROUNDS; round++)); do
    printf "%-8s" "$round"
    for f in "${FILES[@]}"; do
      val="${vals_ref["$f:$round"]:-}"
      [ -z "$val" ] && val="$missing"
      printf "%16s" "$val"
    done
    printf "\n"
  done
}

echo "memory per round (RSS/limit):"
print_table MEM_VAL "-dead-"
echo
echo "throughput per round (req/s):"
print_table REQ_VAL "-"
echo
echo "cgroup CPU throttling (cumulative throttled_usec since container start):"
print_table THR_USEC "-"
echo
echo "final status:"
for i in "${!NAMES[@]}"; do
  f="${FILES[$i]}"
  if [ ! -s "$f" ]; then
    echo "  ${NAMES[$i]}: no data"
    continue
  fi
  last_round="${LAST_ROUND[$f]}"
  last_oom="${LAST_OOM[$f]:-}"
  last_status="${LAST_STATUS[$f]:-}"
  periods="${THR_PERIODS[$f:$last_round]:-0}"
  throttled="${THR_COUNT[$f:$last_round]:-0}"
  usec="${THR_USEC[$f:$last_round]:-0}"
  pct="-"
  [ "${periods:-0}" -gt 0 ] 2>/dev/null && pct=$(awk -v t="$throttled" -v p="$periods" 'BEGIN { printf "%.1f%%", (p > 0 ? 100*t/p : 0) }')
  throttle_summary="nr_periods=$periods nr_throttled=$throttled ($pct) throttled_usec=$usec"
  if [ "$last_oom" = "true" ] || [ "$last_status" != "running" ]; then
    echo "  ${NAMES[$i]}: died at round $last_round (oom=$last_oom status=$last_status) $throttle_summary"
  else
    echo "  ${NAMES[$i]}: survived all $last_round rounds -- $throttle_summary"
  fi
done
