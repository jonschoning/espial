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
for f in "${FILES[@]}"; do
  n=$(wc -l < "$f" 2>/dev/null || echo 0)
  [ "$n" -gt "$MAX_ROUNDS" ] && MAX_ROUNDS=$n
done

# extract <file> <round> <field> -- pulls "field=value" from the "round=N ..." line.
extract() {
  local file="$1" round="$2" field="$3"
  awk -v want="round=$round " -v key="$field=" '
    index($0, want) == 1 {
      for (i = 1; i <= NF; i++) {
        if (index($i, key) == 1) { print substr($i, length(key) + 1); exit }
      }
    }' "$file" 2>/dev/null
}

print_table() {
  local field="$1" missing="$2"
  printf "%-8s" "round"
  for name in "${NAMES[@]}"; do printf "%16s" "$name"; done
  printf "\n"
  local round f val
  for ((round = 1; round <= MAX_ROUNDS; round++)); do
    printf "%-8s" "$round"
    for f in "${FILES[@]}"; do
      val=$(extract "$f" "$round" "$field")
      [ -z "$val" ] && val="$missing"
      printf "%16s" "$val"
    done
    printf "\n"
  done
}

echo "memory per round (RSS/limit):"
print_table "mem" "-dead-"
echo
echo "throughput per round (req/s):"
print_table "req_per_s" "-"
echo
echo "final status:"
for i in "${!NAMES[@]}"; do
  f="${FILES[$i]}"
  if [ ! -s "$f" ]; then
    echo "  ${NAMES[$i]}: no data"
    continue
  fi
  last_round=$(wc -l < "$f")
  last_line=$(tail -1 "$f")
  last_oom=$(printf '%s' "$last_line" | grep -o 'oom=[a-z]*' | cut -d= -f2)
  last_status=$(printf '%s' "$last_line" | grep -o 'status=[a-z]*' | cut -d= -f2)
  if [ "$last_oom" = "true" ] || [ "$last_status" != "running" ]; then
    echo "  ${NAMES[$i]}: died at round $last_round (oom=$last_oom status=$last_status)"
  else
    echo "  ${NAMES[$i]}: survived all $last_round rounds"
  fi
done
