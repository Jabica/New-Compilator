#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Vetorizacao fill2d (4×i32) — smoke"
PASS=0
FAIL=0

check_vec_stores() {
  local file="$1"
  local count
  # Dump IR e procure por store em <4 x i32>
  count=$("$BIN" --dump-ir "$file" --vec2d=always | grep -E -c "store <4 x i32>")
  if [ "$count" -ge 1 ]; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: $file não mostrou store vetorizado"
    FAIL=$((FAIL+1))
  fi
}

check_vec_stores "$TESTS/29_fill2d_vec4_ok.my"
check_vec_stores "$TESTS/30_fill2d_vec4_tail_ok.my"

echo
echo "Resumo vec2d: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]

