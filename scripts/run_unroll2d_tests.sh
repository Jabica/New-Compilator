#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Unroll escalar (x8) — smoke"
PASS=0; FAIL=0

check_unroll() {
  local file="$1"
  local cnt
  cnt=$("$BIN" --dump-ir "$file" --vec2d=off --unroll2d=always \
        | awk '/fill2d.unr.loop/{flag=1;next}/fill2d.unr.exit/{flag=0}flag' \
        | grep -E -c "store i32")
  if [ "$cnt" -ge 6 ]; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: $file não evidenciou stores desenrolados (cnt=$cnt)"
    FAIL=$((FAIL+1))
  fi
}

check_unroll "$TESTS/31_fill2d_unroll8_ok.my"
check_unroll "$TESTS/32_fill2d_unroll8_tail_ok.my"

echo
echo "Resumo unroll2d: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]

