#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Fast2D contíguo (1× memcpy)"
PASS=0
FAIL=0

check_one() {
  local file="$1"
  local count
  # Emite IR no stdout e conta memcpys (deve ser 1)
  count=$("$BIN" --dump-ir "$file" --fast2d=auto | grep -c "call .*llvm.memcpy")
  if [ "$count" -eq 1 ]; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: $file esperava 1 memcpy, encontrou $count"
    FAIL=$((FAIL+1))
  fi
}

check_one "$TESTS/27_copy2d_wholeblock_ok.my"
check_one "$TESTS/28_copy2d_aligned_subrows_ok.my"

echo
echo "Resumo fast2d: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]
