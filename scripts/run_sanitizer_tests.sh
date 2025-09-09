#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
T="$ROOT/tests"

echo "▶︎ Sanitizer smoke"
PASS=0; FAIL=0

echo "──> ASan"
if "$BIN" --emit-exe --asan -o /tmp/a25 "$T/25_asan_overflow.my" >/dev/null 2>&1; then
  out=$(/tmp/a25 2>&1 || true)
  if echo "$out" | grep -q "AddressSanitizer"; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: ASan não detectou overflow"
    FAIL=$((FAIL+1))
  fi
else
  echo "❌ FAIL: build com --asan falhou"
  FAIL=$((FAIL+1))
fi

echo "──> UBSan"
if "$BIN" --emit-exe --ubsan -o /tmp/a26 "$T/26_ubsan_divzero.my" >/dev/null 2>&1; then
  out=$(/tmp/a26 2>&1 || true)
  if echo "$out" | grep -qi "runtime error"; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: UBSan não detectou divzero"
    FAIL=$((FAIL+1))
  fi
else
  echo "❌ FAIL: build com --ubsan falhou"
  FAIL=$((FAIL+1))
fi

echo
echo "Resumo sanitizers: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]
