#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Testes de slices contíguos (copy/fill)"
PASS=0
FAIL=0

echo "──> 23_copy_row_ok.my"
if "$BIN" --check "$TESTS/23_copy_row_ok.my" >/dev/null; then PASS=$((PASS+1)); else echo "❌ FAIL copy ok"; FAIL=$((FAIL+1)); fi

echo "──> 24_fill_row_ok.my"
if "$BIN" --check "$TESTS/24_fill_row_ok.my" >/dev/null; then PASS=$((PASS+1)); else echo "❌ FAIL fill ok"; FAIL=$((FAIL+1)); fi

echo "──> 107_copy_len_mismatch.my (fail)"
if "$BIN" --check "$TESTS/107_copy_len_mismatch.my" >/dev/null 2>&1; then
  echo "❌ FAIL: deveria falhar"
  FAIL=$((FAIL+1))
else
  PASS=$((PASS+1))
fi

echo "──> 108_fill_type_mismatch.my (fail)"
if "$BIN" --check "$TESTS/108_fill_type_mismatch.my" >/dev/null 2>&1; then
  echo "❌ FAIL: deveria falhar"
  FAIL=$((FAIL+1))
else
  PASS=$((PASS+1))
fi

echo
echo "Resumo view-ops: pass=$PASS fail=$FAIL"
exit 0

