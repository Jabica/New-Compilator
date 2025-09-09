#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Testes de views derivadas (transpose/slice)"
PASS=0
FAIL=0

run_ok()   { if "$BIN" --check "$1" >/dev/null; then PASS=$((PASS+1)); else echo "❌ FAIL: $1"; FAIL=$((FAIL+1)); fi; }
run_fail() { if "$BIN" --check "$1" >/dev/null 2>&1; then echo "❌ FAIL: deveria falhar $1"; FAIL=$((FAIL+1)); else PASS=$((PASS+1)); fi; }

run_ok   "$TESTS/28_transpose_col_copy_ok.my"
run_ok   "$TESTS/29_slice_step2_ok.my"
run_ok   "$TESTS/30_slice_fill_ok.my"
run_fail "$TESTS/113_slice_bad_len.my"
run_fail "$TESTS/114_slice_bad_step.my"

echo
echo "Resumo derived: pass=$PASS fail=$FAIL"
exit 0

