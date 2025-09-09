#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Testes de slices estridados (colunas) + açúcar"
PASS=0
FAIL=0

run_ok() { if "$BIN" --check "$1" >/dev/null; then PASS=$((PASS+1)); else echo "❌ FAIL: $1"; FAIL=$((FAIL+1)); fi; }
run_fail() { if "$BIN" --check "$1" >/dev/null 2>&1; then echo "❌ FAIL: deveria falhar $1"; FAIL=$((FAIL+1)); else PASS=$((PASS+1)); fi; }

run_ok   "$TESTS/25_copy_col_ok.my"
run_ok   "$TESTS/26_fill_col_ok.my"
run_ok   "$TESTS/27_slice_assign_sugar_row_ok.my"
run_fail "$TESTS/111_copy_shape_mismatch.my"
run_fail "$TESTS/112_fill_view_bad_rhs.my"

echo
echo "Resumo strided: pass=$PASS fail=$FAIL"
exit 0

