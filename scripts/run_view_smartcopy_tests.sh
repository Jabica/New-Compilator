#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
T="$ROOT/tests"

echo "▶︎ Smart copy/fill (fast-path e unroll)"
PASS=0; FAIL=0

ok()   { if "$BIN" --check "$1" >/dev/null; then PASS=$((PASS+1)); else echo "❌ FAIL: $1"; FAIL=$((FAIL+1)); fi; }

ok "$T/31_runtime_fastpath_memcpy_ok.my"
ok "$T/32_runtime_slow_unroll_ok.my"
ok "$T/33_transpose_fast_slow_mix_ok.my"

echo
echo "Resumo smart: pass=$PASS fail=$FAIL"
exit 0

