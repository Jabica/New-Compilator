#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS_DIR="$ROOT/tests"

PASS=0
FAIL=0

echo "▶︎ Testes de otimização (smoke)"

# 1) Dump IR com O2
echo "──> --dump-ir com --opt=O2 em tests/exprs.my"
"$BIN" --opt=O2 --dump-ir "$TESTS_DIR/exprs.my" >/dev/null && PASS=$((PASS+1)) || { echo "❌ FAIL: dump-ir O2 exprs"; FAIL=$((FAIL+1)); }

# 2) Emit LL com O3
echo "──> --emit-ll com --opt=O3 em tests/18_opt_constfold_ok.my"
"$BIN" --opt=O3 --emit-ll -o /tmp/opt.ll "$TESTS_DIR/18_opt_constfold_ok.my" >/dev/null && PASS=$((PASS+1)) || { echo "❌ FAIL: emit-ll O3"; FAIL=$((FAIL+1)); }

# 3) Run com O2 (lli)
echo "──> --run com --opt=O2 em tests/18_opt_constfold_ok.my"
"$BIN" --opt=O2 --run "$TESTS_DIR/18_opt_constfold_ok.my" >/dev/null && PASS=$((PASS+1)) || { echo "❌ FAIL: run O2"; FAIL=$((FAIL+1)); }

echo
echo "Resumo otimizações: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]

