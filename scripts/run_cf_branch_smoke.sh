#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
T="$ROOT/tests"

echo "▶︎ Folding de condições + pesos de branch"

# 42: não deve ter bloco else (then pode ter sido inline)
ir42=$("$BIN" --dump-ir "$T/42_const_if_true_ok.my" --verify-ir)
echo "$ir42" | grep -q "if.else"    && { echo "FAIL: else nao deveria existir"; exit 1; }

# 43: não deve ter bloco then (else pode ter sido inline)
ir43=$("$BIN" --dump-ir "$T/43_const_if_false_ok.my" --verify-ir)
echo "$ir43" | grep -q "if.then"    && { echo "FAIL: then nao deveria existir"; exit 1; }

# 44: laço removido (sem corpo executável)
ir44=$("$BIN" --dump-ir "$T/44_while_false_fold_ok.my" --verify-ir)
echo "$ir44" | grep -q "while.end"  || { echo "FAIL: falta while.end"; exit 1; }

echo "Resumo folding/pesos: pass=3 fail=0"
