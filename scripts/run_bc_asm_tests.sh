#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Testes de bitcode/assembly (smoke)"
PASS=0
FAIL=0

OUT_BC="/tmp/t21.bc"
OUT_S="/tmp/t22.s"

echo "──> --emit-bc"
if "$BIN" --emit-bc -o "$OUT_BC" "$TESTS/21_emit_bc_ok.my" >/dev/null; then
  if [ -s "$OUT_BC" ]; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: bitcode vazio"
    FAIL=$((FAIL+1))
  fi
else
  echo "❌ FAIL: emit-bc falhou"
  FAIL=$((FAIL+1))
fi

echo "──> --emit-asm"
if "$BIN" --emit-asm -o "$OUT_S" "$TESTS/22_emit_asm_ok.my" >/dev/null; then
  if [ -s "$OUT_S" ]; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: assembly vazio"
    FAIL=$((FAIL+1))
  fi
else
  echo "❌ FAIL: emit-asm falhou"
  FAIL=$((FAIL+1))
fi

echo
echo "Resumo bc/asm: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]

