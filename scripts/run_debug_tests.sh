#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Testes de debug info (smoke)"
PASS=0
FAIL=0

# 1) IR com metadata de debug
OUT_LL="/tmp/dbg24.ll"
if "$BIN" --emit-ll -g -o "$OUT_LL" "$TESTS/24_debug_ir_flag.my" >/dev/null; then
  if grep -q "!llvm.dbg.cu" "$OUT_LL"; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: IR sem !llvm.dbg.cu (debug metadata ausente)"
    FAIL=$((FAIL+1))
  fi
else
  echo "❌ FAIL: emit-ll -g falhou"
  FAIL=$((FAIL+1))
fi

# 2) Objeto com -g (apenas smoke: arquivo existe e não-vazio)
OUT_O="/tmp/dbg25.o"
if "$BIN" --emit-obj -g -o "$OUT_O" "$TESTS/25_debug_obj_flag.my" >/dev/null; then
  if [ -s "$OUT_O" ]; then
    PASS=$((PASS+1))
    if command -v llvm-dwarfdump >/dev/null 2>&1; then
      if ! llvm-dwarfdump "$OUT_O" >/dev/null 2>&1; then
        echo "⚠️  WARN: llvm-dwarfdump retornou erro (ignorado no smoke)"
      fi
    fi
  else
    echo "❌ FAIL: objeto de debug vazio"
    FAIL=$((FAIL+1))
  fi
else
  echo "❌ FAIL: emit-obj -g falhou"
  FAIL=$((FAIL+1))
fi

# 3) Assembly com -g (arquivo existe e não-vazio)
OUT_S="/tmp/dbg25.s"
if "$BIN" --emit-asm -g -o "$OUT_S" "$TESTS/25_debug_obj_flag.my" >/dev/null; then
  if [ -s "$OUT_S" ]; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: assembly de debug vazio"
    FAIL=$((FAIL+1))
  fi
else
  echo "❌ FAIL: emit-asm -g falhou"
  FAIL=$((FAIL+1))
fi

echo
echo "Resumo debug: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]

