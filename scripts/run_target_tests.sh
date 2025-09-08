#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Testes de target (smoke)"

HOST_TRIPLE="$(llvm-config --host-target 2>/dev/null || true)"
if [ -z "$HOST_TRIPLE" ]; then
  HOST_TRIPLE="$("$BIN" --help >/dev/null 2>&1; echo aarch64-apple-darwin)"
fi

PASS=0
FAIL=0

echo "──> --emit-ll com --target=$HOST_TRIPLE"
if "$BIN" --target="$HOST_TRIPLE" --emit-ll -o /tmp/tgt.ll "$TESTS/19_target_ok.my" >/dev/null; then
  PASS=$((PASS+1))
else
  echo "❌ FAIL: emit-ll --target=$HOST_TRIPLE"
  FAIL=$((FAIL+1))
fi

echo "──> --emit-obj com --target=$HOST_TRIPLE"
if "$BIN" --target="$HOST_TRIPLE" --emit-obj -o /tmp/tgt.o "$TESTS/19_target_ok.my" >/dev/null; then
  PASS=$((PASS+1))
else
  echo "❌ FAIL: emit-obj --target=$HOST_TRIPLE"
  FAIL=$((FAIL+1))
fi

# (Opcional) teste x86, se disponível
if "$BIN" --target="x86_64-apple-darwin" --emit-ll -o /tmp/tgt_x86.ll "$TESTS/19_target_ok.my" >/dev/null 2>&1; then
  echo "──> --emit-ll (x86_64-apple-darwin) OK"
  PASS=$((PASS+1))
else
  echo "ℹ️  Pulo x86_64-apple-darwin (backend pode não estar disponível)."
fi

echo
echo "Resumo target: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]

