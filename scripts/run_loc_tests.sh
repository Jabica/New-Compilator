#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Testes de DILocation (lin/col)"
PASS=0
FAIL=0

check_has_line() {
  local file="$1"
  local want="$2"
  if grep -q "!DILocation(line: ${want}," "$file"; then
    PASS=$((PASS+1))
  else
    echo "❌ FAIL: não encontrou !DILocation(line: ${want}, ...) em $file"
    FAIL=$((FAIL+1))
  fi
}

# 26
TMP1="/tmp/loc26.ll"
if "$BIN" --emit-ll -g -o "$TMP1" "$TESTS/26_dbg_locs_simple.my" >/dev/null; then
  # Esperados: linhas 2, 3, 4
  check_has_line "$TMP1" 2
  check_has_line "$TMP1" 3
  check_has_line "$TMP1" 4
else
  echo "❌ FAIL: emit-ll -g falhou em 26"
  FAIL=$((FAIL+1))
fi

# 27
TMP2="/tmp/loc27.ll"
if "$BIN" --emit-ll -g -o "$TMP2" "$TESTS/27_dbg_locs_if_while.my" >/dev/null; then
  # Linhas-chave conforme arquivo
  check_has_line "$TMP2" 3  # s = 0;
  check_has_line "$TMP2" 4  # se (...)
  check_has_line "$TMP2" 5  # s = s + 1;
  check_has_line "$TMP2" 9  # enquanto (...)
  check_has_line "$TMP2" 10 # s = s + 3;
  check_has_line "$TMP2" 12 # retorna s;
else
  echo "❌ FAIL: emit-ll -g falhou em 27"
  FAIL=$((FAIL+1))
fi

echo
echo "Resumo DILocation: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]
