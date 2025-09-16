#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
EX="$ROOT/examples"

echo "▶︎ A3 Demo — build + exec exemplos"
echo

if [[ ! -x "$BIN" ]]; then
  echo "🔧 Buildando projeto..."
  cmake -S "$ROOT" -B "$ROOT/build" -G Ninja ${LLVM_DIR:+-DLLVM_DIR="$LLVM_DIR"}
  cmake --build "$ROOT/build" --config Release
  echo
fi

PASS=0; FAIL=0

run_case() {
  local file="$1"; local expect="$2"
  echo "— $file"
  local exe="/tmp/$(basename "$file" .my)_a3_demo"
  "$BIN" --emit-exe -o "$exe" "$file" >/dev/null 2>&1 || true
  if [[ ! -x "$exe" ]]; then
    echo "   ❌ FAIL (nao gerou executavel)"; FAIL=$((FAIL+1)); echo; return
  fi
  local out; out=$("$exe" 2>&1 || true)
  if echo "$out" | grep -q "^$expect$"; then
    echo "   ✅ OK (stdout='$expect')"; PASS=$((PASS+1))
  else
    echo "   ❌ FAIL (stdout esperado='$expect')"
    echo "      Obtido:"; echo "$out" | sed 's/^/      /'
    FAIL=$((FAIL+1))
  fi
  echo
}

run_case "$EX/01_hello.my"          "123"
run_case "$EX/02_soma_funcoes.my"   "42"
run_case "$EX/03_fatorial_iter.my"  "120"
run_case "$EX/04_fibonacci_iter.my" "55"
run_case "$EX/05_io_soma.my"        "42"

echo "Resumo A3: pass=$PASS fail=$FAIL"
[[ $FAIL -eq 0 ]]
