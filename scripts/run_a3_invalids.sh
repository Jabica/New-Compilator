#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
INV="$ROOT/examples_invalid"

echo "▶︎ A3 Invalidos — parse/semântica + resumo"
echo

# Garante binário
if [[ ! -x "$BIN" ]]; then
  echo "🔧 Buildando projeto..."
  cmake -S "$ROOT" -B "$ROOT/build" -G Ninja ${LLVM_DIR:+-DLLVM_DIR="$LLVM_DIR"}
  cmake --build "$ROOT/build" --config Release
  echo
fi

PASS=0; FAIL=0

should_fail_grep() {
  local file="$1"
  local expect_msg="$2"

  echo "— $file"
  # Rodamos a checagem semântica diretamente
  out=$("$BIN" --check "$file" 2>&1 || true)

  if echo "$out" | grep -qi "$expect_msg"; then
    echo "   ✅ OK (erro esperado encontrado: '$expect_msg')"
    PASS=$((PASS+1))
  else
    echo "   ❌ FAIL: não encontrou erro esperado '$expect_msg'"
    echo "      Saída:"
    echo "$out" | sed 's/^/      /'
    FAIL=$((FAIL+1))
  fi
  echo
}

# Casos e padrões (substrings robustas)
should_fail_grep "$INV/01_var_undeclared_err.my"        "variavel nao declarada"
should_fail_grep "$INV/02_break_outside_loop_err.my"    "fora de laco"
should_fail_grep "$INV/03_redeclare_function_err.my"    "redefinicao de funcao"
should_fail_grep "$INV/04_call_arity_mismatch_err.my"   "aridade incorreta"
should_fail_grep "$INV/05_return_type_mismatch_err.my"  "retorno sem valor"

echo "Resumo A3-invalidos: pass=$PASS fail=$FAIL"
[[ "$FAIL" -eq 0 ]]

