#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS="$ROOT/tests"

echo "▶︎ Controle de fluxo robusto — verify IR"

# Se 'opt' não estiver disponível, apenas gera IR e considera OK
if ! command -v opt >/dev/null 2>&1; then
  echo "(aviso) 'opt' não encontrado no PATH; pulando -verify externo"
  "$BIN" --emit-ll "$TESTS/37_if_early_return_ok.my" --verify-ir >/dev/null
  "$BIN" --emit-ll "$TESTS/38_while_return_in_body_ok.my" --verify-ir >/dev/null
  "$BIN" --emit-ll "$TESTS/39_empty_then_else_ok.my" --verify-ir >/dev/null
  "$BIN" --emit-ll "$TESTS/40_nested_if_while_ok.my" --verify-ir >/dev/null
  "$BIN" --emit-ll "$TESTS/41_unreachable_after_return_ok.my" --verify-ir >/dev/null
  echo "Resumo ctrlflow+verify: pass=5 fail=0"
  exit 0
fi

run_and_verify() {
  local file="$1"
  # Gera IR textual e roda verificador do LLVM
  "$BIN" --emit-ll "$file" --verify-ir > /dev/null
  "$BIN" --emit-ll "$file" --verify-ir | opt -verify -disable-output >/dev/null
}

run_and_verify "$TESTS/37_if_early_return_ok.my"
run_and_verify "$TESTS/38_while_return_in_body_ok.my"
run_and_verify "$TESTS/39_empty_then_else_ok.my"
run_and_verify "$TESTS/40_nested_if_while_ok.my"
run_and_verify "$TESTS/41_unreachable_after_return_ok.my"

echo "Resumo ctrlflow+verify: pass=5 fail=0"
