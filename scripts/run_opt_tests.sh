#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"
TESTS_DIR="$ROOT/tests"

PASS=0
FAIL=0

# Tentar garantir que 'lli' esteja no PATH (via LLVM_CONFIG ou paths comuns)
LLVM_CONFIG_BIN="${LLVM_CONFIG:-}"
if [[ -z "$LLVM_CONFIG_BIN" ]]; then
  LLVM_CONFIG_BIN="$(command -v llvm-config || true)"
fi
if [[ -n "$LLVM_CONFIG_BIN" ]]; then
  LLVM_BINDIR="$($LLVM_CONFIG_BIN --bindir 2>/dev/null || true)"
  if [[ -n "$LLVM_BINDIR" && -d "$LLVM_BINDIR" ]]; then
    PATH="$LLVM_BINDIR:$PATH"
    export PATH
  fi
fi
# Brew macOS (fallback comum)
if [[ -x "/opt/homebrew/opt/llvm/bin/lli" ]]; then
  PATH="/opt/homebrew/opt/llvm/bin:$PATH"
  export PATH
fi

echo "▶︎ Testes de otimização (smoke)"

# 1) Dump IR com O2
echo "──> --dump-ir com --opt=O2 em tests/exprs.my"
"$BIN" --opt=O2 --dump-ir "$TESTS_DIR/exprs.my" >/dev/null && PASS=$((PASS+1)) || { echo "❌ FAIL: dump-ir O2 exprs"; FAIL=$((FAIL+1)); }

# 2) Emit LL com O3
echo "──> --emit-ll com --opt=O3 em tests/18_opt_constfold_ok.my"
"$BIN" --opt=O3 --emit-ll -o /tmp/opt.ll "$TESTS_DIR/18_opt_constfold_ok.my" >/dev/null && PASS=$((PASS+1)) || { echo "❌ FAIL: emit-ll O3"; FAIL=$((FAIL+1)); }

# 3) Run com O2 (lli) — só se 'lli' estiver disponível
if command -v lli >/dev/null 2>&1; then
  echo "──> --run com --opt=O2 em tests/18_opt_constfold_ok.my"
  "$BIN" --opt=O2 --run "$TESTS_DIR/18_opt_constfold_ok.my" >/dev/null && PASS=$((PASS+1)) || { echo "❌ FAIL: run O2"; FAIL=$((FAIL+1)); }
else
  echo "↷ skip: 'lli' não encontrado no PATH; pulando teste de --run"
fi

# 4) Novos smokes: -O* + --print-pipeline para ll/bc/asm/obj
run_case () {
  local LEVEL="$1"   # -O0|-O1|-O2|-O3|-Os|-Oz
  local MODE="$2"    # ll|bc|asm|obj
  local TEST="$TESTS_DIR/23_opt_pipeline_smoke.my"
  local OUT="/tmp/opt_${LEVEL:2}.${MODE}" # remove ' -O'
  local FLAG
  case "$MODE" in
    ll)  FLAG="--emit-ll" ;;
    bc)  FLAG="--emit-bc" ;;
    asm) FLAG="--emit-asm" ;;
    obj) FLAG="--emit-obj" ;;
    *) echo "modo invalido"; return 1 ;;
  esac
  local LOG="/tmp/pipeline_${LEVEL:2}_${MODE}.log"
  if "$BIN" "$FLAG" -o "$OUT" "$TEST" "$LEVEL" --print-pipeline >/dev/null 2>"$LOG"; then
    if [[ -s "$OUT" && -s "$LOG" ]]; then
      PASS=$((PASS+1))
    else
      echo "❌ FAIL: ${LEVEL} ${MODE} gerou saida/pipeline vazio"
      FAIL=$((FAIL+1))
    fi
  else
    echo "❌ FAIL: ${LEVEL} ${MODE} falhou"
    FAIL=$((FAIL+1))
  fi
}

run_case "-O0" "ll"
run_case "-O2" "ll"
run_case "-O3" "bc"
run_case "-Os" "asm"
run_case "-Oz" "obj"

echo
echo "Resumo otimizações: pass=$PASS fail=$FAIL"
[ $FAIL -eq 0 ]
