#!/usr/bin/env bash

# runner v5: nada de arquivos extras, só stdout/stderr
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BIN="$ROOT/build/mycc_cli"

if [[ ! -x "$BIN" ]]; then
  echo "❌ Binário não encontrado: $BIN"
  echo "   Rode: ./scripts/rebuild.sh"
  exit 1
fi

CHECK_FLAG=""
"$BIN" --help 2>&1 | grep -q -- '--check' && CHECK_FLAG="--check"

MUST_PASS=(
  "$ROOT/tests/01_arrays_decl_get_set.my"
  "$ROOT/tests/02_arrays_sum_while.my"
  "$ROOT/tests/03_assign_impl_semicolon.my"
  "$ROOT/tests/04_if_else.my"
  "$ROOT/tests/05_funcs_and_calls.my"
  "$ROOT/tests/06_expr_precedence.my"
  "$ROOT/tests/07_arrays_nested.my"
  "$ROOT/tests/08_func_calls_typecheck.my"
  "$ROOT/tests/07_arrays_nested.my"
"$ROOT/tests/08_func_calls_typecheck.my"
"$ROOT/tests/11_if_both_branches_return.my"
"$ROOT/tests/12_shadowing_ok.my"
"$ROOT/tests/13_params_implicit_conv_ok.my"
"$ROOT/tests/14_unary_ok.my"
"$ROOT/tests/17_conversions_ok.my"

)
MUST_FAIL=(
  "$ROOT/tests/90_should_fail_missing_semicolon.my"
  "$ROOT/tests/91_should_fail_bad_type_suffix.my"
  "$ROOT/tests/09_func_calls_bad_arity.my"
  "$ROOT/tests/10_return_check_fail.my"
  "$ROOT/tests/09_func_calls_bad_arity.my"
"$ROOT/tests/10_return_check_fail.my"
"$ROOT/tests/92_unary_type_errors.my"
"$ROOT/tests/93_unary_minus_on_bool.my"
"$ROOT/tests/94_equality_mismatch.my"
"$ROOT/tests/95_comparison_non_int.my"
"$ROOT/tests/96_assign_undeclared.my"
"$ROOT/tests/97_redeclaration_same_scope.my"
"$ROOT/tests/98_use_before_decl.my"
"$ROOT/tests/99_while_cond_not_bool.my"
"$ROOT/tests/100_index_non_int.my"
"$ROOT/tests/101_call_undeclared.my"
"$ROOT/tests/102_call_arg_type_mismatch.my"
"$ROOT/tests/103_func_redef_diff_signature.my"
"$ROOT/tests/104_return_value_in_void.my"
"$ROOT/tests/105_missing_return_in_nonvoid.my"
"$ROOT/tests/106_if_only_one_branch_returns.my"
)

pass=0
fail=0

run_case () {
  local want="$1"
  local file="$2"

  echo "──> Rodando: $file (esperado: $want)"
  "$BIN" $CHECK_FLAG "$file"
  local rc=$?

  if [[ "$want" == "pass" ]]; then
    if [[ $rc -eq 0 ]]; then
      echo "✅ OK  : $file"
      pass=$((pass+1))
    else
      echo "❌ FAIL: $file (deveria passar)"
      fail=$((fail+1))
    fi
  else
    if [[ $rc -ne 0 ]]; then
      echo "✅ OK(F): $file (falhou como esperado)"
      pass=$((pass+1))
    else
      echo "❌ FAIL: $file (deveria falhar)"
      fail=$((fail+1))
    fi
  fi
}

echo "▶︎ Testes que DEVEM passar"
for f in "${MUST_PASS[@]}"; do
  run_case pass "$f"
done

echo
echo "▶︎ Testes que DEVEM falhar"
for f in "${MUST_FAIL[@]}"; do
  run_case fail "$f"
done

echo

# Casos especiais para --emit-exe
echo "▶︎ Teste extra: --emit-exe (deve passar)"
echo "──> Rodando: $ROOT/tests/18_emit_exe_ok.my (esperado: pass --emit-exe)"
"$BIN" --emit-exe -o /tmp/t18 "$ROOT/tests/18_emit_exe_ok.my"
rc=$?
if [[ $rc -eq 0 ]]; then
  if /tmp/t18 >/dev/null 2>&1; then
    echo "   Execução OK de /tmp/t18"
    pass=$((pass+1))
  else
    echo "❌ FAIL: execução de /tmp/t18 não retornou 0"
    fail=$((fail+1))
  fi
else
  echo "❌ FAIL: --emit-exe falhou para 18_emit_exe_ok.my"
  fail=$((fail+1))
fi

echo
echo "▶︎ Teste extra: --emit-exe (deve falhar)"
echo "──> Rodando: $ROOT/tests/107_emit_exe_missing_main.my (esperado: fail --emit-exe)"
"$BIN" --emit-exe -o /tmp/t_no_main "$ROOT/tests/107_emit_exe_missing_main.my"
rc=$?
if [[ $rc -ne 0 ]]; then
  echo "✅ OK(F): $ROOT/tests/107_emit_exe_missing_main.my (falhou como esperado)"
  pass=$((pass+1))
else
  echo "❌ FAIL: $ROOT/tests/107_emit_exe_missing_main.my (deveria falhar)"
  fail=$((fail+1))
fi

echo

# Casos extras: --opt e --emit-bc
echo "▶︎ Teste extra: --emit-bc com otimização (deve passar)"
echo "──> Rodando: $ROOT/tests/19_opt_emit_bc_ok.my (esperado: pass --opt=O2 --emit-bc)"
"$BIN" --opt=O2 --emit-bc -o /tmp/t19.bc "$ROOT/tests/19_opt_emit_bc_ok.my"
rc=$?
if [[ $rc -eq 0 ]]; then
  if [[ -s /tmp/t19.bc ]]; then
    echo "✅ OK  : --emit-bc O2 /tmp/t19.bc"
    # opcional: desassemblar se disponível
    if command -v llvm-dis >/dev/null 2>&1; then
      llvm-dis -o /tmp/t19.ll /tmp/t19.bc && echo "   llvm-dis OK (bitcode legível)"
    fi
    pass=$((pass+1))
  else
    echo "❌ FAIL: /tmp/t19.bc ausente ou vazio"
    fail=$((fail+1))
  fi
else
  echo "❌ FAIL: --emit-bc O2"
  fail=$((fail+1))
fi

echo
echo "▶︎ Teste extra: --opt nivel inválido (deve falhar)"
"$BIN" --opt=banana --emit-ll -o /tmp/t108.ll "$ROOT/tests/108_opt_invalid_level.my" >/dev/null 2>&1
rc=$?
if [[ $rc -ne 0 ]]; then
  echo "✅ OK(F): --opt=banana falhou como esperado"
  pass=$((pass+1))
else
  echo "❌ FAIL: deveria falhar com --opt=banana"
  fail=$((fail+1))
fi

echo
echo "Resumo: pass=$pass fail=$fail"
[[ $fail -eq 0 ]] || exit 1
