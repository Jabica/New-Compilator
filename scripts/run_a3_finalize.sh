#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BUILD="$ROOT/build"
BIN="$BUILD/mycc_cli"
bold(){ printf "\033[1m%s\033[0m\n" "$*"; }
ok(){ printf "   ✅ %s\n" "$*"; }
warn(){ printf "   ⚠️  %s\n" "$*"; }
fail(){ printf "   ❌ %s\n" "$*"; }
need_cmd(){ command -v "$1" >/dev/null 2>&1 || { fail "Comando ausente: $1"; exit 1; }; }

bold "▶︎ 1) Checando ambiente"
need_cmd cmake; need_cmd bash; need_cmd grep; need_cmd awk; need_cmd sed; need_cmd tar
if command -v ninja >/dev/null 2>&1; then GEN="-G Ninja"; ok "Ninja OK"; else GEN=""; warn "Ninja não encontrado — usando Makefiles"; fi
if command -v llvm-config >/dev/null 2>&1; then export LLVM_DIR="$(llvm-config --cmakedir)"; ok "LLVM OK (LLVM_DIR=$LLVM_DIR)"; else warn "llvm-config não encontrado — instale LLVM (Homebrew)."; fi

bold "▶︎ 2) Build do projeto"
if [[ ! -x "$BIN" ]]; then cmake -S "$ROOT" -B "$BUILD" $GEN ${LLVM_DIR:+-DLLVM_DIR="$LLVM_DIR"}; cmake --build "$BUILD" --config Release; ok "Build concluído"; else ok "Binário já presente"; fi

bold "▶︎ 3) Válidos/Inválidos (A3 All)"
if [[ ! -x "$ROOT/scripts/run_a3_all.sh" ]]; then fail "scripts/run_a3_all.sh ausente"; exit 1; fi
bash "$ROOT/scripts/run_a3_all.sh"; ok "A3 All OK"

bold "▶︎ 4) --help / --version"
HELP_OUT="$("$BIN" --help 2>&1 || true)"
FLAGS_REQ=( "--help" "--version" "--parse-only" "--dump-ast" "--dump-ir" "--emit-ll" "--run" "--verify-ir" "--fast2d=" "--vec2d=" "--unroll2d=" )
MISS=0; for f in "${FLAGS_REQ[@]}"; do printf "%s" "$HELP_OUT" | grep -q "$f" || { warn "Flag ausente no --help: $f"; MISS=1; }; done
[[ "$MISS" -eq 0 ]] && ok "--help completo" || warn "Atualize a ajuda no CLI"
VER_OUT="$("$BIN" --version 2>&1 || true)"
printf "%s" "$VER_OUT" | grep -Eiq 'mycc-pt|mycc.*v[0-9]+\.[0-9]+' && ok "--version OK: $VER_OUT" || warn "--version ausente"

bold "▶︎ 5) Goldens (--dump-ast / --dump-ir)"
mkdir -p "$ROOT/goldens"
EX="$ROOT/examples/02_soma_funcoes.my"
if [[ -f "$EX" ]]; then "$BIN" --dump-ast "$EX" > "$ROOT/goldens/02_soma_funcoes.ast.txt"; "$BIN" --dump-ir "$EX" > "$ROOT/goldens/02_soma_funcoes.ir.txt"; ok "Goldens em goldens/"; else warn "Exemplo 02 ausente; pulando goldens"; fi

bold "▶︎ 6) Verificador LLVM (amostra)"
"$BIN" --emit-ll "$ROOT/examples/04_fibonacci_iter.my" --verify-ir >/dev/null 2>&1 && ok "Verifier OK" || warn "Verifier reportou problemas"

bold "▶︎ 7) Empacote (dist/mycc-pt-a3.tar.gz)"
DIST="$ROOT/dist"; PKG="$DIST/mycc-pt-a3"; rm -rf "$PKG"; mkdir -p "$PKG/bin" "$PKG/examples" "$PKG/examples_invalid" "$PKG/scripts" "$PKG/docs" "$PKG/goldens"
cp "$BIN" "$PKG/bin/" || { fail "Falha ao copiar bin"; exit 1; }
for s in run_a3_all.sh run_a3_demo.sh run_a3_invalids.sh run_a3_finalize.sh; do [[ -f "$ROOT/scripts/$s" ]] && cp "$ROOT/scripts/$s" "$PKG/scripts/" || warn "Script ausente: $s"; done
if [[ -f "$ROOT/docs/guia.md" ]]; then cp "$ROOT/docs/guia.md" "$PKG/docs/"; else
  cat > "$PKG/docs/guia.md" <<'MD'
# mycc-pt — Guia rápido (A3)
## Build
cmake -S . -B build -G Ninja ${LLVM_DIR:+-DLLVM_DIR="$LLVM_DIR"}
cmake --build build --config Release
## Uso
mycc_cli --help
## Execução da A3
./scripts/run_a3_all.sh
MD
  ok "docs/guia.md mínimo criado"
fi
[[ -d "$ROOT/examples" ]] && cp -a "$ROOT/examples/." "$PKG/examples/" || warn "examples/ ausente"
[[ -d "$ROOT/examples_invalid" ]] && cp -a "$ROOT/examples_invalid/." "$PKG/examples_invalid/" || warn "examples_invalid/ ausente"
[[ -d "$ROOT/goldens" ]] && cp -a "$ROOT/goldens/." "$PKG/goldens/" || true
mkdir -p "$DIST"; tar -C "$DIST" -czf "$DIST/mycc-pt-a3.tar.gz" "mycc-pt-a3"; ok "Pacote: $DIST/mycc-pt-a3.tar.gz"
bold "Fim ✔"

