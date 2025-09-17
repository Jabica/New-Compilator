#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

echo "▶︎ Quickstart mycc-pt"

need() { command -v "$1" >/dev/null 2>&1; }
have_brew() { command -v brew >/dev/null 2>&1; }
have_apt() { command -v apt-get >/dev/null 2>&1; }

miss=()
for t in cmake ninja clang llvm-config; do
  need "$t" || miss+=("$t")
done

if (( ${#miss[@]} > 0 )); then
  echo "⚠️  Dependências faltando: ${miss[*]}"
  if have_brew; then
    echo "💡 Instalando via Homebrew..."
    brew install llvm cmake ninja || true
    export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
  elif have_apt; then
    echo "💡 Instalando via apt (requer sudo)..."
    sudo apt-get update
    sudo apt-get install -y llvm clang cmake ninja-build || true
  else
    echo "ℹ️  Instale manualmente: LLVM/Clang, CMake e Ninja."
  fi
fi

if command -v llvm-config >/dev/null 2>&1; then export LLVM_DIR="$(llvm-config --cmakedir)"; fi
if [[ -z "${LLVM_DIR:-}" ]]; then
  for p in /opt/homebrew/opt/llvm/lib/cmake/llvm /usr/local/opt/llvm/lib/cmake/llvm; do
    [[ -d "$p" ]] && export LLVM_DIR="$p" && break
  done
fi

echo "▶︎ Build + A3 All"
"$ROOT/scripts/run_a3_all.sh"

echo "▶︎ Pacote final (dist/)"
"$ROOT/scripts/run_a3_finalize.sh" || true

echo "✅ Pronto. Veja dist/mycc-pt-a3.tar.gz"

