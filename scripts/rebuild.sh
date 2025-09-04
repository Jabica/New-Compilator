

#!/usr/bin/env bash
set -euo pipefail

# Raiz do projeto (um nível acima da pasta scripts)
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD="${ROOT}/build"

# Caminho do llvm-config (Homebrew Apple Silicon)
LLVM_CONFIG="${LLVM_CONFIG:-/opt/homebrew/opt/llvm/bin/llvm-config}"

echo "[mycc-pt] Limpando build..."
rm -rf "${BUILD}"
mkdir -p "${BUILD}"
cd "${BUILD}"

echo "[mycc-pt] Configurando CMake (Ninja)..."
cmake -G Ninja -DLLVM_DIR="$(${LLVM_CONFIG} --cmakedir)" ..

echo "[mycc-pt] Compilando..."
ninja

echo "[mycc-pt] Pronto! Binário em: ${BUILD}/mycc_cli"