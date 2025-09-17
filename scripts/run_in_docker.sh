#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
IMG_NAME="mycc-pt"

if ! command -v docker >/dev/null 2>&1; then
  echo "❌ Docker não encontrado. Instale Docker Desktop ou Engine." >&2
  exit 1
fi

if ! docker image inspect "$IMG_NAME" >/dev/null 2>&1; then
  echo "▶︎ Construindo imagem Docker ($IMG_NAME)..."
  docker build -t "$IMG_NAME" "$ROOT"
fi

echo "▶︎ Rodando scripts/run_a3_all.sh dentro do container..."
docker run --rm -it -v "$ROOT":/work "$IMG_NAME" ./scripts/run_a3_all.sh

