#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"

if [[ -x "$ROOT/scripts/run_a3_demo.sh" ]]; then
  "$ROOT/scripts/run_a3_demo.sh"
else
  echo "(aviso) run_a3_demo.sh não encontrado; pulando válidos"
fi

if [[ -x "$ROOT/scripts/run_a3_invalids.sh" ]]; then
  "$ROOT/scripts/run_a3_invalids.sh"
else
  echo "(aviso) run_a3_invalids.sh não encontrado"; exit 1
fi

