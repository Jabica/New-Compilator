# Changelog — mycc-pt

Todas as mudanças notáveis deste projeto serão documentadas aqui.

## v0.1.0 — A3 (R2‑17…R2‑22)

Data: 2025‑09

Novidades:
- CLI
  - `--version` (ex.: `mycc-pt v0.1.0 (R2-A3)`)
  - Help atualizado (flags 2D e verificação de IR)
- Otimizações 2D
  - `--fast2d` (R2‑17): copy2d com fast‑path contíguo (um único `llvm.memcpy` quando bloco contíguo)
  - `--vec2d` (R2‑18): fill2d(value!=0) vetorizado `<4 x i32>` por linha (splat + store vetorial), tail escalar
  - `--unroll2d` (R2‑19): fallback escalar desenrolado em passos de 8 (×8) + tail
- Robustez de controle de fluxo (R2‑21)
  - Terminadores garantidos (br/ret) e blocos de merge
  - `--verify-ir` e smokes de verificação
- Folding + Pesos de Branch + `break/continue` (R2‑22)
  - Folding conservador: `if(const)`/`while(false)`
  - MD_prof em condbr (pesos básicos)
  - Suporte a `break/continue` (também `quebra/continua`) em `enquanto`
- A3 — Scripts e exemplos
  - 5 exemplos válidos (prints) e 5 inválidos (erros semânticos clássicos)
  - Scripts: `run_a3_demo.sh`, `run_a3_invalids.sh`, `run_a3_all.sh`, `run_a3_finalize.sh`
  - Empacote final: `dist/mycc-pt-a3.tar.gz` (bin/scripts/examples/docs)

Notas técnicas:
- IR com blocos sempre terminados; merges consistentes; verificação LLVM habilitável via CLI.
- Vetorização/Unroll conservadores, com fallback escalar quando heurísticas não se aplicam.
- Instalação opcional via `cmake --install` para distribuição de binário e materiais.

