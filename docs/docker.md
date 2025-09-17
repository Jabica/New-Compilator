# Guia Docker — mycc-pt

Este guia mostra como usar o mycc‑pt apenas com Docker, sem instalar LLVM/CMake/Ninja no host.

## Pré‑requisitos

- Docker Desktop (macOS/Windows) ou Docker Engine (Linux).
- Espaço em disco para a imagem base (Ubuntu + LLVM 17 + toolchain).

## 1) Construir a imagem

No diretório raiz do repositório (onde está o `Dockerfile`):

```bash
docker build -t mycc-pt .
```

Isso cria uma imagem com:
- Ubuntu 24.04
- LLVM 17 (llvm-17-dev), Clang 17, LLD 17
- CMake e Ninja
- Variáveis de ambiente ajustadas (`LLVM_DIR`, `CC`, `CXX`).

## 2) Executar a suíte A3

Monte o diretório atual no container para que artefatos apareçam no host:

```bash
docker run --rm -it -v "$PWD":/work mycc-pt ./scripts/run_a3_all.sh
```

Isso fará build + execução dos 5 exemplos válidos e 5 inválidos. Saídas no host:
- `build/` (binários, objetos, IR)
- `dist/mycc-pt-a3.tar.gz` (se você rodar o finalize)

## 3) Executar válidos/invalidos separadamente

```bash
docker run --rm -it -v "$PWD":/work mycc-pt ./scripts/run_a3_demo.sh
docker run --rm -it -v "$PWD":/work mycc-pt ./scripts/run_a3_invalids.sh
```

## 4) Pacote final

```bash
docker run --rm -it -v "$PWD":/work mycc-pt ./scripts/run_a3_finalize.sh
# artefato em dist/mycc-pt-a3.tar.gz no host
```

## 5) Shell interativa no container

Para explorar o ambiente (CMake/Ninja/LLVM/Clang) e rodar comandos manualmente:

```bash
docker run --rm -it -v "$PWD":/work mycc-pt /bin/bash
# exemplo: cmake -S . -B build -G Ninja -DLLVM_DIR="$LLVM_DIR" && cmake --build build
```

## 6) Limpeza

No host:
```bash
rm -rf build dist goldens
```

Opcional: remover a imagem para liberar espaço:
```bash
docker rmi mycc-pt
```

## 7) Dicas

- Apple Silicon (arm64): a imagem base é multi‑arch e o Clang 17 do Ubuntu suporta a toolchain arm64; não é necessário `--platform`. Se você tiver exigências x86_64 específicas, use `--platform=linux/amd64` (pode exigir emulação).
- Performance: use bind mount (`-v "$PWD":/work`) para reutilizar `build/` entre execuções.
- Scripts: você pode usar `./scripts/run_in_docker.sh` para automatizar o build da imagem e a execução do `run_a3_all.sh`.

