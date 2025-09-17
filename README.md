# mycc-pt — Compilador Educacional em Português

Um compilador didático que implementa uma linguagem pequena (tipos inteiros, lógicos e texto, vetores 1D, funções, controle de fluxo) e gera IR do LLVM, objeto nativo, assembly e executáveis. Inclui checagem semântica, opções de otimização, debug info e execução via `lli`.

- Projeto: `mycc-pt` (versão 0.1)
- Binário: `build/mycc_cli`
- Bibliotecas: `build/libmycc.a` (core) e `build/libmycc_runtime.a` (runtime para linkedição)

Versão do CLI:
- `./build/mycc_cli --version` → exibe algo como `mycc-pt v0.1.0 (R2-A3)`

## Novidades da A3

- CLI: `--version` implementado e help atualizado.
- Otimizações 2D:
  - `--fast2d`: copy2d com fast‑path contíguo (1× `llvm.memcpy` quando possível).
  - `--vec2d`: fill2d(value!=0) vetorizado em `<4 x i32>`; tail escalar.
  - `--unroll2d`: fallback escalar desenrolado ×8 para fill2d.
- Robustez de controle de fluxo: terminadores garantidos, merges consistentes e `--verify-ir` (R2‑21).
- Folding e pesos de branch: `if(const)`/`while(false)` e `MD_prof` em `condbr` (R2‑22).
- Suporte a `break/continue` (PT‑BR: `quebra/continua`) em `enquanto`.
- Suite A3: 5 exemplos válidos e 5 inválidos + scripts (`run_a3_demo.sh`, `run_a3_invalids.sh`, `run_a3_all.sh`, `run_a3_finalize.sh`).
- Instalação opcional com `cmake --install` (bin/scripts/examples/docs em `share/mycc-pt/`).

Leitura complementar: `docs/A3.md` (conceito/arquitetura/otimizações/roteiro).

## Execução Fácil (Quickstart & Docker)

- Quickstart (nativo):
  ```bash
  ./scripts/quickstart.sh
  # faz build, roda A3 (válidos+inválidos) e gera pacote dist/
  ```

- Docker (sem dependências no host):
  ```bash
  # build da imagem
  docker build -t mycc-pt .
  # executa A3 all dentro do container com o repo montado
  docker run --rm -it -v "$PWD":/work mycc-pt ./scripts/run_a3_all.sh
  # ou simplesmente
  ./scripts/run_in_docker.sh
  ```

- Makefile (atalhos):
  ```bash
  make build      # ./scripts/rebuild.sh
  make a3         # ./scripts/run_a3_all.sh
  make dist       # ./scripts/run_a3_finalize.sh
  make docker     # ./scripts/run_in_docker.sh
  make quickstart # ./scripts/quickstart.sh
  ```

## Visão Geral

- Front-end completo: léxico, parser, AST e checagens semânticas (escopos, tipos, retorno, arrays, conversões válidas, etc.).
- Back-end LLVM: geração de IR com suporte a otimizações (`--opt`/`-O*`), emissão de `.ll`, `.bc`, `.s`, `.o` e linkedição para executável (`--emit-exe`).
- Execução direta: `--run` usa `lli` para rodar o IR sem gerar binário.
- Debug info: `-g/--debug` emite DWARF em `.ll`, `.o`, `.s`.
- Sanitizers (educacional): `--ubsan` injeta checagens de divisão por zero no IR; `--asan` reservado.
- Alvos: `--target=<triple>` para escolher triple (ex.: `aarch64-apple-darwin`, `x86_64-apple-darwin`).

Otimizações 2D (R2-17…R2-19):
- `--fast2d=<off|auto|always>`: copia 2D com fast‑path contíguo (um único `llvm.memcpy` quando `cols==stride` e índices alinhados). Fallback: memcpy por linha.
- `--vec2d=<off|auto|always>`: `fill2d(value!=0)` vetorizado `<4 x i32>` por linha (splat + stores vetoriais). Fallback: escalar.
- `--unroll2d=<off|auto|always>`: unroll escalar ×8 no fallback de `fill2d`, com tail.

Robustez de controle de fluxo (R2-21) e folding (R2-22):
- Terminadores garantidos (br/ret) e blocos de merge consistentes em `se/senao` e `enquanto`.
- `--verify-ir` roda o verificador LLVM no módulo gerado.
- Folding conservador: `se(const)` elimina ramo impossível; `enquanto(false)` vira no‑op. Branch weights (MD_prof) básicos são aplicados em `condbr`.
- `break`/`continue` em `enquanto` são suportados (com `quebra`/`continua`).

## Requisitos

- CMake ≥ 3.16 e Ninja
- LLVM (com `LLVMConfig.cmake` disponível)
- Clang (para `--emit-exe`)
- Opcional: `lli`, `llvm-dis`, `llvm-dwarfdump` (para scripts/tests)

## Instalação / Build

Você pode usar o script de build (recomendado) ou rodar CMake manualmente.

- macOS (Apple Silicon):
  ```bash
  xcode-select --install
  brew install llvm cmake ninja
  # Build
  ./scripts/rebuild.sh
  ```
  O script detecta `LLVM_DIR` via `llvm-config` do Homebrew (`/opt/homebrew/opt/llvm/bin/llvm-config`).

- Linux (exemplo com LLVM 17):
  ```bash
  sudo apt-get install -y llvm-17 clang-17 cmake ninja-build
  export LLVM_CONFIG=$(command -v llvm-config-17)
  ./scripts/rebuild.sh
  # ou manualmente:
  mkdir -p build && cd build
  cmake -G Ninja -DLLVM_DIR="$($LLVM_CONFIG --cmakedir)" ..
  ninja
  ```

Artefatos ficam em `build/`.

Dicas:
- Para `--run`, garanta `lli` no PATH (ex.: `export PATH="$(llvm-config --bindir):$PATH"`).
- Em macOS, `--emit-exe` usa `xcrun` para detectar o SDK e define `-mmacosx-version-min=15.0` (ajustado no CMake).

Instalação (opcional):
```bash
sudo cmake --install build
# Binário: /usr/local/bin/mycc_cli (ou conforme CMAKE_INSTALL_PREFIX)
# Recursos: /usr/local/share/mycc-pt/ (examples, scripts A3, docs)
```

## Uso Rápido

- Checar um arquivo (sem flags cai em `--check`):
  ```bash
  ./build/mycc_cli path/arquivo.my
  ```

- Ajuda completa:
  ```bash
  ./build/mycc_cli --help
  ```

- Geração de IR/obj/asm/bitcode e executável:
  ```bash
  ./build/mycc_cli --emit-ll  -o prog.ll     prog.my
  ./build/mycc_cli --emit-bc  -o prog.bc     prog.my
  ./build/mycc_cli --emit-asm -o prog.s      prog.my
  ./build/mycc_cli --emit-obj -o prog.o      prog.my
  ./build/mycc_cli --emit-exe -o prog        prog.my   # requer clang
  ```

- Execução via `lli` (sem binário):
  ```bash
  ./build/mycc_cli --run prog.my
  ```

- Otimizações e pipeline:
  ```bash
  ./build/mycc_cli --emit-ll-opt -o prog.opt.ll --opt=O2   prog.my
  ./build/mycc_cli --emit-ll -o prog.ll -O3 --print-pipeline prog.my
  ./build/mycc_cli --emit-ll-opt --opt-pipeline="default<O2>" -o prog.ll prog.my
  ```

- Alvo específico e debug info:
  ```bash
  ./build/mycc_cli --target=aarch64-apple-darwin --emit-obj -g -o prog.o prog.my
  ```

- Runtime para `--emit-exe`:
  - Por padrão, usa `build/libmycc_runtime.a` (caminho embutido). Para customizar:
    ```bash
    ./build/mycc_cli --emit-exe --rt=/caminho/para/libmycc_runtime.a -o prog prog.my
    ```

## A Linguagem (resumo)

- Tipos: `inteiro`, `logico`, `vazio`, `texto` (string literal). Vetores 1D com tamanho fixo: `inteiro[N]`, `logico[N]`.
- Literais: inteiros (ex.: `123`), booleanos (`verdadeiro`, `falso`), strings (`"abc"`, com escapes `\n`, `\t`, `\\`, `\"`).
- Declarações:
  - Variáveis locais: `variavel x: inteiro = 5;`
  - Globais (topo do arquivo), com suporte a `const` e inicialização:
    - Escalar: `const inteiro X = 10;`
    - Vetor: `const logico T[3] = {verdadeiro, falso, verdadeiro};`
- Funções: `funcao nome(p1: tipo, p2: tipo): retorno { ... }`
- Controle de fluxo: `se`/`senao`, `enquanto`, `for (init; cond; step)`, `break`, `continue`, `retorna`.
- Indexação de vetores: `v[i]` (índice inteiro). Atribuição à posição: `v[i] = expr;`.
- Operadores: `+ - * / %` (inteiros), `!` unário, comparações `< <= > >= == !=` (escalares), lógicos `&& ||` com curto‑circuito. Expressões avaliam para `inteiro` (bools retornam `0/1`).
- Conversões implícitas (escalares):
  - `logico -> inteiro` permitido.
  - `inteiro -> logico` somente se valor for booleano (literal/const 0 ou 1, ou variável marcada "bool‑like").
  - Arrays: sem conversões implícitas nem retorno por valor.
- Built-ins: `printi(inteiro)`, `printb(logico)`, `prints(texto)` disponíveis.

Exemplo:
```my
funcao principal(): inteiro {
  variavel v: inteiro[5];
  v[0] = 1; v[1] = 2; v[2] = 3; v[3] = 4; v[4] = 5;
  variavel s: inteiro = 0;
  variavel i: inteiro = 0;
  enquanto (i < 5) { s = s + v[i]; i = i + 1; }
  prints("soma = "); printi(s); // 15
  retorna 0;
}
```

## Testes

Scripts úteis:
- `./scripts/rebuild.sh` — limpa e recompila (detecta `LLVM_DIR` via `llvm-config`).
- `./scripts/run_tests.sh` — roda a suíte principal (casos que devem passar/falhar + smokes de emissão/otimização/alvo/debug).
- Smokes adicionais: `run_opt_tests.sh`, `run_target_tests.sh`, `run_bc_asm_tests.sh`, `run_debug_tests.sh`, `run_loc_tests.sh`.

Suite A3 (exemplos e entrega):
- Válidos: `examples/01_hello.my` … `05_io_soma.my`
- Inválidos: `examples_invalid/01_*.my` … `05_*.my`
- Scripts:
  - `./scripts/run_a3_demo.sh` → compila com `--emit-exe` e valida o stdout (5/5 válidos)
  - `./scripts/run_a3_invalids.sh` → valida 5 inválidos (erros semânticos esperados)
  - `./scripts/run_a3_all.sh` → executa válidos + inválidos, com resumos
  - `./scripts/run_a3_finalize.sh` → checklist final + pacote `dist/mycc-pt-a3.tar.gz`

Pré‑requisitos dos testes:
- `lli` no PATH para `--run` (o script tenta ajustar via `llvm-config`).
- `clang` disponível para os testes de `--emit-exe`.

## Dicas e Solução de Problemas

- CMake não encontra LLVM: passe `-DLLVM_DIR=$(llvm-config --cmakedir)` no configure ou defina `LLVM_CONFIG` antes de `./scripts/rebuild.sh`.
- `--emit-exe` falha: garanta `clang` no PATH e que o runtime exista (padrão: `build/libmycc_runtime.a`). Use `--rt=...` para customizar.
- `--run` falha com `lli`: adicione `$(llvm-config --bindir)` ao `PATH`.
- macOS: se vir warnings de SDK/target, o projeto já define `CMAKE_OSX_DEPLOYMENT_TARGET` e usa `xcrun` para `--emit-exe`.

## Estrutura do Repositório

- `src/` — lexer, parser, AST, semântica, codegen, CLI e runtime C.
- `include/` — headers públicos (ex.: `include/mycc/version.hpp`).
- `tests/` — casos positivos/negativos e smokes de emissão/otimização.
- `scripts/` — build e runners de testes.
- `CMakeLists.txt` — build com `find_package(LLVM CONFIG)`.
- `docs/guia.md` — anotações rápidas (WIP).
  - Guia completo para A3 (build, uso, exemplos, scripts e empacotamento)

## Limitações Atuais

- Somente vetores 1D de tamanho fixo; sem strings dinâmicas ou operações entre `texto` fora de `prints`.
- Sem retorno de arrays por valor; sem ponteiros/structs.
- `--asan` reservado; `--ubsan` cobre divisão por zero no IR.
- Linkedição via `clang` do sistema; toolchains não‑padrão podem exigir ajustes.

---

Sinta‑se à vontade para abrir issues/sugestões. Bom estudo e bons hacks no LLVM!
