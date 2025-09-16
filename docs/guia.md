# Guia Completo — mycc-pt (A3)

Este guia descreve como instalar, compilar e usar o compilador educacional mycc-pt, detalha cada flag do CLI, os conceitos da linguagem, e explica a base utilizada para os testes automatizados.

Versão atual: 0.1 — `mycc_cli --version` imprime algo como `mycc-pt v0.1.0 (R2-A3)`

—

## 1) Instalação e Build

Pré‑requisitos gerais:
- LLVM com arquivos de configuração (`LLVMConfig.cmake`)
- CMake ≥ 3.16 e Ninja
- Clang (para linkar executáveis no `--emit-exe`)
- Opcional: `lli`, `llvm-dis`, `llvm-dwarfdump` (para executar e inspecionar resultados em testes)

macOS (Apple Silicon):
```bash
xcode-select --install
brew install llvm cmake ninja
./scripts/rebuild.sh   # detecta automaticamente o LLVM_DIR via llvm-config
```
O script detecta o `LLVM_DIR` via `llvm-config` do Homebrew (`/opt/homebrew/opt/llvm/bin/llvm-config`).

Linux (exemplo com LLVM 17):
```bash
sudo apt-get install -y llvm-17 clang-17 cmake ninja-build
export LLVM_CONFIG=$(command -v llvm-config-17)
./scripts/rebuild.sh
```
Build manual (alternativa):
```bash
mkdir -p build && cd build
cmake -G Ninja -DLLVM_DIR="$($LLVM_CONFIG --cmakedir)" ..
ninja
```

Artefatos em `build/`:
- `mycc_cli` (binário do CLI)
- `libmycc.a` (biblioteca principal)
- `libmycc_runtime.a` (runtime C para linkedição com `--emit-exe`)

—

## 2) Conceitos da Linguagem

Tipos:
- `inteiro`, `logico`, `vazio`, `texto` (strings literais)
- Vetores 1D de tamanho fixo: `inteiro[N]`, `logico[N]`

Literais e identificadores:
- Inteiros: `0`, `42`
- Booleanos: `verdadeiro`, `falso`
- Strings: `"abc"` com escapes `\n`, `\t`, `\\`, `\"`

Declarações e escopos:
- Locais: `variavel x: inteiro = 5;`
- Globais (topo do arquivo): `const inteiro X = 10;`, `const logico T[3] = {verdadeiro, falso, verdadeiro};`

Funções:
```
funcao nome(a: inteiro, b: logico): inteiro {
  retorna a;
}
```

Controle de fluxo:
- `se (...) {...} senao {...}`
- `enquanto (...) {...}`
- `for (init; cond; step) {...}`
- `break`, `continue`, `retorna [expr];`

Vetores e indexação:
- Declaração: `variavel v: inteiro[5];`
- Acesso: `v[i]`, atribuição: `v[i] = expr;`

Operadores:
- Aritméticos: `+ - * / %` (inteiros)
- Unário: `!` (lógico), `-` (numérico)
- Comparações: `< <= > >= == !=` (escalares)
- Lógicos: `&& ||` com curto‑circuito

Conversões implícitas (escalares):
- `logico -> inteiro` permitido
- `inteiro -> logico` apenas se for booleano (literal/const 0 ou 1, ou variável “bool‑like”)
- Arrays: sem conversão implícita; retorno de array por valor não é suportado

Built‑ins:
- `printi(inteiro)`, `printb(logico)`, `prints(texto)`

Otimizações 2D e CF (highlights):
- `--fast2d=<off|auto|always>`: `copy2d` com fast‑path contíguo (um único `llvm.memcpy` quando o retângulo é contíguo). Caso contrário, memcpy por linha.
- `--vec2d=<off|auto|always>`: `fill2d(value!=0)` vetorizado `<4 x i32>` por linha; tail escalar.
- `--unroll2d=<off|auto|always>`: fallback escalar desenrolado em passos de 8 (×8) + tail.
- `--verify-ir`: pede verificação do IR (também existem smokes de `opt -verify`).
- Folding conservador: `se(const)` elimina ramo impossível; `enquanto(false)` não entra no laço. Branch weights básicos (MD_prof) são aplicados aos `condbr`.
- `break/continue` (também `quebra/continua`) em `enquanto` têm suporte.

—

## 3) CLI e Flags (o que cada uma faz)

Forma geral:
```bash
mycc_cli [modo/flags] arquivo.my [opções]
```
Se o primeiro argumento não for flag, o modo padrão é `--check`.

Modos principais:
- `--help`: mostra ajuda
- `--parse-only`: apenas léxico/sintático
- `--dump-ast`: imprime a AST
- `--dump-ir`: gera IR e valida com `verifyModule`
- `--check`: checagem semântica (retorno 0/erro)
- `--run`: gera IR e executa com `lli`
- `--emit-ll[=<arq>]` ou `--emit-llvm[=<arq>]`: salva IR textual `.ll`
- `--emit-bc[=<arq>]`: salva bitcode `.bc`
- `--emit-asm[=<arq>]`: salva assembly `.s`
- `--emit-obj[=<arq>]`: salva objeto `.o`
- `--emit-exe[=<arq>]`: linka executável nativo (usa `clang`)
- `--emit-ll-opt[=<arq>]`: salva IR já otimizado (ver seção de otimização)

Opções gerais:
- `-o <arquivo>`: define o caminho de saída para os modos de emissão
- `-g` ou `--debug`: inclui debug info (DWARF) em `.ll`, `.o`, `.s`
- `--target=<triple>`: define o target (ex.: `aarch64-apple-darwin`)
- `--rt=<arquivo>`: caminho para `libmycc_runtime.a` ao linkar com `--emit-exe`

Otimização e pipeline:
- `--opt[=O0|O1|O2|O3|Os|Oz]` ou níveis `-O0|-O1|-O2|-O3|-Os|-Oz`
- `--opt-pipeline=<texto>`: pipeline textual custom do PassBuilder
- `--print-pipeline`: imprime o pipeline antes de rodar

Sanitizers (educacional):
- `--ubsan`: injeta checagem de divisão por zero no IR
- `--asan`: reservado para futura instrumentação

Exemplos:
```bash
# Checagem padrão
./build/mycc_cli tests/sample.my

# Geração de IR/obj/asm/bitcode
./build/mycc_cli --emit-ll  -o /tmp/p.ll tests/sample.my
./build/mycc_cli --emit-bc  -o /tmp/p.bc tests/sample.my
./build/mycc_cli --emit-asm -o /tmp/p.s  tests/sample.my
./build/mycc_cli --emit-obj -o /tmp/p.o  tests/sample.my

# Linkar executável (usa runtime padrão embutido; pode sobrescrever com --rt)
./build/mycc_cli --emit-exe -o /tmp/p tests/sample.my

# Executar via lli
./build/mycc_cli --run tests/sample.my

# Otimizar e salvar IR otimizado
./build/mycc_cli --emit-ll-opt --opt=O2 -o /tmp/opt.ll tests/19_opt_constfold_ok.my

# Imprimir pipeline com -O3
./build/mycc_cli --emit-ll -O3 --print-pipeline -o /tmp/p.ll tests/23_opt_pipeline_smoke.my
```

Observações (macOS): o projeto define `CMAKE_OSX_DEPLOYMENT_TARGET=15.0` e, no `--emit-exe`, usa `xcrun` para detectar o SDK e passar `-mmacosx-version-min` automaticamente.

—

## 4) Casos Práticos

Programa com vetores e laço:
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
Emitindo e rodando:
```bash
./build/mycc_cli --emit-exe -o /tmp/prog exemplo.my
/tmp/prog
```

—

## 5) Testes: Base Utilizada e Como Rodar

Base e filosofia:
- A suíte de testes está em `tests/` com arquivos `.my` que exercitam léxico, parser, semântica, codegen e recursos de emissão.
- Os scripts em `scripts/` (principalmente `run_tests.sh`) classificam testes em dois grupos:
  - MUST_PASS: o compilador deve terminar com código 0; em alguns casos também se valida a execução do binário gerado.
  - MUST_FAIL: o compilador deve reportar erro (retorno != 0) para entradas inválidas.
- Há “smoke tests” adicionais que verificam: emissão de `.bc`/`.s`, otimizações (`--opt`, `--opt-pipeline`, `--emit-ll-opt`), targets e debug info.
- A verificação usa: retorno do `mycc_cli`, existência/conteúdo de arquivos de saída, `lli` (quando disponível), `clang` para linkedição e, opcionalmente, ferramentas LLVM (`llvm-dis`, `llvm-dwarfdump`).

Como executar a suíte principal:
```bash
./scripts/rebuild.sh
./scripts/run_tests.sh
```

Dependências dos testes:
- `lli` no PATH para casos de `--run` (os scripts tentam ajustar usando `llvm-config`).
- `clang` no PATH para casos de `--emit-exe`.

Categorias (amostras):
- Arrays: declaração, indexação, tipos de índice, tamanhos por const‑expr.
- Funções/chamadas: aridade, tipos de parâmetros e conversões permitidas.
- Controle de fluxo: `se/senao`, `enquanto`, `for`, `break/continue`.
- Retornos: presença/ausência de valor conforme o tipo de retorno.
- Globais/const: inicialização literal/const‑expr, listas em vetores, regras de atribuição.
- Texto/strings: literais de `texto` e interação com built‑ins (`prints`).
- Emissão: `.ll`, `.bc`, `.s`, `.o`, executáveis; flags de alvo e debug.
- Otimização: níveis `-O*`, pipelines textuais e impressão do pipeline.

Scripts auxiliares:
- `run_opt_tests.sh`: smoke de otimização e `--run`
- `run_target_tests.sh`: smoke de `--target=<triple>`
- `run_bc_asm_tests.sh`: smoke de `.bc` e `.s`
- `run_debug_tests.sh`: valida metadata DWARF em IR/obj/asm
- `run_loc_tests.sh`: smoke de localizações de debug

“Base utilizada para os testes” em resumo:
- Os testes são baseados na especificação funcional do próprio compilador (léxico, gramática, regras semânticas e geração de IR) e validam o comportamento via códigos de retorno, verificação de arquivos gerados e execução de artefatos quando aplicável.
- A infraestrutura de validação usa apenas ferramentas padrão (Bash + binários LLVM/Clang) — não há framework externo; tudo está versionado no repositório.

—

## 6) Dicas e Solução de Problemas

- LLVM não encontrado no CMake: passe `-DLLVM_DIR=$(llvm-config --cmakedir)` no configure ou defina `LLVM_CONFIG` antes de `./scripts/rebuild.sh`.
- `--emit-exe` falha ao linkar: confira se `clang` está no PATH e se o runtime existe (padrão: `build/libmycc_runtime.a`); use `--rt=...` para sobrescrever.
- `--run` falha: inclua o diretório dos binários LLVM no PATH (`export PATH="$(llvm-config --bindir):$PATH"`).
- macOS: o projeto já ajusta SDK/target; se ainda ver warnings, verifique sua instalação do Xcode/Command Line Tools.

—

## 7) Limitações Atuais

—

## 8) A3 — Exemplos, Scripts e Empacotamento

Exemplos (válidos):
- `examples/01_hello.my` → imprime 123
- `examples/02_soma_funcoes.my` → soma 7+35=42
- `examples/03_fatorial_iter.my` → fatorial(5)=120
- `examples/04_fibonacci_iter.my` → fib(10)=55
- `examples/05_io_soma.my` → soma determinística 10+32=42

Exemplos (inválidos):
- `examples_invalid/01_var_undeclared_err.my` → variável não declarada
- `examples_invalid/02_break_outside_loop_err.my` → `quebra` fora de laço
- `examples_invalid/03_redeclare_function_err.my` → função redeclarada
- `examples_invalid/04_call_arity_mismatch_err.my` → aridade incorreta
- `examples_invalid/05_return_type_mismatch_err.my` → retorno sem valor em função inteira

Scripts A3:
- `./scripts/run_a3_demo.sh` — build + executa os 5 válidos com `--emit-exe` e verifica stdout
- `./scripts/run_a3_invalids.sh` — valida 5 inválidos procurando substrings robustas nas mensagens
- `./scripts/run_a3_all.sh` — roda os dois anteriores em sequência
- `./scripts/run_a3_finalize.sh` — checklist final (ambiente, build, all, help/version, goldens, verifier) + empacote `dist/mycc-pt-a3.tar.gz`

Checklist rápido (15–20 min):
```bash
rm -rf build && cmake -S . -B build -G Ninja ${LLVM_DIR:+-DLLVM_DIR="$LLVM_DIR"}
cmake --build build --config Release
./scripts/run_a3_all.sh
./build/mycc_cli --help | grep -E -- "--version|--parse-only|--dump-ast|--dump-ir|--emit-ll|--run|--verify-ir|--fast2d=|--vec2d=|--unroll2d="
./build/mycc_cli --version
mkdir -p goldens
./build/mycc_cli --dump-ast examples/02_soma_funcoes.my > goldens/02.ast.txt
./build/mycc_cli --dump-ir  examples/02_soma_funcoes.my > goldens/02.ir.txt
./scripts/run_a3_finalize.sh
```

Apresentação sugerida (3–5 min):
- `--version` e `--help`
- `./scripts/run_a3_all.sh` (resumos de válidos/invalidos)
- `--dump-ast` e `--dump-ir` de `examples/02_soma_funcoes.my`
- Limitações e demonstração do pacote `dist/mycc-pt-a3.tar.gz`

- Vetores apenas 1D; sem strings dinâmicas nem operações gerais entre `texto` (além de `prints`).
- Sem retorno de arrays por valor.
- `--asan` reservado; `--ubsan` cobre explicitamente divisão por zero no IR.
- Linkedição via `clang` do sistema; toolchains alternativos podem requerer ajustes.
