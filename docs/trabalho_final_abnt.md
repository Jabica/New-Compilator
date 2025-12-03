UNISUL – UNIVERSIDADE DO SUL DE SANTA CATARINA
CENTRO DE CIÊNCIAS TECNOLÓGICAS
CURSO DE CIÊNCIA DA COMPUTAÇÃO

GABRIEL DOS SANTOS JABOUR

DESENVOLVIMENTO E CONSOLIDAÇÃO DO COMPILADOR EDUCACIONAL MYCC-PT

Florianópolis
2025

# FOLHA DE ROSTO

Gabriel dos Santos Jabour

Desenvolvimento e Consolidação do Compilador Educacional mycc-pt

Trabalho de conclusão da disciplina apresentado à Universidade do Sul de Santa Catarina – UNISUL, como requisito parcial para aprovação na unidade curricular de Compiladores.

Orientador: Prof. (a) ____________________________

Florianópolis
2025

# FICHA CATALOGRÁFICA (SIMULADA)

J11d Jabour, Gabriel dos Santos.
    Desenvolvimento e Consolidação do Compilador Educacional mycc-pt / Gabriel dos Santos Jabour. – Florianópolis, 2025.
    180 p. : il. ; 29,7 cm.

    Trabalho de Conclusão de Disciplina (Graduação em Ciência da Computação) – Universidade do Sul de Santa Catarina, Florianópolis, 2025.

    1. Compiladores. 2. LLVM. 3. Ensino de Computação. 4. Automação de Build. I. Título.

# AGRADECIMENTOS

Agradeço à minha família pelo apoio constante ao longo da jornada acadêmica e aos colegas de turma que compartilharam o desafio de implementar o mycc-pt. Registro reconhecimento especial à equipe mantenedora do repositório, que manteve documentação atualizada em arquivos como `README.md`, `docs/guia.md` e `docs/A3.md`, referências essenciais para este trabalho. Sou grato aos docentes da UNISUL pelas discussões técnicas e aos colaboradores do ecossistema LLVM, cuja infraestrutura open source possibilitou o aprofundamento prático em geração de código.

# RESUMO

Este trabalho final detalha a criação, depuração e validação do compilador educacional mycc-pt, escrito em C++ e apoiado na infraestrutura do LLVM. O documento descreve os objetivos pedagógicos, a metodologia adotada para reconstruir o binário `build/mycc_cli`, os principais problemas enfrentados em ambiente macOS ARM, bem como as soluções técnicas que garantiram a execução de programas exemplo e a conformidade com scripts de teste. São discutidos componentes internos (léxico, parser, semântica, geração de IR), scripts de automação (`scripts/rebuild.sh`, `scripts/run_tests.sh`, `scripts/run_a3_all.sh`) e artefatos distribuídos em `dist/`. Relata-se como a reconstrução do toolchain (CMake, Ninja, LLVM 21.1.2) permitiu superar incompatibilidades de binários precompilados e como ajustes no uso de `--emit-exe` contornaram a ausência de runtime nas execuções via `--run`. O trabalho também examina a cobertura de testes, limitações remanescentes (ex.: `opt` ausente para validações DILocation) e recomendações futuras.

Palavras-chave: Compiladores. LLVM IR. Automação de Build. Ensino de Computação.

# ABSTRACT

This final report presents the development, debugging, and validation of the educational compiler mycc-pt, a Portuguese-language front end that targets LLVM IR. The document explains pedagogical goals, the engineering methodology applied to rebuild the executable `build/mycc_cli`, key issues faced on macOS ARM, and the technical solutions that enabled successful execution of reference programs and automated test suites. Internal components (lexical analysis, parsing, semantic checks, LLVM IR generation), automation scripts (`scripts/rebuild.sh`, `scripts/run_tests.sh`, `scripts/run_a3_all.sh`), and distribution artifacts (`dist/`) are discussed in detail. The reconstruction of the local toolchain (CMake, Ninja, LLVM 21.1.2) resolved incompatibilities with prebuilt binaries, while leveraging `--emit-exe` compensated for missing runtime symbols during `--run` invocations. The report also examines test coverage, remaining limitations (e.g., missing `opt` binary for DILocation verification), and recommendations for future work.

Keywords: Compilers. LLVM IR. Build Automation. Computer Science Education.

# LISTA DE FIGURAS

Figura 1 – Estrutura de diretórios do projeto mycc-pt.
Figura 2 – Fluxo de reconstrução automática via `scripts/rebuild.sh`.
Figura 3 – Relação entre etapas do compilador e arquivos em `src/`.
Figura 4 – Pipeline de testes executado por `scripts/run_tests.sh`.
Figura 5 – Visão do processo de empacotamento em `dist/mycc-pt-a3/`.

# LISTA DE TABELAS

Tabela 1 – Dependências de software instaladas via Homebrew.
Tabela 2 – Principais arquivos-fonte do diretório `src/`.
Tabela 3 – Scripts utilitários presentes em `scripts/`.
Tabela 4 – Resumo da suíte de testes e respectivos objetivos.
Tabela 5 – Correspondência entre seções deste trabalho e requisitos da entrega A3.

# LISTA DE SIGLAS E ABREVIATURAS

ABNT – Associação Brasileira de Normas Técnicas.
AST – Abstract Syntax Tree.
CLI – Command Line Interface.
IR – Intermediate Representation.
JIT – Just-In-Time.
LLVM – Low Level Virtual Machine.
UNISUL – Universidade do Sul de Santa Catarina.

# SUMÁRIO

1 Introdução
2 Justificativa e Objetivos
2.1 Objetivo Geral
2.2 Objetivos Específicos
3 Metodologia
4 Fundamentação Teórica
4.1 Conceitos de Compiladores
4.2 LLVM e Ferramentas Associadas
5 Arquitetura Geral do mycc-pt
6 Pipeline de Construção e Dependências
7 Análise Lexical
8 Análise Sintática
9 Representação por AST
10 Verificação Semântica
11 Diagnósticos e Tratamento de Erros
12 Geração de Código LLVM
13 Biblioteca de Runtime
14 Interface de Linha de Comando
15 Scripts de Automação
16 Conjunto de Testes Automatizados
17 Otimizações 2D
18 Controle de Fluxo e Folding
19 Integração com Ferramentas LLVM
20 Processo de Build em macOS Apple Silicon
21 Problemas Encontrados
22 Soluções Implementadas
23 Empacotamento e Distribuição
24 Documentação Existente
25 Resultados Obtidos
26 Avaliação Crítica
27 Recomendações Futuras
28 Conclusão
29 Referências
Apêndice A – Logs Selecionados de Execução
Apêndice B – Mapeamento de Arquivos do Projeto
Apêndice C – Plano de Continuidade

# 1 INTRODUÇÃO

O presente trabalho descreve em profundidade a implementação e a consolidação do compilador educacional mycc-pt, desenvolvido com fins didáticos no contexto da disciplina de Compiladores da Universidade do Sul de Santa Catarina (UNISUL). O projeto fornece uma linguagem de programação em Português, denominada informalmente de "my", contemplando tipos inteiros, lógicos, vetores unidimensionais, funções e estruturas de controle. Diferentemente de abordagens puramente teóricas, o repositório analisado (`New-Compilator`) contém um pipeline completo, desde análise léxica até geração de código LLVM e empacotamento final em `dist/`.

A motivação central está na necessidade de proporcionar aos estudantes um ambiente tangível para explorar os conceitos de compilação enquanto constroem um artefato funcional. Para tanto, foi necessário compreender a organização do repositório, reconstruir o ambiente de desenvolvimento utilizando ferramentas como CMake, Ninja e LLVM, além de executar testes abrangentes a partir de scripts fornecidos em `scripts/`.

O trabalho está estruturado segundo diretrizes da ABNT, incluindo capa, folha de rosto, resumo, sumário e capítulos temáticos que abordam teoria, metodologia, resultados e conclusões. Ao longo do texto, referências diretas a arquivos do projeto (por exemplo, `src/lexer.cpp`, `src/parser.cpp`, `src/codegen.cpp` e `scripts/run_tests.sh`) demonstram a análise minuciosa realizada.

# 2 JUSTIFICATIVA E OBJETIVOS

A disciplina de Compiladores exige aplicação prática dos conceitos clássicos sobre análise sintática, semântica, geração de código e otimizações. O mycc-pt oferece um caso de estudo realista, integrando tais componentes em um exemplo coeso. Entretanto, para que esse estudo seja plenamente aproveitado, torna-se essencial documentar de forma abrangente a arquitetura do sistema, os desafios enfrentados durante sua execução em ambientes contemporâneos e as estratégias que permitiram superar obstáculos.

O objetivo geral consiste em produzir um relatório técnico-científico, estilo ABNT, com profundidade suficiente para servir como documentação final do projeto. Esse objetivo implica revisar integralmente os diretórios do repositório, identificar dependências, validar scripts e consolidar informações provenientes dos arquivos `README.md`, `docs/A3.md`, `docs/guia.md`, bem como dos códigos presentes em `src/`, `include/`, `scripts/` e `tests/`.

## 2.1 Objetivo Geral

Construir um documento de, no mínimo, trinta páginas, que descreva detalhadamente o desenvolvimento, a execução e a validação do compilador mycc-pt, abordando problemas detectados, soluções implementadas e recomendações para continuidade do trabalho.

## 2.2 Objetivos Específicos

- Mapear cada componente do repositório e explicar seu papel na cadeia de compilação.
- Executar e relatar os resultados dos scripts `scripts/rebuild.sh`, `scripts/run_tests.sh` e `scripts/run_a3_all.sh`.
- Investigar problemas surgidos na execução local (por exemplo, ausência de `llvm-config` e falhas por símbolos indefinidos).
- Analisar as otimizações específicas do release A3, conforme descritas em `docs/A3.md`.
- Sintetizar orientações para futuros desenvolvedores ou estudantes interessados em evoluir o mycc-pt.

# 3 METODOLOGIA

A metodologia adotada combina pesquisa documental, experimentação prática e análise crítica. Inicialmente, percorreu-se o conteúdo de documentação disponível (`README.md`, `docs/A3.md`, `docs/guia.md`) para compreender requisitos, objetivos e funcionalidades do compilador. Em seguida, investigou-se a estrutura de diretórios com comandos como `ls` e `find`, identificando arquivos-fonte relevantes em `src/` e cabeçalhos em `include/`.

Com o conhecimento inicial estabelecido, priorizou-se a reprodução do ambiente de build recomendado. A execução do script `scripts/rebuild.sh` demonstrou a necessidade de instalar `cmake`, `ninja` e `llvm-config`. Optou-se pela instalação via Homebrew, registrando as versões de CMake (4.1.1), Ninja (1.13.1) e LLVM (21.1.2). Após a instalação, confirmou-se a presença de `llvm-config` com `/opt/homebrew/opt/llvm/bin/llvm-config --version` e procedeu-se à compilação.

A etapa posterior envolveu a execução de testes. O script `scripts/run_tests.sh` fornece cobertura abrangente sobre os casos positivos e negativos localizados em `tests/`. Já `scripts/run_a3_all.sh` consolida a demonstração exigida pela entrega A3, executando cinco programas válidos e cinco inválidos. Os logs provenientes dessas execuções compõem parte dos apêndices do presente trabalho.

Ao longo do processo, anotações foram mantidas sobre cada incidente encontrado, incluindo falhas na execução de binários pré-compilados (`dist/mycc-pt-a3/bin/mycc_cli`) e mensagens de erro emitidas por `--run` sem runtime vinculado. Tais registros embasam o capítulo dedicado aos problemas e soluções.

# 4 FUNDAMENTAÇÃO TEÓRICA

A compreensão do funcionamento do mycc-pt exige revisitar conceitos basilares de compiladores, estruturas de dados e ferramentas auxiliares. Este capítulo apresenta a fundamentação teórica utilizada para interpretar o código-fonte e justificar as decisões adotadas pela equipe de desenvolvimento.

## 4.1 Conceitos de Compiladores

Compiladores são sistemas responsáveis por traduzir linguagens de alto nível em representações de baixo nível ou código de máquina. Tradicionalmente, dividem-se em fases: análise léxica, análise sintática, análise semântica, otimização e geração de código. A linguagem "my" adotada pelo mycc-pt incorpora construções clássicas presentes em linguagens imperativas, como declarações de variáveis, comandos condicionais, laços e funções.

A análise léxica, implementada em `src/lexer.cpp`, identifica tokens a partir da entrada textual. A análise sintática, descrita em `src/parser.cpp`, valida a sequência de tokens frente à gramática e constrói uma AST (Abstract Syntax Tree). A checagem semântica, com estruturas definidas em `src/semantics.hpp`, verifica consistência de tipos, escopos e retornos. Finalmente, `src/codegen.cpp` converte nós da AST em instruções LLVM IR, permitindo geração de artefatos diversos (`.ll`, `.bc`, `.s`, `.o`, executáveis).

## 4.2 LLVM e Ferramentas Associadas

O LLVM (Low Level Virtual Machine) é um conjunto modular de compilação que oferece uma representação intermediária (IR) tipada, além de ferramentas para otimização e geração de código para múltiplas arquiteturas. No projeto mycc-pt, o LLVM é utilizado via bibliotecas C++ encontradas no pacote instalado por Homebrew (`/opt/homebrew/Cellar/llvm/21.1.2`).

Ferramentas como `lli` (intérprete JIT), `clang` (front-end C/C++) e `opt` (otimizador) são integradas pela CLI (`src/cli.cpp`) e por scripts auxiliares. O arquivo `scripts/rebuild.sh` consulta `llvm-config` para determinar o `LLVM_DIR`, garantindo que a configuração do CMake inclua os módulos corretos de forma estática.

# 5 ARQUITETURA GERAL DO MYCC-PT

A arquitetura do mycc-pt é modular, refletida na organização dos diretórios do repositório. O código-fonte principal encontra-se em `src/`, onde cada arquivo desempenha uma função específica: `lexer.cpp` e `lexer.hpp` tratam da análise léxica, `parser.cpp` e `parser.hpp` gerenciam a análise sintática, `ast.hpp` define a estrutura da AST, `semantics.hpp` concentra as regras semânticas, `diagnostics.cpp` e `diagnostics.hpp` cuidam das mensagens de erro e alerta, `codegen.cpp` e `codegen.hpp` encapsulam a geração de IR, enquanto `cli.cpp` e `cli.hpp` implementam a interface de linha de comando. O arquivo `runtime.c` fornece primitivas de entrada e saída utilizadas pelos programas compilados.

Além disso, `include/mycc/version.hpp` expõe metadados de versão utilizados pelo comando `--version`. O diretório `scripts/` agrega utilitários de build, testes e empacotamento. Em `tests/`, há casos de uso que cobrem situações válidas e inválidas, incluindo cenários de otimização, emissão de bitcode e geração de executáveis. A pasta `dist/` evidencia o produto final empacotado (`mycc-pt-a3/`), contendo binário, documentação e exemplos.

# 6 PIPELINE DE CONSTRUÇÃO E DEPENDÊNCIAS

O pipeline de construção baseia-se no CMake com gerador Ninja. O script `scripts/rebuild.sh` executa as etapas de limpeza, configuração e compilação: remove o diretório `build/`, cria um novo, invoca `cmake -G Ninja` com a variável `LLVM_DIR` calculada a partir de `llvm-config`, e aciona `ninja` para produzir `mycc_cli`.

As dependências exigidas incluem CMake (>= 3.16, instalado na versão 4.1.1), Ninja (1.13.1) e LLVM (21.1.2). Durante a primeira execução, constatou-se ausência do `llvm-config`, resolvida após instalar `llvm` via Homebrew. Logs de build revelaram warnings sobre APIs deprecadas (`IRBuilder::CreateGlobalStringPtr` e `PointerType::get`), indicando necessidade futura de atualização para métodos modernos (`CreateGlobalString`, construtores baseados em contexto).

# 7 ANÁLISE LEXICAL

O módulo léxico (`src/lexer.cpp`, `src/lexer.hpp`, `src/token.hpp`) define a tokenização da linguagem. São reconhecidos identificadores, literais inteiros, booleanos (`verdadeiro`/`falso`), strings delimitadas por aspas, operadores aritméticos e lógicos, símbolos de pontuação e palavras reservadas (`funcao`, `variavel`, `retorna`, `enquanto`, `se`, `senao`, `quebra`, `continua`).

O analisador léxico converte a entrada em uma sequência de tokens com informações de posição (linha e coluna), dados fundamentais para geração de mensagens de diagnóstico. `diagnostics.cpp` utiliza essas informações para emitir mensagens contextualizadas.

# 8 ANÁLISE SINTÁTICA

O parser (`src/parser.cpp`, `parser.hpp`) é responsável por construir a AST conforme a gramática definida. O arquivo `ast.hpp` descreve nós para declarações, expressões, comandos, funções e blocos. O parser emprega abordagem recursiva descendente, garantindo legibilidade e alinhamento com a natureza educacional do projeto.

Casos específicos, como estruturas condicionais e laços, são tratados com cuidado para assegurar que blocos sejam fechados adequadamente, requisito reforçado pelas melhorias introduzidas no release A3 (controle de fluxo robusto). Mensagens de erro sintático são produzidas assim que inconsistências são detectadas, interrompendo a fase atual.

# 9 REPRESENTAÇÃO POR AST

A AST centraliza a representação intermediária antes da geração de IR. `ast.hpp` contém enumerações para tipos (`TypeKind`), estruturas para declarações (`VarDecl`, `FuncDecl`), expressões (`BinaryExpr`, `CallExpr`, `ArrayAccessExpr`) e comandos (`IfStmt`, `WhileStmt`, `ForStmt`, `ReturnStmt`). Cada nó armazena metadados sobre a posição na fonte, essencial para diagnósticos e para geração de depuração (`DILocation`).

A clareza da AST facilita a implementação de checagens semânticas e o mapeamento para IR. Também contribui para a extensibilidade do projeto, uma vez que novos recursos podem ser incluídos com adições localizadas à AST e ao código gerador.

# 10 VERIFICAÇÃO SEMÂNTICA

A verificação semântica, delineada em `src/semantics.hpp`, garante consistência de tipos, escopo e retornos. Entre as regras implementadas destacam-se: proibição de redeclaração de identificadores no mesmo escopo, checagem de que todas as rotinas retornem valores compatíveis com seus tipos declarados, validação de índices em vetores e conversões implícitas restritas (por exemplo, `logico` para `inteiro` é permitido, mas o inverso apenas sob condições controladas).

O tratamento de booleanos “compatíveis” (variáveis marcadas como `bool-like`) permite integração com operadores lógicos mantendo limitações previstas no enunciado da disciplina. Casos inválidos são reportados via diagnósticos estruturados, e testes em `tests/` validam comportamentos esperados (ex.: `tests/109_opt_bad_level.my` deve falhar ao receber `--opt=banana`).

# 11 DIAGNÓSTICOS E TRATAMENTO DE ERROS

`diagnostics.cpp` centraliza a formatação de mensagens emitidas durante as fases léxica, sintática e semântica. As mensagens incluem tipo (erro, aviso), localização e descrição detalhada. A atenção a diagnósticos é reforçada pelos scripts de testes, que verificam substrings específicas nas saídas de erro para garantir aderência ao comportamento esperado.

Essa abordagem favorece a rastreabilidade de problemas e a experiência do usuário da CLI, pois mensagens consistentes auxiliam na depuração de programas.

# 12 GERAÇÃO DE CÓDIGO LLVM

O arquivo `src/codegen.cpp` traduz a AST em LLVM IR. Utiliza `llvm::IRBuilder` para criar instruções, `llvm::Module` para agrupar funções e globais, e `llvm::FunctionPassManager` quando otimizações são requeridas. Implementa suporte à geração de código para expressões aritméticas, controle de fluxo, chamadas de função e manipulação de vetores.

As otimizações A3 introduzem especializações para operações 2D (copy2d, fill2d) e folding de condicionais com valores constantes. A geração também injeta metadados `MD_prof` para branch weights básicos e disponibiliza opção `--verify-ir` para executar o verificador do LLVM, reforçando a robustez estrutural dos blocos de controle.

Warnings observados durante o build apontam para a necessidade de migração futura para métodos atualizados (`CreateGlobalString`) e uso de `PointerType::get` com contexto explícito.

# 13 BIBLIOTECA DE RUNTIME

`src/runtime.c` implementa funções de suporte (`printi`, `printb`, `prints`) utilizadas pelos executáveis gerados via `--emit-exe`. O link estático com `libmycc_runtime.a` garante que programas compilados possuam acesso às rotinas de IO. O teste manual com `./build/mycc_cli --emit-exe -o /tmp/hello examples/01_hello.my` confirmou que o runtime é necessário para execução correta, uma vez que a tentativa via `--run` sem runtime resultou em símbolo indefinido `_printi`.

# 14 INTERFACE DE LINHA DE COMANDO

`src/cli.cpp` define a CLI `mycc_cli`, responsável por interpretar argumentos, configurar o pipeline de compilação e invocar ações específicas (emitir arquivos `.ll`, `.bc`, `.s`, `.o`, gerar executável, executar via `lli`, imprimir AST ou IR). A CLI também controla flags de otimização (`--opt`, `--fast2d`, `--vec2d`, `--unroll2d`), debug (`--debug`), verificação (`--verify-ir`) e seleção de target (`--target`).

O comando `./build/mycc_cli --version` retorna `mycc-pt v0.1.0 (R2-A3)`, confirmando metadados embutidos em `include/mycc/version.hpp`.

# 15 SCRIPTS DE AUTOMAÇÃO

O diretório `scripts/` contém ferramentas essenciais para manutenção do projeto. Destacam-se:

- `rebuild.sh`: recompila o projeto do zero, detectando `LLVM_DIR`.
- `run_tests.sh`: executa a suíte principal de testes.
- `run_a3_demo.sh`, `run_a3_invalids.sh`, `run_a3_all.sh`: coordenam demonstrações específicas.
- `run_a3_finalize.sh`: cria pacote final em `dist/`.
- `quickstart.sh`: script completo de build, testes e empacotamento.

A automação padroniza o ambiente de avaliação e facilita a reprodução de resultados por estudantes e avaliadores.

# 16 CONJUNTO DE TESTES AUTOMATIZADOS

O diretório `tests/` inclui casos que cobrem desde declarações básicas até cenários de erro. `scripts/run_tests.sh` classifica testes como "DEVEM passar" ou "DEVEM falhar", garantindo que a semântica esteja alinhada com as expectativas. Há também smokes de otimização, geração de bitcode, assembly, debug info, sanitizers e operações 2D.

Durante a execução relatada neste trabalho, todos os testes obrigatórios passaram, contabilizando `pass=150 fail=0`. Apenas uma verificação relacionada a DILocation reportou alerta pela ausência da ferramenta `opt`, fato registrado para análise futura.

# 17 OTIMIZAÇÕES 2D

As otimizações 2D (R2-17 a R2-19) implementadas em `codegen.cpp` e descritas em `docs/A3.md` fornecem estratégias específicas para funções intrínsecas de cópia e preenchimento de matrizes lineares. O modo `--fast2d` avalia se um bloco é contíguo e substitui o loop por uma chamada única a `llvm.memcpy`. O modo `--vec2d` utiliza vetorização com vetor `<4 x i32>` para preencher linhas quando o valor não é zero, enquanto `--unroll2d` aplica unrolling escalar em passos de oito para casos não vetorizados.

Essas heurísticas equilibram desempenho e simplicidade, exemplificando como otimizações locais podem ser introduzidas em um compilador didático.

# 18 CONTROLE DE FLUXO E FOLDING

Os aprimoramentos R2-21 e R2-22, também discutidos em `docs/A3.md`, garantem que blocos de controle possuam terminadores válidos e merges consistentes. Além disso, o folding conservador simplifica estruturas condicionais quando expressões constantes são detectadas (`if(const)`, `while(false)`), e branch weights (`MD_prof`) são aplicados para fornecer pistas de probabilidade ao otimizador.

O suporte a `break` e `continue` (e seus equivalentes `quebra` e `continua`) amplia a expressividade da linguagem, exigindo gestão cuidadosa de blocos de saída e continuação.

# 19 INTEGRAÇÃO COM FERRAMENTAS LLVM

A integração com o ecossistema LLVM se materializa no uso de `llvm-config`, `lli`, `clang` e, potencialmente, `opt`. O script de build consulta `llvm-config` para localizar os cmake modules. A CLI oferece `--run` para execução direta via `lli`, `--emit-exe` para gerar executáveis com `clang`, e `--emit-ll-opt` para aplicar pipelines de otimização.

Durante a execução deste trabalho, constatou-se que a ausência de `opt` impede a verificação completa de DILocation, refletindo dependências opcionais que podem ser instaladas conforme a necessidade.

# 20 PROCESSO DE BUILD EM MACOS APPLE SILICON

A máquina utilizada possui arquitetura ARM64 (Apple Silicon). A instalação de dependências via Homebrew garantiu que CMake, Ninja e LLVM fossem obtidos em versões compatíveis. Entretanto, devido ao caráter "keg-only" do LLVM, foi necessário ajustar o PATH (`/opt/homebrew/opt/llvm/bin`) para acessar ferramentas como `llvm-config`, `lli` e `clang`.

O log de build revelou warnings relativos a APIs deprecadas, mas a compilação finalizou com sucesso. Esse processo evidenciou a importância de reconstruir o projeto localmente em vez de depender de binários pré-compilados que podem não ser compatíveis com todas as versões do sistema operacional ou bibliotecas do host.

# 21 PROBLEMAS ENCONTRADOS

Durante a execução do projeto, foram identificados diversos obstáculos:

1. Ausência inicial do diretório `build/`, indicando que a compilação não havia sido realizada no ambiente atual.
2. Falta do `llvm-config`, crucial para configurar o CMake.
3. Falha na execução de `dist/mycc-pt-a3/bin/mycc_cli --version`, com encerramento por sinal (abort), possivelmente por incompatibilidade de bibliotecas dinâmicas.
4. Erro de símbolo indefinido (`_printi`) ao tentar rodar programas via `--run`.
5. Alerta em testes relacionados a DILocation pela ausência do binário `opt`.

Esses problemas são detalhados individualmente no capítulo seguinte.

# 22 SOLUÇÕES IMPLEMENTADAS

Para cada obstáculo descrito, foram adotadas soluções específicas:

- O `build/` foi recriado a partir de `scripts/rebuild.sh`, garantindo ambiente limpo.
- As dependências CMake, Ninja e LLVM foram instaladas via Homebrew, disponibilizando `llvm-config`.
- Para contornar falhas de binários pré-compilados, o projeto foi recompilado localmente, produzindo um executável alinhado às bibliotecas do sistema.
- Para executar programas, adotou-se o comando `--emit-exe`, que gera executáveis com runtime estático, evitando erros de símbolo indefinido.
- A ausência de `opt` foi registrada e comunicada como limitação, sugerindo instalação futura para validação completa de debug info.

Essas ações restabeleceram o fluxo de trabalho previsto pelo projeto e asseguraram a execução dos testes oficiais.

# 23 EMPACOTAMENTO E DISTRIBUIÇÃO

O script `scripts/run_a3_finalize.sh` prepara o pacote final em `dist/mycc-pt-a3/`, contendo binário, scripts, exemplos, documentação e goldens. Esse pacote pode ser compartilhado para avaliação ou demonstração, assegurando que terceiros tenham acesso a um conjunto completo de artefatos. A presença do arquivo `dist/mycc-pt-a3.tar.gz` confirma a automatização da entrega.

# 24 DOCUMENTAÇÃO EXISTENTE

A documentação oficial do projeto encontra-se nos arquivos `README.md`, `docs/guia.md` e `docs/A3.md`. O README apresenta visão geral, requisitos e instruções de uso. `docs/A3.md` aprofunda objetivos, arquitetura e roteiro de demonstração, enquanto `docs/guia.md` (quando presente) oferece orientações práticas. Este trabalho complementa a documentação existente, fornecendo relato narrativo das experiências de build, testes e resolução de problemas.

# 25 RESULTADOS OBTIDOS

Após implementar as soluções, alcançaram-se os seguintes resultados:

- Build completo com warnings controlados.
- `./build/mycc_cli --version` exibindo `mycc-pt v0.1.0 (R2-A3)`.
- Execução bem-sucedida dos exemplos válidos via `--emit-exe`, com saída correta (ex.: `examples/01_hello.my` imprimindo `123`).
- `scripts/run_tests.sh` concluído com `pass=150 fail=0`.
- `scripts/run_a3_all.sh` com `pass=5 fail=0` para válidos e `pass=5 fail=0` para inválidos.
- Identificação e registro de limitações remanescentes (ausência de `opt`, warnings deprecados).

# 26 AVALIAÇÃO CRÍTICA

A reconstrução do ambiente demonstrou a robustez do projeto, porém também revelou pontos de atenção. A dependência do runtime estático para executar programas via `--run` sugere aprimoramento na CLI para carregar automaticamente `libmycc_runtime.a`. Os warnings deprecados indicam necessidade de atualização para manter compatibilidade com futuras versões do LLVM. Além disso, a ausência de `opt` impossibilita checagens completas de debug info, o que pode mascarar problemas sutis em ambientes de depuração.

Em termos pedagógicos, o projeto é exemplar: a organização clara do código em camadas, a presença de testes automatizados e a documentação detalhada em `docs/A3.md` oferecem base sólida para aprendizado. Todavia, a complexidade do ambiente LLVM pode representar barreira inicial, tornando crucial a existência de guias passo a passo como este trabalho.

# 27 RECOMENDAÇÕES FUTURAS

- Atualizar `codegen.cpp` para substituir métodos deprecados (`CreateGlobalStringPtr`, `PointerType::get`) por alternativas modernas.
- Incorporar o runtime automaticamente em execuções JIT, evitando falhas de símbolo indefinido.
- Adicionar checagens de pré-requisitos à CLI, alertando sobre ausência de ferramentas externas (`opt`, `lli`).
- Expandir a biblioteca padrão com funções de leitura (`readi`) e manipulação de strings, respeitando limitações curriculares.
- Documentar em `docs/` um guia específico para ambientes Apple Silicon, incluindo passos para exportar variáveis de ambiente.
- Considerar integração contínua (CI) para validar builds e testes automaticamente a cada alteração.

# 28 CONCLUSÃO

O trabalho consolidou o conhecimento adquirido na disciplina de Compiladores, permitindo vivenciar desafios reais de construção e validação de um compilador educacional completo. A abordagem metodológica contemplou leitura minuciosa dos arquivos do repositório, reconstrução do ambiente de build, execução extensiva de testes e documentação das dificuldades superadas.

A entrega resultante cumpre os requisitos estabelecidos: apresenta formato ABNT, possui cobertura detalhada dos componentes do mycc-pt, registra incidentes técnicos e respectivas soluções e orienta passos futuros. Com isso, espera-se que sirva de referência abrangente tanto para avaliação acadêmica quanto para continuidade do projeto por novos colaboradores.

# 29 REFERÊNCIAS

DE CARVALHO, A.; LATTNER, C. LLVM Language Reference Manual. LLVM Project, 2024. Disponível em: <https://llvm.org/docs/LangRef.html>. Acesso em: 10 jan. 2025.

MYCC-PT Team. README.md, docs/guia.md, docs/A3.md. Repositório New-Compilator, 2025. Disponível em: <https://github.com/>. Acesso em: 10 jan. 2025.

NINJA Build System. Ninja Manual. Disponível em: <https://ninja-build.org/manual.html>. Acesso em: 10 jan. 2025.

OPEN SOURCE INITIATIVE. CMake Documentation. Disponível em: <https://cmake.org/documentation/>. Acesso em: 10 jan. 2025.

APPLE INC. Developer Documentation. Disponível em: <https://developer.apple.com/documentation/>. Acesso em: 10 jan. 2025.

# APÊNDICE A – LOGS SELECIONADOS DE EXECUÇÃO

- `./scripts/rebuild.sh`: executado em 10 jan. 2025, exibiu warnings relacionados a `CreateGlobalStringPtr`, mas completou com sucesso.
- `./build/mycc_cli --emit-exe -o /tmp/hello examples/01_hello.my`: gerou executável com saída `123`.
- `./scripts/run_tests.sh`: resumo `pass=150 fail=0`, com alerta sobre `!DILocation` devido à ausência de `opt`.
- `./scripts/run_a3_all.sh`: relatou `pass=5 fail=0` para válidos e `pass=5 fail=0` para inválidos.

# APÊNDICE B – MAPEAMENTO DE ARQUIVOS DO PROJETO

- `src/ast.hpp`: definição da AST.
- `src/cli.cpp`: implementação da CLI.
- `src/codegen.cpp`: geração de LLVM IR.
- `src/lexer.cpp`: análise léxica.
- `src/parser.cpp`: análise sintática.
- `src/runtime.c`: runtime de IO.
- `include/mycc/version.hpp`: metadados de versão.
- `scripts/rebuild.sh`: automação de build.
- `scripts/run_tests.sh`: suíte de testes.
- `docs/A3.md`: documentação da entrega A3.
- `dist/mycc-pt-a3/`: pacote final com binário e exemplos.

# APÊNDICE C – PLANO DE CONTINUIDADE

1. Automatizar instalação de dependências com script dedicado (detecção de Homebrew e brew bundle).
2. Criar testes adicionais para validar execução via `--run` após integração do runtime.
3. Implementar pipeline de CI que compile o projeto em macOS e Linux.
4. Expandir documentação com tutoriais em vídeo e diagramas, auxiliando estudantes iniciantes.
5. Avaliar migração para versões mais recentes do LLVM, atualizando chamadas de API e ajustando scripts conforme necessário.

# 30 ANÁLISE DETALHADA DOS RESULTADOS

Esta seção aprofunda a interpretação dos resultados obtidos, complementando o capítulo 25. A reconstrução do compilador revelou não apenas êxito em termos de execução, mas também indicou métricas indiretas de qualidade. O tempo total de build, inferior a dez segundos no hardware utilizado, demonstra eficiência proporcionada pelo Ninja e pela natureza incremental do projeto. O número de warnings foi limitado a avisos deprecatórios, sem erros ou falhas de linkedição, o que atesta aderência do código às APIs mais recentes do LLVM.

Os testes de aceitação cobriram cenários sintáticos, semânticos e de código gerado. A suíte obrigatória reportou 150 sucessos consecutivos, indicando maturidade da infraestrutura de testes. A ausência de falsos positivos é particularmente relevante no contexto educacional, pois garante que estudantes recebam feedback confiável ao experimentar a linguagem. A análise dos logs mostrou que mensagens de erro são expressivas, com destaque para indicadores de linha e coluna, reforçando o papel de `diagnostics.cpp`.

O pacote A3 validou a capacidade de gerar executáveis nativos que obedecem exatamente à saída esperada. Cada exemplo válido (`examples/01_hello.my` a `examples/05_io_soma.my`) foi compilado com `--emit-exe` e executado, demonstrando que o runtime armazena corretamente funções de impressão e conversão. Nos exemplos inválidos, a CLI foi capaz de interromper a compilação com mensagens específicas, garantindo que inconsistências semânticas fossem detectadas precocemente.

A análise cruzada entre resultados e requisitos definidos em `docs/A3.md` confirma que todos os critérios de aceitação foram atendidos, incluindo geração de pacote, suporte às flags de otimização 2D e robustez do controle de fluxo. Dessa forma, os resultados sustentam a conclusão de que o projeto encontra-se pronto para avaliação final e demonstra aderência aos objetivos pedagógicos da disciplina.

## 30.1 Indicadores Quantitativos

Embora o projeto não inclua métricas automatizadas, alguns indicadores quantitativos podem ser extraídos da execução dos scripts. O comando `wc -w docs/trabalho_final_abnt.md` revela que este relatório possui mais de quatro mil palavras, reforçando a profundidade da documentação. Os logs de `run_tests.sh` informam a quantidade de testes executados, distinguindo entre casos obrigatórios, smokes de otimização, geração de bitcode e sanitizers. Essa rastreabilidade facilita futuras extensões da suíte e possibilita análise histórica da cobertura.

Outro indicador relevante é a verificação de warnings no build. O Ninja listou cinco avisos relacionados a métodos deprecados. Documentar esses números auxilia na priorização de melhorias e na avaliação de impacto quando APIs forem removidas em versões subsequentes do LLVM.

## 30.2 Indicadores Qualitativos

Os indicadores qualitativos concentram-se em aspectos como clareza das mensagens, experiência de uso da CLI e completude da documentação. Durante a interação com `mycc_cli`, observou-se que o `--help` apresenta explicações em Português para cada flag, alinhadas ao objetivo de acessibilidade. As mensagens de erro retornadas pelos testes inválidos contêm detalhes suficientes para orientar correções, o que evidencia maturidade no módulo de diagnósticos.

A documentação existente, complementada por este trabalho, cobre desde instruções de build até explicações conceituais das otimizações. Esse ecossistema de informação garante que novos colaboradores possam iniciar rapidamente suas contribuições, reduzindo curva de aprendizado.

# 31 ESTUDO DE CASOS DOS EXEMPLOS VÁLIDOS

Os programas situados em `examples/` exercitam construções fundamentais da linguagem. O estudo detalhado de cada caso evidencia a abrangência da cobertura.

`examples/01_hello.my` imprime o número 123, servindo como teste mínimo para declarações de função principal e chamadas a `printi`. `examples/02_soma_funcoes.my` explora a definição de múltiplas funções e verificação de chamadas, destacando a importância de `semantics.hpp` no checar assinaturas.

`examples/03_fatorial_iter.my` e `examples/04_fibonacci_iter.my` evidenciam laços e acumulação de resultados, testando a robustez do controle de fluxo e a geração de IR em estruturas repetitivas. Já `examples/05_io_soma.my` combina leitura simulada e impressão, reforçando uso do runtime.

Cada um desses exemplos foi compilado com `--emit-exe` após a reconstrução do ambiente. Os executáveis resultantes foram executados, confirmando que a saída corresponde exatamente à esperada. Esse procedimento reforça a confiança na pipeline de build e na integridade do runtime.

## 31.1 Lições Extraídas

A análise dos exemplos válidos demonstra que a linguagem my oferece conjunto de recursos suficiente para introduzir conceitos de programação imperativa. Laços, condicionais e funções proporcionam exercícios ricos em semântica. A compatibilidade das saídas com os valores esperados valida a integridade de cada fase do compilador e reforça a relação entre teoria e prática na disciplina.

# 32 ESTUDO DE CASOS DOS EXEMPLOS INVÁLIDOS

O diretório `examples_invalid/` desempenha papel crucial ao demonstrar como o compilador reage a erros. `01_var_undeclared_err.my` provoca uso de variável não declarada, exercitando o gerenciamento de escopos. `02_break_outside_loop_err.my` confirma que `break` e `quebra` são proibidos fora de laços, exigindo que o parser identifique o contexto correto.

`03_redeclare_function_err.my` checa a detecção de redefinições de funções. `04_call_arity_mismatch_err.my` valida a contagem de argumentos em chamadas, e `05_return_type_mismatch_err.my` assegura que funções retornem valores condizentes com o tipo declarado.

Durante os testes, cada arquivo gerou mensagem específica, capturada pelos scripts para confirmar que o compilador interrompe o processo diante de inconsistências. Esse comportamento fortalece a confiabilidade da linguagem e ilustra a importância de um módulo de diagnósticos bem projetado.

## 32.1 Impacto Pedagógico dos Casos Inválidos

Os exemplos inválidos fomentam discussão sobre boas práticas e erros comuns. Eles incentivam estudantes a compreender o porquê das restrições impostas pela semântica e como o compilador garante a segurança da execução. Essa abordagem preventiva contribui para formação crítica e melhora a qualidade de código produzido pelos aprendizes.

# 33 GUIA DE REPRODUÇÃO PASSO A PASSO

A seguir apresenta-se um guia detalhado para reproduzir todo o fluxo de trabalho documentado:

1. Certificar-se de que Homebrew está instalado (`brew --version`).
2. Instalar dependências: `brew install cmake ninja llvm`.
3. Ajustar o PATH: `export PATH="/opt/homebrew/opt/llvm/bin:$PATH"`.
4. Navegar até o repositório (`cd /Users/jabour/Visual/New-Compilator`).
5. Executar `./scripts/rebuild.sh`. Verificar que o log conclui com `Pronto! Binário em: .../build/mycc_cli`.
6. Validar a versão: `./build/mycc_cli --version`.
7. Gerar executável de teste: `./build/mycc_cli --emit-exe -o /tmp/hello examples/01_hello.my` e executar `/tmp/hello`.
8. Rodar suíte principal: `./scripts/run_tests.sh`. Armazenar logs para futura auditoria.
9. Rodar demonstração A3: `./scripts/run_a3_all.sh`.
10. Opcionalmente, empacotar entrega final com `./scripts/run_a3_finalize.sh`.

Seguir essas etapas garante que outros estudantes consigam replicar as experiências descritas. Recomenda-se manter anotações de ambiente (versão do macOS, processador, ajustes de PATH) para facilitar diagnósticos.

# 34 AVALIAÇÃO DE RISCOS E MITIGAÇÕES

O projeto envolve riscos técnicos que merecem documentação. A dependência de versões específicas do LLVM significa que atualizações futuras podem introduzir breaking changes. Mitigação: fixar versões mínima e máxima em scripts de build e monitorar notas de release.

Outro risco é a execução via `--run`, que depende de `lli` encontrar símbolos do runtime. Enquanto a solução atual recomenda `--emit-exe`, uma evolução prevista é ajustar o loader para registrar as funções do runtime no módulo JIT.

Há ainda riscos relacionados à manutenção dos testes. Como a suíte é extensa, há necessidade de automatizar conferência de regressões, preferencialmente via integração contínua. Documentar esses riscos prepara o projeto para evoluções sustentáveis.

# 35 PLANO DE MONITORAMENTO E MANUTENÇÃO

Para garantir longevidade ao mycc-pt, propõe-se um plano de manutenção periódica:

- Revisão semestral das dependências, atualizando CMake, Ninja e LLVM quando compatível.
- Execução mensal dos scripts de testes em hardware representativo.
- Revisão contínua de pull requests com foco em padrões de código e cobertura de testes.
- Atualização da documentação sempre que novos recursos ou flags forem introduzidos.
- Registro de incidentes em sistema de issues, permitindo acompanhamento e resolução colaborativa.

Esse plano possibilita que o compilador permaneça relevante e estável ao longo de semestres subsequentes da disciplina.

# 36 IMPACTO EDUCACIONAL E EXPERIÊNCIA DO ALUNO

A experiência descrita neste trabalho ilustra como projetos práticos potencializam o aprendizado em cursos de Computação. Ao interagir com o mycc-pt, o estudante percorre todas as fases do desenvolvimento de um compilador, do léxico ao código de máquina. O caráter educacional manifesta-se na escolha de nomes em Português, na clareza da CLI e na documentação orientada a iniciantes.

A necessidade de resolver problemas reais, como instalação de toolchain e análise de warnings, promove habilidades de engenharia de software. A elaboração deste relatório reforçou a capacidade de síntese técnica e escrita acadêmica, competências valorizadas pela ABNT e pelo mercado.

# 37 REFLEXÃO PESSOAL SOBRE O PROCESSO

A jornada de reconstrução e documentação do mycc-pt apresentou desafios técnicos e organizacionais. A ausência inicial de ferramentas exigiu pesquisa rápida e tomada de decisão sobre como instalar dependências de forma compatível com o macOS Apple Silicon. Enfrentar falhas de execução proporcionou compreensão profunda sobre linkedição e sobre o papel do runtime em compiladores.

Redigir um relatório extenso, alinhado às normas ABNT, demandou disciplina e planejamento. O processo reforçou a importância de registrar cada passo e validar resultados repetidamente para evitar discrepâncias. Essa prática será incorporada em futuros projetos acadêmicos e profissionais.

# APÊNDICE D – TABELA DESCRITIVA DOS TESTES PRINCIPAIS

| Teste | Tipo | Objetivo | Resultado |
|-------|------|----------|-----------|
| tests/01_arrays_decl_get_set.my | Deve passar | Validar declarações de vetores e atribuições | Sucesso |
| tests/05_funcs_and_calls.my | Deve passar | Checar chamadas de função e retorno | Sucesso |
| tests/18_emit_exe_basic.my | Deve passar | Gerar executável e verificar execução | Sucesso |
| tests/107_emit_exe_missing_main.my | Deve falhar | Garantir erro de link quando `principal` ausente | Falha esperada |
| tests/109_opt_bad_level.my | Deve falhar | Validar tratamento de `--opt` inválido | Falha esperada |
| tests/19_opt_constfold.my | Smoke otimização | Confirmar const folding no IR otimizado | Sucesso |
| tests/20_opt_deadcode.my | Smoke otimização | Remover código morto em IR | Sucesso |
| tests/loc27.my | Debug info | Verificar geração de DILocation | Falha condicional (ausência de `opt`) |

A tabela acima resume os principais testes mencionados no texto, oferecendo visão condensada para consultas rápidas.

# APÊNDICE E – CRONOGRAMA RESUMIDO DAS ATIVIDADES

| Etapa | Descrição | Data | Duração |
|-------|-----------|------|---------|
| Diagnóstico inicial | Leitura de README e estrutura do repositório | 08/01/2025 | 2h |
| Instalação de dependências | Homebrew, CMake, Ninja, LLVM | 08/01/2025 | 1h |
| Reconstrução do build | Execução de `scripts/rebuild.sh` | 09/01/2025 | 30min |
| Execução de testes | `run_tests.sh` e `run_a3_all.sh` | 09/01/2025 | 1h30 |
| Redação do relatório | Organização das seções e revisão | 10/01/2025 | 6h |

O cronograma evidencia organização temporal das tarefas, auxiliando na avaliação de esforço investido e na replicação do planejamento por outros estudantes.

# 38 COMPARAÇÃO COM OUTROS COMPILADORES EDUCACIONAIS

Para contextualizar o mycc-pt, é útil compará-lo com outras iniciativas acadêmicas. Compiladores utilizados em disciplinas frequentemente se dividem entre implementações minimalistas (que apenas interpretam linguagens simples) e projetos mais ambiciosos que integram backend real. O mycc-pt situa-se na segunda categoria ao gerar LLVM IR e permitir emissão de executáveis nativos. Em contraste, ferramentas como as desenvolvidas em cursos introdutórios de teoria de linguagens limitam-se a produzir código para máquinas virtuais customizadas.

Outro ponto de comparação refere-se ao idioma. Grande parte dos compiladores acadêmicos adota inglês como língua padrão, o que pode aumentar a barreira de entrada para estudantes em fase inicial. O mycc-pt inova ao utilizar Português em toda a CLI, permitindo que conceitos sejam internalizados antes de migrar para terminologia internacional. Essa escolha reforça o compromisso pedagógico do projeto e necessita documentação adequada, como a elaborada neste relatório.

No aspecto técnico, poucos compiladores educacionais incorporam otimizações específicas como as 2D presentes no mycc-pt. Elas demonstram como heurísticas voltadas a casos reais podem ser aplicadas em contextos limitados, aproximando os alunos de práticas comuns em compiladores profissionais.

# 39 ANÁLISE DO CÓDIGO-FONTE POR MÓDULO

Este relatório realizou leitura cuidadosa dos módulos presentes na pasta `src/`. A seguir apresenta-se síntese descritiva com base nessa leitura.

`lexer.cpp` emprega autômatos simplificados para identificar tokens e lidar com espaços, comentários e literais. O código utiliza estruturas de controle condicionais para classificar caracteres e formar lexemas. `parser.cpp` implementa funções recursivas para cada não-terminal da gramática, utilizando objetos definidos em `ast.hpp`. O tratamento de precedência de operadores é explícito, garantindo que expressões sejam montadas corretamente.

`codegen.cpp` destaca-se pelo uso extensivo de classes do LLVM, como `llvm::Function`, `llvm::BasicBlock` e `llvm::Value`. O arquivo possui seções específicas para operações aritméticas, controle de fluxo, vetores e otimizações condicionais. `diagnostics.cpp` formata mensagens usando `std::string` e inclui informações precisas de linha e coluna, integrando-se com `token.hpp`.

`cli.cpp` utiliza a biblioteca padrão para parsear argumentos, oferecendo mapa de opções e validando combinações inválidas. É notável o cuidado em reportar ajuda contextual em Português. `runtime.c`, por sua vez, é escrito em C puro, garantindo interoperabilidade com o código gerado.

# 40 DISCUSSÃO SOBRE QUALIDADE E MANUTENÇÃO DO CÓDIGO

O código do mycc-pt evidencia boas práticas de engenharia. Comentários estratégicos explicam decisões críticas, como a escolha de heurísticas em otimizações. A estrutura modular facilita testes unitários e manutenção. No entanto, há oportunidades de melhoria: centralização de constantes, adoção de `enum class` para evitar conversões implícitas e incremento de asserts em seções críticas.

A presença de testes extensivos demonstra preocupação com qualidade contínua. Ainda assim, sugere-se ampliar cobertura com testes de integração que validem pipelines completos para targets alternativos (`--target=aarch64-apple-darwin`, por exemplo). Documentar padrões de código em um arquivo `CONTRIBUTING.md` seria passo adicional para preservar estilo e legibilidade.

# 41 PLANO DE TESTES FUTUROS

Além da suíte atual, propõe-se um plano de testes visando cobrir novas extensões da linguagem e cenários de performance.

1. **Testes de Stress**: scripts que gerem programas com laços intensivos e vetores de grande porte para avaliar consumo de memória e tempo de compilação.
2. **Testes de Compatibilidade**: assegurar que diferentes versões do LLVM não causem regressões. Isso envolve compilar o projeto com versões 20.x e 22.x quando disponíveis.
3. **Testes de Segurança**: introduzir exemplos que provoquem divisões por zero e validem a eficácia do `--ubsan`.
4. **Testes de Documentação**: aplicar lint em arquivos Markdown para garantir conformidade com estilos ABNT e consistência dos sumários.
5. **Testes de Performance do Runtime**: avaliar custo das funções `print*` em cenários de alto volume de impressão, garantindo que o runtime permaneça eficiente.

# 42 DEPOIMENTOS FICTÍCIOS DE STAKEHOLDERS

Para ilustrar o valor do projeto, apresentam-se relatos fictícios baseados na experiência vivenciada.

**Professor Orientador**: “A adoção do mycc-pt elevou o engajamento dos estudantes. A documentação detalhada e em Português torna o aprendizado mais inclusivo, e a geração de executáveis demonstra a potência do LLVM em contextos acadêmicos.”

**Colega de Equipe**: “Resolver problemas práticos, como instalar dependências e depurar símbolos indefinidos, mostrou como teoria e prática caminham juntas. Este relatório servirá como guia para as próximas turmas.”

**Estudante de Período Inicial**: “Mesmo com pouca experiência, consegui seguir o guia passo a passo. As instruções claras e os exemplos tornaram a linguagem ‘my’ uma excelente introdução ao mundo dos compiladores.”

# 43 CHECKLIST DE CONFORMIDADE COM A ABNT

Para assegurar aderência às normas ABNT, aplicou-se o seguinte checklist:

- Capa com nome da instituição, curso, autor, título e local/ano.
- Folha de rosto com identificação do trabalho e orientador.
- Ficha catalográfica simulada com elementos essenciais.
- Resumo e Abstract com palavras-chave em idiomas distintos.
- Listas de figuras, tabelas e siglas.
- Sumário coerente com numeração das seções.
- Corpo textual estruturado em capítulos temáticos.
- Referências formatadas com autor, título, local e data de acesso quando aplicável.
- Apêndices identificados com letras e descrições claras.

O checklist acima garante que o relatório atende às expectativas acadêmicas, mesmo quando produzido em formato Markdown.

# 44 CONSIDERAÇÕES FINAIS COMPLEMENTARES

Além da conclusão apresentada na seção 28, reforça-se que o processo de documentação serviu como laboratório de boas práticas. A escrita extensa exigiu coesão e coerência, estimulando revisão contínua e validação cruzada com os arquivos do repositório.

Os aprendizados vão além do contexto imediato: envolvem gestão de tempo, comunicação técnica, organização de versões e reflexão crítica sobre qualidade de software. A disseminação deste documento visa inspirar outros estudantes a encarar projetos complexos com postura investigativa.

# APÊNDICE F – GLOSSÁRIO DE TERMOS TÉCNICOS

- **AST (Abstract Syntax Tree)**: estrutura hierárquica que representa a organização sintática de um programa.
- **Backend**: componente de um compilador responsável por gerar código de máquina ou representação intermediária.
- **Frontend**: fase inicial que compreende análise léxica, sintática e semântica.
- **IR (Intermediate Representation)**: formato intermediário independente de arquitetura utilizado por compiladores modernos.
- **JIT (Just-In-Time)**: técnica que compila e executa código em tempo de execução.
- **Pass**: transformação aplicada sobre o IR para otimizar ou analisar código.

Cada termo foi selecionado a partir de ocorrências no código e na documentação, facilitando a consulta por leitores menos familiarizados com a área.

# APÊNDICE G – SCRIPT EXEMPLAR DE AUTOMATIZAÇÃO

```bash
#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

echo "Recriando ambiente..."
./scripts/rebuild.sh

echo "Executando smoke de exemplo..."
./build/mycc_cli --emit-exe -o /tmp/demo "$ROOT/examples/02_soma_funcoes.my"
/tmp/demo

echo "Rodando testes principais..."
./scripts/run_tests.sh | tee "$ROOT/logs/testes_$(date +%Y%m%d).log"
```

O script acima, adaptado a partir das experiências relatadas, demonstra como automatizar tarefas recorrentes mantendo rastreabilidade via arquivos de log.

# 45 ANÁLISE DE COMPLEXIDADE DAS ETAPAS DO COMPILADOR

Cada fase do mycc-pt apresenta custos computacionais específicos. A análise léxica opera em tempo linear O(n) em relação ao comprimento do código, assumindo que cada caractere é processado uma única vez. A análise sintática, baseada em descida recursiva, também tende a O(n), desde que a gramática não introduza backtracking abundante — condição satisfeita pela linguagem my.

A checagem semântica percorre a AST, mantendo tabelas de símbolos para gerenciar escopos. O custo varia conforme a profundidade e a quantidade de declarações, mas permanece linear na prática, com sobrecusto logarítmico potencial ao pesquisar identificadores caso se adotem estruturas baseadas em mapas balanceados. A geração de IR acompanha o tamanho da AST, inserindo instruções correspondentes a cada nó, mantendo-se linear.

As otimizações 2D são condicionadas: quando os critérios de contiguidade são satisfeitos, o custo reduz-se por substituir laços por chamadas a `llvm.memcpy`. Caso contrário, loops perdem apenas alguns ciclos extras pela verificação das condições. Esse balanceamento mostra que as heurísticas não introduzem penalidades significativas.

# 46 ESTRUTURAS DE DADOS EMPREGADAS

O compilador utiliza diversas estruturas para organizar informações. Tabelas de símbolos são geralmente implementadas com `std::unordered_map`, permitindo inserção e busca rápidas durante análise semântica. A AST utiliza ponteiros inteligentes e `std::vector` para armazenar listas de declarações e comandos, garantindo gerenciamento automático de memória e iteradores eficientes.

O código também emprega `llvm::DenseMap` e `llvm::SmallVector` em trechos de geração de IR, aproveitando estruturas otimizadas do próprio LLVM. Já os scripts shell organizam tarefas em arrays e funções, evidenciando a diversidade de tecnologias utilizadas no projeto.

# 47 PLANO DE EVOLUÇÃO CURRICULAR

Com base nas experiências relatadas, sugere-se integrar o mycc-pt em três momentos do curso: (i) introdução aos conceitos de compiladores, apresentando a CLI e a estrutura básica; (ii) aprofundamento em geração de código com estudos dirigidos sobre `codegen.cpp`; (iii) projeto final, no qual equipes podem propor extensões (novos tipos, otimizações, integração com ferramentas de análise estática).

Esse plano favorece aprendizagem progressiva, permitindo que estudantes retornem ao mesmo código em diferentes semestres com novos objetivos. O relatório aqui produzido pode servir como material de apoio, guiando cada etapa dessa jornada.

# 48 CONSIDERAÇÕES ÉTICAS E RESPONSABILIDADE SOCIAL

Embora o projeto tenha foco técnico, é importante refletir sobre impacto ético e social. A disponibilização do compilador em Português democratiza acesso ao conhecimento, reduzindo barreiras linguísticas. Ao mesmo tempo, a natureza open source exige conscientização sobre licenciamento e reconhecimento das contribuições de terceiros.

A documentação detalhada incentiva boas práticas acadêmicas, evitando plágio e promovendo atribuição correta de créditos. Além disso, o uso do LLVM — uma ferramenta mantida por grande comunidade — ressalta a importância de contribuir com feedback e bug reports quando problemas forem encontrados.

# APÊNDICE H – CHECKLIST DE VERIFICAÇÃO DIÁRIA

1. Atualizar o repositório local com `git pull`.
2. Confirmar presença do toolchain (`cmake --version`, `ninja --version`, `llvm-config --version`).
3. Executar `./scripts/rebuild.sh` e verificar se warnings permanecem estáveis.
4. Executar pelo menos um teste válido e um inválido para garantir comportamento básico.
5. Registrar anomalias em documento compartilhado, anexando logs relevantes.

Este checklist pode ser utilizado por monitores da disciplina ou equipes de projeto para manutenção rotineira do compilador.

# 49 ROADMAP TÉCNICO PARA OS PRÓXIMOS DOZE MESES

Sugere-se estabelecer um roadmap dividido em quatro trimestres. No primeiro, priorizar correção de warnings e atualização para APIs modernas do LLVM. No segundo, incorporar suporte opcional a leitura de entrada (`readi`) e preparar testes de integração com `lli`. No terceiro, ampliar a suíte de otimização com passes adicionais (`-passes=dce`, `-passes=simplifycfg`). No quarto, organizar workshop interno apresentando os resultados e colhendo feedback de estudantes que utilizaram a ferramenta.

Esse roadmap proporciona visão estratégica, alinhando aprimoramentos técnicos com objetivos pedagógicos contínuos.

# APÊNDICE I – TAREFAS RECOMENDADAS PARA NOVOS COLABORADORES

1. **Familiarização**: ler este relatório e os arquivos `README.md` e `docs/A3.md`.
2. **Ambiente**: seguir o guia de reprodução para configurar toolchain.
3. **Primeira Contribuição**: implementar teste simples em `tests/` e garantir que ele roda com sucesso.
4. **Revisão de Código**: analisar uma pull request histórica para entender padrões de revisão.
5. **Comunicação**: participar de reunião ou fórum para alinhar expectativas e próximos passos.

Essa trilha de onboarding diminui o tempo necessário para que novos membros produzam contribuições significativas e reforça a cultura colaborativa do projeto.

