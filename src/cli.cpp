#include "cli.hpp"
#include "diagnostics.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "ast.hpp"
#include "semantics.hpp"
#include "codegen.hpp"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <fstream>
#include <sstream>
#include <iostream>
#include <string>
#include <cstdlib>
#include <cstdio>
#include <llvm/Target/TargetMachine.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/TargetParser/Triple.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/LegacyPassManager.h>
// Debug info support
#include <llvm/IR/DIBuilder.h>
// Extra headers for optimization utilities (Patch 11)
#include <llvm/Transforms/Scalar/LoopPassManager.h>
#include <llvm/Transforms/IPO/InferFunctionAttrs.h>
// Patch 14: Sanitizers
#include <llvm/Transforms/Instrumentation/AddressSanitizer.h>

namespace mycc {
namespace cli {

// Patch 11: global flag to control printing of optimization pipeline
static bool gPrintPipeline = false;
// Patch 12: global flag to enable debug info generation
static bool debugEnabled = false;

static std::string readFile(const std::string& path) {
    std::ifstream ifs(path);
    if (!ifs) return {};
    std::stringstream ss; ss << ifs.rdbuf();
    return ss.str();
}

int run(int argc, char** argv) {
    if (argc < 2) {
        std::cout << "Uso: mycc_cli [--help|--parse-only|--dump-ast|--dump-ir|--check|--emit-ll[=<arq>]|--emit-obj[=<arq>]|--emit-exe[=<arq>]|--run] <arquivo.my>\n";
        return 1;
    }

    auto isFlag = [](const std::string& s) {
        return s.rfind("--", 0) == 0 || s == "-o";
    };

    std::string mode = argv[1];
    bool emitLL = false;
    std::string outPath;
    std::string file;
    bool emitOBJ = false;
    std::string outObjPath;
    bool runMode = false;
    bool emitEXE = false;
    std::string outExePath;
    std::string rtPath; // --rt=<arquivo>
    bool emitASM = false;
    std::string outAsmPath;
    bool emitBC = false;
    std::string outBCPath;
    bool emitLLOpt = false;
    std::string outOptLLPath;
    std::string optLevel = "O0";      // padrão
    bool optProvided = false;          // se o usuário passou --opt
    std::string optPipeline;           // pipeline textual custom
    std::string targetTripleArg;       // --target=<triple>
    bool useASan = false;              // --asan
    bool useUBSan = false;             // --ubsan
    std::string fast2dArg = "auto";    // --fast2d=<off|auto|always>

    // Pre-scan: capture opções de otimização em qualquer posição
    for (int i = 1; i < argc; ++i) {
        std::string a = argv[i];
        if (a.rfind("--opt=", 0) == 0) {
            optLevel = a.substr(std::string("--opt=").size());
            optProvided = true;
        } else if (a == "--opt") {
            optProvided = true; // mantém O0
        } else if (a == "-O0" || a == "-O1" || a == "-O2" || a == "-O3" || a == "-Os" || a == "-Oz") {
            // níveis de otimização no estilo clang
            optLevel = a.substr(1); // remove '-' e mantém "O*"
            optProvided = true;
        } else if (a.rfind("--opt-pipeline=", 0) == 0) {
            optPipeline = a.substr(std::string("--opt-pipeline=").size());
            if (optPipeline.empty()) {
                std::cerr << "error: --opt-pipeline requer um texto de pipeline\n";
                return 1;
            }
        } else if (a == "--opt-pipeline") {
            std::cerr << "error: falta valor para --opt-pipeline (use --opt-pipeline=<texto>)\n";
            return 1;
        } else if (a.rfind("--rt=", 0) == 0) {
            rtPath = a.substr(5);
        } else if (a == "--print-pipeline") {
            gPrintPipeline = true;
        } else if (a == "-g" || a == "--debug") {
            debugEnabled = true;
        } else if (a == "--asan") {
            useASan = true;
        } else if (a == "--ubsan") {
            useUBSan = true;
        } else if (a.rfind("--fast2d=", 0) == 0) {
            fast2dArg = a.substr(std::string("--fast2d=").size());
        }

    }

    if (mode == "--help") {
        std::cout << "mycc-pt - Compilador Educacional\n";
        std::cout << "Flags disponíveis:\n";
        std::cout << "  --help               Exibe esta mensagem\n";
        std::cout << "  --parse-only         Executa apenas análise léxica/sintática\n";
        std::cout << "  --dump-ast           Mostra a AST gerada\n";
        std::cout << "  --dump-ir            Mostra o IR LLVM (roda semântica e verifica IR)\n";
        std::cout << "  --check              Verifica semântica (tabela de símbolos e tipos)\n";
        std::cout << "  --emit-ll            Gera o IR LLVM em .ll (sem -o: salva em <input>.ll)\n";
        std::cout << "  --emit-ll=<arq>      Gera o IR LLVM no arquivo informado\n";
        std::cout << "  --emit-llvm          Alias de --emit-ll\n";
        std::cout << "  --emit-llvm=<arq>    Alias de --emit-ll=<arq>\n";
        std::cout << "  --emit-obj           Gera objeto nativo (.o) (sem -o: salva em <input>.o)\n";
        std::cout << "  --emit-obj=<arq>     Gera objeto nativo no arquivo informado\n";
        std::cout << "  --emit-exe           Gera executavel nativo (sem -o: salva em <input> sem extensao)\n";
        std::cout << "  --emit-exe=<arq>     Gera executavel no arquivo informado\n";
        std::cout << "  --rt=<arq>           Caminho do runtime (libmycc_runtime.a). Default embutido.\n";
        std::cout << "  --emit-asm           Gera assembly (.s) (sem -o: salva em <input>.s)\n";
        std::cout << "  --emit-asm=<arq>     Gera assembly textual no arquivo informado\n";
        std::cout << "  --emit-bc            Gera bitcode LLVM (.bc) (sem -o: <input>.bc)\n";
        std::cout << "  --emit-bc=<arq>      Gera bitcode no arquivo informado\n";
        std::cout << "  --emit-ll-opt        Salva IR otimizado em <input>.opt.ll (ou -o <arq>)\n";
        std::cout << "  --emit-ll-opt=<arq>  Salva IR otimizado no arquivo informado\n";
        std::cout << "  --opt[=O0|O1|O2|O3|Os|Oz]   Define o nivel de otimizacao (padrao: O0)\n";
        std::cout << "  -O0|-O1|-O2|-O3|-Os|-Oz     Niveis de otimizacao no estilo clang\n";
        std::cout << "  --opt-pipeline=<texto>      Pipeline textual personalizado (PassBuilder)\n";
        std::cout << "  --print-pipeline            Mostra pipeline de otimizacao antes de rodar\n";
        std::cout << "  -g | --debug           Gera informacoes de depuracao (DWARF) em ll/obj/asm\n";
        std::cout << "  --asan               Ativa AddressSanitizer (instrumentacao LLVM)\n";
        std::cout << "  --ubsan              Ativa UndefinedBehaviorSanitizer (instrumentacao LLVM)\n";
        std::cout << "  --run                Gera IR e executa com 'lli'\n";
        std::cout << "  --fast2d=<modo>      Controla fast-paths 2D: off|auto|always (padrao: auto)\n";
        std::cout << "  --target=<triple>    Define o target triple (ex.: aarch64-apple-darwin, x86_64-apple-darwin)\n";
        std::cout << "  -o <arquivo>         Especifica saída (também para --emit-ll)\n";
        std::cout << "\nDica: você pode passar só o arquivo (sem flag) para rodar --check por padrão.\n";
        return 0;
    }

    auto startsWith = [](const std::string& s, const char* pfx){ return s.rfind(pfx, 0) == 0; };

    // Se argv[1] NÃO é flag, tratamos como arquivo e assumimos modo --check
    if (!isFlag(mode)) {
        file = mode;      // argv[1] é o arquivo
        mode = std::string("--check");
    } else {
        // Modo com flags
        bool modeEmitLL = (mode == "--emit-ll" || mode == "--emit-llvm" ||
                           mode.rfind("--emit-ll=", 0) == 0 || mode.rfind("--emit-llvm=", 0) == 0);
        bool modeEmitOBJ = (mode == "--emit-obj" || mode.rfind("--emit-obj=", 0) == 0);
        bool modeRun = (mode == "--run");
        bool modeEmitEXE = (mode == "--emit-exe" || mode.rfind("--emit-exe=", 0) == 0);
        bool modeEmitLLOpt = (mode == "--emit-ll-opt" || mode.rfind("--emit-ll-opt=", 0) == 0);
        bool modeEmitASM = (mode == "--emit-asm" || mode.rfind("--emit-asm=", 0) == 0);
        bool modeEmitBC  = (mode == "--emit-bc"  || mode.rfind("--emit-bc=",  0) == 0);

        // Trata --opt=... quando vier como primeira flag e combine com outra flag depois
        bool consumedOptFirst = false;
        if (startsWith(mode, "--opt=")) {
            optLevel = mode.substr(std::string("--opt=").size());
            if (optLevel.empty()) {
                std::cerr << "error: nivel de otimizacao vazio\n";
                return 1;
            }
            // Se houver uma segunda flag, trate-a como modo principal
            if (argc < 3) {
                std::cerr << "error: faltou o caminho do arquivo .my\n";
                return 1;
            }
            mode = argv[2];
            consumedOptFirst = true;
            optProvided = true;
            // recalc modos
            modeEmitLL = (mode == "--emit-ll" || mode == "--emit-llvm" ||
                           startsWith(mode, "--emit-ll=") || startsWith(mode, "--emit-llvm="));
            modeEmitOBJ = (mode == "--emit-obj" || startsWith(mode, "--emit-obj="));
            modeRun = (mode == "--run");
            modeEmitEXE = (mode == "--emit-exe" || startsWith(mode, "--emit-exe="));
            modeEmitLLOpt = (mode == "--emit-ll-opt" || startsWith(mode, "--emit-ll-opt="));
            modeEmitASM = (mode == "--emit-asm" || startsWith(mode, "--emit-asm="));
            modeEmitBC  = (mode == "--emit-bc"  || startsWith(mode, "--emit-bc="));
        }

        // Trata --target=<triple> quando vem antes do modo
        if (startsWith(mode, "--target=")) {
            targetTripleArg = mode.substr(std::string("--target=").size());
            if (targetTripleArg.empty()) {
                std::cerr << "error: --target requer um triple (ex.: aarch64-apple-darwin)\n";
                return 1;
            }
            if (argc < 3) {
                std::cerr << "error: faltou o modo e o arquivo de entrada\n";
                return 1;
            }
            mode = argv[2];
            // recalc modos
            modeEmitLL = (mode == "--emit-ll" || mode == "--emit-llvm" ||
                           startsWith(mode, "--emit-ll=") || startsWith(mode, "--emit-llvm="));
            modeEmitOBJ = (mode == "--emit-obj" || startsWith(mode, "--emit-obj="));
            modeRun = (mode == "--run");
            modeEmitEXE = (mode == "--emit-exe" || startsWith(mode, "--emit-exe="));
            modeEmitLLOpt = (mode == "--emit-ll-opt" || startsWith(mode, "--emit-ll-opt="));
            modeEmitASM = (mode == "--emit-asm" || startsWith(mode, "--emit-asm="));
            modeEmitBC  = (mode == "--emit-bc"  || startsWith(mode, "--emit-bc="));
        }

        // Trata quando --asan/--ubsan vierem antes do modo principal
        if (mode == "--asan" || mode == "--ubsan") {
            if (argc < 3) {
                std::cerr << "error: faltou o modo e o arquivo de entrada\n";
                return 1;
            }
            mode = argv[2];
            // recalc modos
            modeEmitLL = (mode == "--emit-ll" || mode == "--emit-llvm" ||
                           startsWith(mode, "--emit-ll=") || startsWith(mode, "--emit-llvm="));
            modeEmitOBJ = (mode == "--emit-obj" || startsWith(mode, "--emit-obj="));
            modeRun = (mode == "--run");
            modeEmitEXE = (mode == "--emit-exe" || startsWith(mode, "--emit-exe="));
            modeEmitLLOpt = (mode == "--emit-ll-opt" || startsWith(mode, "--emit-ll-opt="));
            modeEmitASM = (mode == "--emit-asm" || startsWith(mode, "--emit-asm="));
            modeEmitBC  = (mode == "--emit-bc"  || startsWith(mode, "--emit-bc="));
        }

        if (modeEmitLL) {
            emitLL = true;
            // permite --emit-ll=out.ll ou --emit-llvm=out.ll
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outPath = mode.substr(eq + 1);
            }

            int idx = consumedOptFirst ? 3 : 2; // próximo argumento deve ser -o ou o arquivo de entrada
            if (argc <= idx) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
            // permita flags após o modo; procure primeiro não-flag como arquivo
            auto isDashFlag = [](const std::string& s){ return (s=="-o"||s=="-g"|| (s.size()>=2 && s[0]=='-' && s[1]=='O')); };
            // trata -o se aparecer
            for (int j = idx; j < argc; ++j) {
                std::string a = argv[j];
                if (a == "-o") { if (j+1<argc) { outPath = argv[j+1]; ++j; } continue; }
                if (a.rfind("--",0)==0 || isDashFlag(a)) continue;
                file = a; break;
            }
            if (file.empty()) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
        } else if (modeEmitOBJ) {
            emitOBJ = true;
            // permite --emit-obj=out.o
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outObjPath = mode.substr(eq + 1);
            }

            int idx = consumedOptFirst ? 3 : 2; // pode vir -o <arq> antes do arquivo de entrada
            if (argc <= idx) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
            auto isDashFlag = [](const std::string& s){ return (s=="-o"||s=="-g"|| (s.size()>=2 && s[0]=='-' && s[1]=='O')); };
            for (int j = idx; j < argc; ++j) {
                std::string a = argv[j];
                if (a == "-o") { if (j+1<argc) { outObjPath = argv[j+1]; ++j; } continue; }
                if (a.rfind("--",0)==0 || isDashFlag(a)) continue;
                file = a; break;
            }
            if (file.empty()) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
        } else if (modeEmitEXE) {
            emitEXE = true;
            // permite --emit-exe=out
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outExePath = mode.substr(eq + 1);
            }

            int idx = consumedOptFirst ? 3 : 2; // pode vir -o <arq> antes do arquivo de entrada
            if (argc <= idx) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
            auto isDashFlag = [](const std::string& s){ return (s=="-o"||s=="-g"|| (s.size()>=2 && s[0]=='-' && s[1]=='O')); };
            for (int j = idx; j < argc; ++j) {
                std::string a = argv[j];
                if (a == "-o") { if (j+1<argc) { outExePath = argv[j+1]; ++j; } continue; }
                if (a.rfind("--",0)==0 || isDashFlag(a)) continue;
                file = a; break;
            }
            if (file.empty()) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
        } else if (modeEmitBC) {
            emitBC = true;
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outBCPath = mode.substr(eq + 1);
            }

            int idx = consumedOptFirst ? 3 : 2; // pode vir -o <arq> antes do arquivo de entrada
            if (argc <= idx) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
            auto isDashFlag = [](const std::string& s){ return (s=="-o"||s=="-g"|| (s.size()>=2 && s[0]=='-' && s[1]=='O')); };
            for (int j = idx; j < argc; ++j) {
                std::string a = argv[j];
                if (a == "-o") { if (j+1<argc) { outBCPath = argv[j+1]; ++j; } continue; }
                if (a.rfind("--",0)==0 || isDashFlag(a)) continue;
                file = a; break;
            }
            if (file.empty()) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
        } else if (modeEmitLLOpt) {
            emitLLOpt = true;
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outOptLLPath = mode.substr(eq + 1);
            }

            int idx = consumedOptFirst ? 3 : 2; // pode vir -o <arq> antes do arquivo de entrada
            if (argc <= idx) {
                std::cerr << "error: faltou o caminho do arquivo .my\n";
                return 1;
            }
            if (std::string(argv[idx]) == "-o") {
                if (argc < idx + 3) {
                    std::cerr << "error: faltou o arquivo de saída ou o arquivo de entrada\n";
                    return 1;
                }
                outOptLLPath = argv[idx + 1];
                // procurar o primeiro argumento não-flag após -o <arq>
                bool found = false;
                for (int j = idx + 2; j < argc; ++j) {
                    std::string a = argv[j];
                    if (a == "-o") { ++j; continue; }
                    if (a.rfind("--", 0) == 0) continue;
                    if (a == "-g" || (a.size()>=2 && a[0]=='-' && a[1]=='O')) continue;
                    file = a; found = true; break;
                }
                if (!found) {
                    std::cerr << "error: faltou o caminho do arquivo .my\n";
                    return 1;
                }
            } else {
                // sem -o: pegue o primeiro não-flag
                bool found = false;
                for (int j = idx; j < argc; ++j) {
                    std::string a = argv[j];
                    if (a == "-o") { ++j; continue; }
                    if (a.rfind("--", 0) == 0) continue;
                    if (a == "-g" || (a.size()>=2 && a[0]=='-' && a[1]=='O')) continue;
                    file = a; found = true; break;
                }
                if (!found) {
                    std::cerr << "error: faltou o caminho do arquivo .my\n";
                    return 1;
                }
            }
        } else if (modeEmitASM) {
            emitASM = true;
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outAsmPath = mode.substr(eq + 1);
            }

            int idx = consumedOptFirst ? 3 : 2; // pode vir -o <arq> antes do arquivo de entrada
            if (argc <= idx) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
            auto isDashFlag = [](const std::string& s){ return (s=="-o"||s=="-g"|| (s.size()>=2 && s[0]=='-' && s[1]=='O')); };
            for (int j = idx; j < argc; ++j) {
                std::string a = argv[j];
                if (a == "-o") { if (j+1<argc) { outAsmPath = argv[j+1]; ++j; } continue; }
                if (a.rfind("--",0)==0 || isDashFlag(a)) continue;
                file = a; break;
            }
            if (file.empty()) { std::cerr << "error: faltou o caminho do arquivo .my\n"; return 1; }
        } else if (modeRun) {
            runMode = true;
            int idx = consumedOptFirst ? 3 : 2;
            if (argc < idx+1) {
                std::cerr << "error: faltou o caminho do arquivo .my\n";
                return 1;
            }
            file = argv[idx];
        } else {
            // Demais modos esperam o arquivo em argv[2]
            int idx = consumedOptFirst ? 3 : 2;
            if (argc < idx+1) {
                std::cerr << "error: faltou o caminho do arquivo .my\n";
                return 1;
            }
            file = argv[idx];
        }
    }

    // Helper de otimização: usa OptPipeline textual se fornecida; caso contrário, OptLevel
    auto optimizeModule = [&](llvm::Module& M,
                             llvm::TargetMachine* TM,
                             const std::string& OptLevel,
                             const std::string& OptPipeline,
                             std::string& Err) -> bool {
        using namespace llvm;

        LoopAnalysisManager     LAM;
        FunctionAnalysisManager FAM;
        CGSCCAnalysisManager    CGAM;
        ModuleAnalysisManager   MAM;

        PassBuilder PB(TM);
        // (Opcional) instrumentações/AA: omitidos para simplicidade/compatibilidade

        PB.registerModuleAnalyses(MAM);
        PB.registerCGSCCAnalyses(CGAM);
        PB.registerFunctionAnalyses(FAM);
        PB.registerLoopAnalyses(LAM);
        PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

        ModulePassManager MPM;
        if (!OptPipeline.empty()) {
            if (auto ErrE = PB.parsePassPipeline(MPM, OptPipeline)) {
                Err = "pipeline invalido em --opt-pipeline";
                return false;
            }
        } else {
            OptimizationLevel OL = OptimizationLevel::O0;
            if      (OptLevel == "O0") OL = OptimizationLevel::O0;
            else if (OptLevel == "O1") OL = OptimizationLevel::O1;
            else if (OptLevel == "O2") OL = OptimizationLevel::O2;
            else if (OptLevel == "O3") OL = OptimizationLevel::O3;
            else if (OptLevel == "Os") OL = OptimizationLevel::Os;
            else if (OptLevel == "Oz") OL = OptimizationLevel::Oz;
            else { Err = "nivel de otimizacao invalido em --opt (use O0|O1|O2|O3|Os|Oz)"; return false; }

            MPM = PB.buildPerModuleDefaultPipeline(OL);
        }

        // Patch 14: add sanitizer instrumentation passes
        (void)useASan; // instrumentation handled in codegen for our targets
        // UBSan: no dedicated LLVM pass in this build; we inject div-by-zero checks in codegen.

        // Patch 11: opcionalmente imprime o pipeline textual antes de executar
        if (gPrintPipeline) {
            std::string Text;
            raw_string_ostream OS(Text);
            MPM.printPipeline(OS, [](llvm::StringRef N) { return N; });
            OS.flush();
            errs() << "[pipeline] " << Text << "\n";
        }

        MPM.run(M, MAM);
        return true;
    };

    std::string src = readFile(file);
    if (src.empty()) {
        std::cerr << file << ": error: nao foi possivel ler o arquivo\n";
        return 1;
    }

    Diag diag(file);
    Lexer lex(src, diag);
    auto toks = lex.tokenize();

    if (diag.hadError) { diag.printAll(); return 1; }

    Parser parser(toks, diag, file);
    auto prog = parser.parse();

    if (diag.hadError) { diag.printAll(); return 1; }

    if (mode == "--parse-only") {
        std::cout << "OK: parse concluido\n";
        return 0;
    } else if (mode == "--dump-ast") {
        if (prog) prog->dump(std::cout);
        return 0;
    } else if (mode == "--dump-ir") {
        // Semântica antes de gerar IR
        {
            SemanticChecker sem(diag);
            bool ok = sem.run(prog.get());
            if (!ok || diag.hadError) return 1;
        }
        // Gera IR
        Codegen cg("mycc_module", diag, debugEnabled, file, /*ubsanEnabled=*/useUBSan, /*asanEnabled=*/useASan);
        {
            using F = Codegen::Fast2DMode;
            if      (fast2dArg == "off")    cg.setFast2DMode(F::Off);
            else if (fast2dArg == "always") cg.setFast2DMode(F::Always);
            else                              cg.setFast2DMode(F::Auto);
        }
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        // Marcar triple no IR, se fornecido
        if (!targetTripleArg.empty()) {
            module->setTargetTriple(llvm::Triple(targetTripleArg));
        }

        // Marcar triple no IR, se fornecido
        if (!targetTripleArg.empty()) {
            module->setTargetTriple(llvm::Triple(targetTripleArg));
        }

        // Marcar triple no IR, se fornecido
        if (!targetTripleArg.empty()) {
            module->setTargetTriple(llvm::Triple(targetTripleArg));
        }

        // Verifica IR
        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR inválido gerado (verifyModule)\n";
            return 1;
        }

        // Otimização opcional
        if (!optPipeline.empty() || (optProvided && optLevel != "O0") || useASan || useUBSan) {
            std::string e; if (!optimizeModule(*module, nullptr, optLevel, optPipeline, e)) {
                std::cerr << "error: " << e << "\n"; return 1;
            }
            if (llvm::verifyModule(*module, &llvm::errs())) {
                std::cerr << "error: IR invalido apos otimizacao\n"; return 1;
            }
        }

        module->print(llvm::outs(), nullptr);
        return 0;
    } else if (mode == "--check") {
        SemanticChecker sem(diag);
        bool ok = sem.run(prog.get());
        if (!ok || diag.hadError) { diag.printAll(); return 1; }
        std::cout << "OK: semantica concluida\n";
        return 0;
    }

    if (emitLL) {
        // Semântica antes de gerar IR
        {
            SemanticChecker sem(diag);
            bool ok = sem.run(prog.get());
            if (!ok || diag.hadError) return 1;
        }

        Codegen cg("mycc_module", diag, debugEnabled, file, /*ubsanEnabled=*/useUBSan, /*asanEnabled=*/useASan);
        {
            using F = Codegen::Fast2DMode;
            if      (fast2dArg == "off")    cg.setFast2DMode(F::Off);
            else if (fast2dArg == "always") cg.setFast2DMode(F::Always);
            else                              cg.setFast2DMode(F::Auto);
        }
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        // Verifica IR
        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR inválido gerado (verifyModule)\n";
            return 1;
        }

        // Otimização opcional (ou apenas imprimir pipeline com -O0)
        if (!optPipeline.empty() || (optProvided && optLevel != "O0") || gPrintPipeline || useASan || useUBSan) {
            std::string e; if (!optimizeModule(*module, nullptr, optLevel, optPipeline, e)) {
                std::cerr << "error: " << e << "\n"; return 1; }
            if (llvm::verifyModule(*module, &llvm::errs())) {
                std::cerr << "error: IR invalido apos otimizacao\n"; return 1; }
        }

        // Sem -o (e sem --emit-ll=...), salvar em <input>.ll
        if (outPath.empty()) {
            auto pos = file.rfind('.');
            std::string base = (pos == std::string::npos) ? file : file.substr(0, pos);
            outPath = base + ".ll";
        }

        std::error_code ec;
        llvm::raw_fd_ostream out(outPath, ec, llvm::sys::fs::OF_Text);
        if (ec) {
            std::cerr << outPath << ": error: " << ec.message() << "\n";
            return 1;
        }
        module->print(out, nullptr);
        out.flush();
        std::cout << "OK: IR salvo em " << outPath << "\n";
        return 0;
    }

    if (emitLLOpt) {
        // Semântica antes de gerar IR
        {
            SemanticChecker sem(diag);
            bool ok = sem.run(prog.get());
            if (!ok || diag.hadError) return 1;
        }

        Codegen cg("mycc_module", diag, debugEnabled, file, /*ubsanEnabled=*/useUBSan, /*asanEnabled=*/useASan);
        {
            using F = Codegen::Fast2DMode;
            if      (fast2dArg == "off")    cg.setFast2DMode(F::Off);
            else if (fast2dArg == "always") cg.setFast2DMode(F::Always);
            else                              cg.setFast2DMode(F::Auto);
        }
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        // Verifica IR
        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR inválido gerado (verifyModule)\n";
            return 1;
        }

        // Otimização obrigatória (usa pipeline textual se fornecida; caso contrário, nível)
        if (!optPipeline.empty() || optProvided || optLevel != "O0" || useASan || useUBSan) {
            std::string e; if (!optimizeModule(*module, nullptr, optLevel, optPipeline, e)) {
                std::cerr << "error: " << e << "\n"; return 1; }
            if (llvm::verifyModule(*module, &llvm::errs())) {
                std::cerr << "error: IR invalido apos otimizacao\n"; return 1; }
        }

        // Caminho padrão <input>.opt.ll
        if (outOptLLPath.empty()) {
            auto pos = file.rfind('.');
            std::string base = (pos == std::string::npos) ? file : file.substr(0, pos);
            outOptLLPath = base + ".opt.ll";
        }

        std::error_code ec;
        llvm::raw_fd_ostream out(outOptLLPath, ec, llvm::sys::fs::OF_Text);
        if (ec) {
            std::cerr << outOptLLPath << ": error: " << ec.message() << "\n";
            return 1;
        }
        module->print(out, nullptr);
        out.flush();
        std::cout << "OK: IR otimizado salvo em " << outOptLLPath << "\n";
        return 0;
    }

    if (emitBC) {
        // Semântica antes de gerar IR
        {
            SemanticChecker sem(diag);
            bool ok = sem.run(prog.get());
            if (!ok || diag.hadError) return 1;
        }

        Codegen cg("mycc_module", diag, debugEnabled, file, /*ubsanEnabled=*/useUBSan, /*asanEnabled=*/useASan);
        {
            using F = Codegen::Fast2DMode;
            if      (fast2dArg == "off")    cg.setFast2DMode(F::Off);
            else if (fast2dArg == "always") cg.setFast2DMode(F::Always);
            else                              cg.setFast2DMode(F::Auto);
        }
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR invalido\n";
            return 1;
        }

        // Otimização opcional
        if (!optPipeline.empty() || (optProvided && optLevel != "O0") || useASan || useUBSan) {
            std::string e; if (!optimizeModule(*module, nullptr, optLevel, optPipeline, e)) {
                std::cerr << "error: " << e << "\n"; return 1; }
            if (llvm::verifyModule(*module, &llvm::errs())) {
                std::cerr << "error: IR invalido apos otimizacao\n"; return 1; }
        }

        if (outBCPath.empty()) {
            auto pos = file.rfind('.');
            std::string base = (pos == std::string::npos) ? file : file.substr(0, pos);
            outBCPath = base + ".bc";
        }

        std::error_code ec;
        llvm::raw_fd_ostream out(outBCPath, ec, llvm::sys::fs::OF_None);
        if (ec) {
            std::cerr << outBCPath << ": error: " << ec.message() << "\n";
            return 1;
        }
        llvm::WriteBitcodeToFile(*module, out);
        out.flush();
        std::cout << "OK: bitcode salvo em " << outBCPath << "\n";
        return 0;
    }

    if (emitASM) {
        // Semântica antes de gerar IR
        {
            SemanticChecker sem(diag);
            bool ok = sem.run(prog.get());
            if (!ok || diag.hadError) return 1;
        }

        Codegen cg("mycc_module", diag, debugEnabled, file, /*ubsanEnabled=*/useUBSan, /*asanEnabled=*/useASan);
        {
            using F = Codegen::Fast2DMode;
            if      (fast2dArg == "off")    cg.setFast2DMode(F::Off);
            else if (fast2dArg == "always") cg.setFast2DMode(F::Always);
            else                              cg.setFast2DMode(F::Auto);
        }
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR invalido\n";
            return 1;
        }

        // Inicializa alvo nativo
        static bool inited = false;
        if (!inited) {
            llvm::InitializeNativeTarget();
            llvm::InitializeNativeTargetAsmPrinter();
            llvm::InitializeNativeTargetAsmParser();
            inited = true;
        }

        auto targetTriple = llvm::sys::getDefaultTargetTriple();
        llvm::Triple TT(targetTriple);
        module->setTargetTriple(TT);

        std::string terr;
        const llvm::Target* target = llvm::TargetRegistry::lookupTarget("", TT, terr);
        if (!target) {
            std::cerr << "error: " << terr << "\n";
            return 1;
        }

        llvm::TargetOptions opt;
        auto rm = llvm::Reloc::Model::PIC_;
        std::unique_ptr<llvm::TargetMachine> TM(
            target->createTargetMachine(TT, "generic", "", opt, rm)
        );
        module->setDataLayout(TM->createDataLayout());

        // Otimização opcional (com TM disponível)
        if (!optPipeline.empty() || (optProvided && optLevel != "O0") || useASan || useUBSan) {
            std::string e; if (!optimizeModule(*module, TM.get(), optLevel, optPipeline, e)) {
                std::cerr << "error: " << e << "\n"; return 1; }
            if (llvm::verifyModule(*module, &llvm::errs())) {
                std::cerr << "error: IR invalido apos otimizacao\n"; return 1; }
        }

        // Otimização opcional (com TM disponível)
        if (!optPipeline.empty() || (optProvided && optLevel != "O0")) {
            std::string e; if (!optimizeModule(*module, TM.get(), optLevel, optPipeline, e)) {
                std::cerr << "error: " << e << "\n"; return 1; }
            if (llvm::verifyModule(*module, &llvm::errs())) {
                std::cerr << "error: IR invalido apos otimizacao\n"; return 1; }
        }

        // Caminho de saída padrão (.s ao lado do .my)
        if (outAsmPath.empty()) {
            outAsmPath = file;
            auto pos = outAsmPath.find_last_of('.');
            if (pos != std::string::npos) outAsmPath = outAsmPath.substr(0, pos);
            outAsmPath += ".s";
        }

        std::error_code ec;
        llvm::raw_fd_ostream dest(outAsmPath, ec, llvm::sys::fs::OF_Text);
        if (ec) {
            std::cerr << outAsmPath << ": error: " << ec.message() << "\n";
            return 1;
        }

        llvm::legacy::PassManager pass;
        if (TM->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::AssemblyFile)) {
            std::cerr << "error: este alvo nao suporta emissao de assembly\n";
            return 1;
        }
        pass.run(*module);
        dest.flush();

        std::cout << "OK: assembly salvo em " << outAsmPath << "\n";
        return 0;
    }

    if (emitOBJ) {
        // Semântica antes de gerar IR
        {
            SemanticChecker sem(diag);
            bool ok = sem.run(prog.get());
            if (!ok || diag.hadError) return 1;
        }

        Codegen cg("mycc_module", diag, debugEnabled, file, /*ubsanEnabled=*/useUBSan, /*asanEnabled=*/useASan);
        {
            using F = Codegen::Fast2DMode;
            if      (fast2dArg == "off")    cg.setFast2DMode(F::Off);
            else if (fast2dArg == "always") cg.setFast2DMode(F::Always);
            else                              cg.setFast2DMode(F::Auto);
        }
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        // Verifica IR
        // Inicializa alvo nativo (uma vez)
        static bool inited = false;
        if (!inited) {
            llvm::InitializeNativeTarget();
            llvm::InitializeNativeTargetAsmPrinter();
            llvm::InitializeNativeTargetAsmParser();
            inited = true;
        }

        // Triple escolhido ou nativo
        llvm::Triple TT(targetTripleArg.empty() ? llvm::sys::getDefaultTargetTriple()
                                               : targetTripleArg);
        module->setTargetTriple(TT);

        std::string terr;
        const llvm::Target* target = llvm::TargetRegistry::lookupTarget("", TT, terr);
        if (!target) {
            std::cerr << "error: " << terr << "\n";
            return 1;
        }

        llvm::TargetOptions opt;
        auto rm = llvm::Reloc::Model::PIC_;
        std::unique_ptr<llvm::TargetMachine> TM(
            target->createTargetMachine(TT, "generic", "", opt, rm)
        );

        module->setDataLayout(TM->createDataLayout());

        // Verifica IR
        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR invalido\n";
            return 1;
        }

        // Otimização opcional (com TM disponível)
        if (!optPipeline.empty() || (optProvided && optLevel != "O0") || useASan || useUBSan) {
            std::string e; if (!optimizeModule(*module, TM.get(), optLevel, optPipeline, e)) {
                std::cerr << "error: " << e << "\n"; return 1; }
            if (llvm::verifyModule(*module, &llvm::errs())) {
                std::cerr << "error: IR invalido apos otimizacao\n"; return 1; }
        }

        // Caminho de saída padrão (.o ao lado do .my)
        if (outObjPath.empty()) {
            outObjPath = file;
            auto pos = outObjPath.find_last_of('.');
            if (pos != std::string::npos) outObjPath = outObjPath.substr(0, pos);
            outObjPath += ".o";
        }

        std::error_code ec;
        llvm::raw_fd_ostream dest(outObjPath, ec, llvm::sys::fs::OF_None);
        if (ec) {
            std::cerr << outObjPath << ": error: " << ec.message() << "\n";
            return 1;
        }

        llvm::legacy::PassManager pass;
        if (TM->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::ObjectFile)) {
            std::cerr << "error: este alvo nao suporta emissao de objeto\n";
            return 1;
        }
        pass.run(*module);
        dest.flush();

        std::cout << "OK: objeto salvo em " << outObjPath << "\n";
        return 0;
    }

    if (emitEXE) {
        // 1) Semântica
        {
            SemanticChecker sem(diag);
            bool ok = sem.run(prog.get());
            if (!ok || diag.hadError) return 1;
        }

        // 2) Gera IR
        Codegen cg("mycc_module", diag, debugEnabled, file, /*ubsanEnabled=*/useUBSan, /*asanEnabled=*/useASan);
        {
            using F = Codegen::Fast2DMode;
            if      (fast2dArg == "off")    cg.setFast2DMode(F::Off);
            else if (fast2dArg == "always") cg.setFast2DMode(F::Always);
            else                              cg.setFast2DMode(F::Auto);
        }
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        // 3) Verifica IR
        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR invalido\n";
            return 1;
        }

        // 4) Inicializa alvo nativo
        static bool inited = false;
        if (!inited) {
            llvm::InitializeNativeTarget();
            llvm::InitializeNativeTargetAsmPrinter();
            llvm::InitializeNativeTargetAsmParser();
            inited = true;
        }

        auto targetTriple = llvm::sys::getDefaultTargetTriple();
        llvm::Triple TT(targetTriple);
        module->setTargetTriple(TT);

        std::string terr;
        const llvm::Target* target = llvm::TargetRegistry::lookupTarget("", TT, terr);
        if (!target) {
            std::cerr << "error: " << terr << "\n";
            return 1;
        }

        llvm::TargetOptions opt;
        auto rm = llvm::Reloc::Model::PIC_;
        std::unique_ptr<llvm::TargetMachine> TM(
            target->createTargetMachine(TT, "generic", "", opt, rm)
        );
        module->setDataLayout(TM->createDataLayout());

        // Otimização opcional
        if (!optPipeline.empty() || (optProvided && optLevel != "O0") || useASan || useUBSan) {
            std::string e; if (!optimizeModule(*module, nullptr, optLevel, optPipeline, e)) {
                std::cerr << "error: " << e << "\n"; return 1; }
            if (llvm::verifyModule(*module, &llvm::errs())) {
                std::cerr << "error: IR invalido apos otimizacao\n"; return 1; }
        }

        // 5) Caminhos: .o temporário e exe final
        std::string base = file;
        auto pos = base.find_last_of('/');
        std::string leaf = (pos==std::string::npos) ? base : base.substr(pos+1);
        auto dot = leaf.find_last_of('.');
        if (dot != std::string::npos) leaf = leaf.substr(0, dot);

        std::string objTmp = std::string("/tmp/") + leaf + ".o";
        if (outExePath.empty()) {
            outExePath = file;
            auto p = outExePath.find_last_of('.');
            if (p != std::string::npos) outExePath = outExePath.substr(0, p);
        }

        // 6) Emitir .o
        {
            std::error_code ec;
            llvm::raw_fd_ostream dest(objTmp, ec, llvm::sys::fs::OF_None);
            if (ec) {
                std::cerr << objTmp << ": error: " << ec.message() << "\n";
                return 1;
            }
            llvm::legacy::PassManager pass;
            if (TM->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::ObjectFile)) {
                std::cerr << "error: este alvo nao suporta emissao de objeto\n";
                return 1;
            }
            pass.run(*module);
            dest.flush();
        }

        // 7) Descobre runtime e linka com clang
        auto shellQuote = [](const std::string& s) {
            std::string out = "'";
            for (char c : s) {
                if (c == '\'') out += "'\"'\"'";
                else out += c;
            }
            out += "'";
            return out;
        };
        auto runAndCapture = [](const char* cmd) -> std::string {
            std::string out;
            FILE* p = popen(cmd, "r");
            if (!p) return out;
            char buf[256];
            while (fgets(buf, sizeof(buf), p)) out += buf;
            pclose(p);
            while (!out.empty() && (out.back()=='\n' || out.back()=='\r' || out.back()==' ' || out.back()=='\t')) out.pop_back();
            return out;
        };

        std::string clangBin = "clang";
#ifdef __APPLE__
        // opcional: forcar arquitetura ao clang do sistema
#endif

        std::string archFlag;
        if (TT.isArch64Bit()) {
            if (TT.getArch() == llvm::Triple::aarch64) archFlag = " -arch arm64";
            else if (TT.getArch() == llvm::Triple::x86_64) archFlag = " -arch x86_64";
        }

        // Resolve caminho da runtime: --rt=... > default embutido > erro
        std::string rt = rtPath;
#ifdef MYCC_DEFAULT_RT_PATH
        if (rt.empty()) rt = MYCC_DEFAULT_RT_PATH;
#endif
        if (rt.empty()) {
            std::cerr << "error: runtime nao informado. Use --rt=<caminho_para_libmycc_runtime.a>\n";
            return 1;
        }

        std::string cmd = clangBin
            + archFlag
            + " -o " + shellQuote(outExePath)
            + " " + shellQuote(objTmp)
            + " " + shellQuote(rt);
        // Sanitizer runtimes not linked explicitly; checks injected in IR
        // Se o SDK padrão estiver incorreto no ambiente, force via xcrun
#ifdef __APPLE__
        {
            std::string sysroot = runAndCapture("xcrun --sdk macosx --show-sdk-path 2>/dev/null");
            if (!sysroot.empty()) {
                cmd += " -isysroot " + shellQuote(sysroot);
            }
            // Alinha a versão mínima do macOS ao alvo das libs LLVM (evita warnings)
            cmd += " -mmacosx-version-min=15.0";
        }
#endif

        int rc = std::system(cmd.c_str());
        if (rc != 0) {
            std::cerr << "error: link falhou (clang retornou " << rc << ")\n";
            std::cerr << "cmd: " << cmd << "\n";
            return 1;
        }

        std::cout << "OK: executavel salvo em " << outExePath << "\n";
        return 0;
    }

    if (runMode) {
        // 1) Semântica
        {
            SemanticChecker sem(diag);
            bool ok = sem.run(prog.get());
            if (!ok || diag.hadError) return 1;
        }

        // 2) Gera IR
        Codegen cg("mycc_module", diag, debugEnabled, file, /*ubsanEnabled=*/useUBSan, /*asanEnabled=*/useASan);
        {
            using F = Codegen::Fast2DMode;
            if      (fast2dArg == "off")    cg.setFast2DMode(F::Off);
            else if (fast2dArg == "always") cg.setFast2DMode(F::Always);
            else                              cg.setFast2DMode(F::Auto);
        }
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR invalido\n";
            return 1;
        }

        // Otimização opcional
        if (!optPipeline.empty() || (optProvided && optLevel != "O0") || useASan || useUBSan) {
            std::string e; if (!optimizeModule(*module, nullptr, optLevel, optPipeline, e)) {
                std::cerr << "error: " << e << "\n"; return 1; }
            if (llvm::verifyModule(*module, &llvm::errs())) {
                std::cerr << "error: IR invalido apos otimizacao\n"; return 1; }
        }

        // 3) Arquivo temporario .ll
        std::string tmpLL = file;
        {
            auto pos = tmpLL.find_last_of('/');
            std::string base = (pos == std::string::npos) ? tmpLL : tmpLL.substr(pos+1);
            pos = base.find_last_of('.');
            if (pos != std::string::npos) base = base.substr(0,pos);
            tmpLL = "/tmp/" + base + ".ll";
        }

        {
            std::error_code ec;
            llvm::raw_fd_ostream out(tmpLL, ec, llvm::sys::fs::OF_Text);
            if (ec) {
                std::cerr << tmpLL << ": error: " << ec.message() << "\n";
                return 1;
            }
            module->print(out, nullptr);
            out.flush();
        }

        // 4) Executa com lli
        int rc = std::system((std::string("lli ") + tmpLL).c_str());
        if (rc != 0) {
            std::cerr << "error: lli retornou codigo " << rc << "\n";
            return 1;
        }
        return 0;
    }

    std::cerr << "error: flag desconhecida '" << mode << "'\n";
    return 1;
}

} // namespace cli
} // namespace mycc
