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
#include <llvm/IR/PassManager.h>
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

namespace mycc {
namespace cli {

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
    bool emitASM = false;
    std::string outAsmPath;
    bool emitBC = false;
    std::string outBCPath;
    std::string optLevel = "O0"; // padrão
    bool optProvided = false;     // se o usuário passou --opt

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
        std::cout << "  --emit-asm           Gera assembly (.s) (sem -o: salva em <input>.s)\n";
        std::cout << "  --emit-asm=<arq>     Gera assembly textual no arquivo informado\n";
        std::cout << "  --emit-bc            Gera bitcode LLVM (.bc) (sem -o: <input>.bc)\n";
        std::cout << "  --emit-bc=<arq>      Gera bitcode no arquivo informado\n";
        std::cout << "  --opt=<O0|O1|O2|O3|Os|Oz>  Define o nivel de otimizacao (padrao: O0)\n";
        std::cout << "  --run                Gera IR e executa com 'lli'\n";
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
            if (argc <= idx) {
                std::cerr << "error: faltou o caminho do arquivo .my\n";
                return 1;
            }
            if (std::string(argv[idx]) == "-o") {
                if (argc < idx + 3) {
                    std::cerr << "error: faltou o arquivo de saída ou o arquivo de entrada\n";
                    return 1;
                }
                // -o sempre prevalece sobre o valor vindo de --emit-ll=...
                outPath = argv[idx + 1];
                file    = argv[idx + 2];
            } else {
                file = argv[idx];
            }
        } else if (modeEmitOBJ) {
            emitOBJ = true;
            // permite --emit-obj=out.o
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outObjPath = mode.substr(eq + 1);
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
                outObjPath = argv[idx + 1];
                file       = argv[idx + 2];
            } else {
                file = argv[idx];
            }
        } else if (modeEmitEXE) {
            emitEXE = true;
            // permite --emit-exe=out
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outExePath = mode.substr(eq + 1);
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
                outExePath = argv[idx + 1];
                file       = argv[idx + 2];
            } else {
                file = argv[idx];
            }
        } else if (modeEmitBC) {
            emitBC = true;
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outBCPath = mode.substr(eq + 1);
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
                outBCPath = argv[idx + 1];
                file      = argv[idx + 2];
            } else {
                file = argv[idx];
            }
        } else if (modeEmitASM) {
            emitASM = true;
            auto eq = mode.find('=');
            if (eq != std::string::npos) {
                outAsmPath = mode.substr(eq + 1);
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
                outAsmPath = argv[idx + 1];
                file       = argv[idx + 2];
            } else {
                file = argv[idx];
            }
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

    // Helpers de otimização
    auto parseOptLevel = [](const std::string& s, llvm::OptimizationLevel& out) -> bool {
        using L = llvm::OptimizationLevel;
        if (s == "O0") { out = L::O0; return true; }
        if (s == "O1") { out = L::O1; return true; }
        if (s == "O2") { out = L::O2; return true; }
        if (s == "O3") { out = L::O3; return true; }
        if (s == "Os") { out = L::Os; return true; }
        if (s == "Oz") { out = L::Oz; return true; }
        return false;
    };
    auto runOptimizations = [](llvm::Module& M, llvm::OptimizationLevel OL) {
        using namespace llvm;
        PassBuilder PB;
        LoopAnalysisManager     LAM;
        FunctionAnalysisManager FAM;
        CGSCCAnalysisManager    CGAM;
        ModuleAnalysisManager   MAM;

        PB.registerModuleAnalyses(MAM);
        PB.registerCGSCCAnalyses(CGAM);
        PB.registerFunctionAnalyses(FAM);
        PB.registerLoopAnalyses(LAM);
        PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

        ModulePassManager MPM;
        MPM = PB.buildPerModuleDefaultPipeline(OL);
        MPM.run(M, MAM);
    };

    std::string src = readFile(file);
    if (src.empty()) {
        std::cerr << file << ": error: nao foi possivel ler o arquivo\n";
        return 1;
    }

    Diag diag(file);
    Lexer lex(src, diag);
    auto toks = lex.tokenize();

    if (diag.hadError) return 1;

    Parser parser(toks, diag);
    auto prog = parser.parse();

    if (diag.hadError) return 1;

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
        Codegen cg("mycc_module", diag);
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        // Verifica IR
        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR inválido gerado (verifyModule)\n";
            return 1;
        }

        // Otimização opcional
        llvm::OptimizationLevel OL;
        if (optProvided) {
            if (!parseOptLevel(optLevel, OL)) {
                std::cerr << "error: nivel de otimizacao invalido: " << optLevel
                          << " (use O0,O1,O2,O3,Os,Oz)\n";
                return 1;
            }
            if (!module->getFunctionList().empty()) {
                runOptimizations(*module, OL);
                if (llvm::verifyModule(*module, &llvm::errs())) {
                    std::cerr << "error: IR invalido apos otimizacao\n";
                    return 1;
                }
            }
        }

        module->print(llvm::outs(), nullptr);
        return 0;
    } else if (mode == "--check") {
        SemanticChecker sem(diag);
        bool ok = sem.run(prog.get());
        if (!ok || diag.hadError) return 1;
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

        Codegen cg("mycc_module", diag);
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        // Verifica IR
        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR inválido gerado (verifyModule)\n";
            return 1;
        }

        // Otimização opcional
        {
            llvm::OptimizationLevel OL;
            if (optProvided) {
                if (!parseOptLevel(optLevel, OL)) {
                    std::cerr << "error: nivel de otimizacao invalido: " << optLevel
                              << " (use O0,O1,O2,O3,Os,Oz)\n";
                    return 1;
                }
                if (!module->getFunctionList().empty()) {
                    runOptimizations(*module, OL);
                    if (llvm::verifyModule(*module, &llvm::errs())) {
                        std::cerr << "error: IR invalido apos otimizacao\n";
                        return 1;
                    }
                }
            }
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

    if (emitBC) {
        // Semântica antes de gerar IR
        {
            SemanticChecker sem(diag);
            bool ok = sem.run(prog.get());
            if (!ok || diag.hadError) return 1;
        }

        Codegen cg("mycc_module", diag);
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR invalido\n";
            return 1;
        }

        // Otimização opcional
        {
            llvm::OptimizationLevel OL;
            if (optProvided) {
                if (!parseOptLevel(optLevel, OL)) {
                    std::cerr << "error: nivel de otimizacao invalido: " << optLevel
                              << " (use O0,O1,O2,O3,Os,Oz)\n";
                    return 1;
                }
                if (!module->getFunctionList().empty()) {
                    runOptimizations(*module, OL);
                    if (llvm::verifyModule(*module, &llvm::errs())) {
                        std::cerr << "error: IR invalido apos otimizacao\n";
                        return 1;
                    }
                }
            }
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

        Codegen cg("mycc_module", diag);
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR invalido\n";
            return 1;
        }

        // Otimização opcional
        {
            llvm::OptimizationLevel OL;
            if (optProvided) {
                if (!parseOptLevel(optLevel, OL)) {
                    std::cerr << "error: nivel de otimizacao invalido: " << optLevel
                              << " (use O0,O1,O2,O3,Os,Oz)\n";
                    return 1;
                }
                if (!module->getFunctionList().empty()) {
                    runOptimizations(*module, OL);
                    if (llvm::verifyModule(*module, &llvm::errs())) {
                        std::cerr << "error: IR invalido apos otimizacao\n";
                        return 1;
                    }
                }
            }
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

        Codegen cg("mycc_module", diag);
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        // Verifica IR
        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR invalido\n";
            return 1;
        }

        // Inicializa alvo nativo (uma vez)
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
        {
            llvm::OptimizationLevel OL;
            if (optProvided) {
                if (!parseOptLevel(optLevel, OL)) {
                    std::cerr << "error: nivel de otimizacao invalido: " << optLevel
                              << " (use O0,O1,O2,O3,Os,Oz)\n";
                    return 1;
                }
                if (!module->getFunctionList().empty()) {
                    runOptimizations(*module, OL);
                    if (llvm::verifyModule(*module, &llvm::errs())) {
                        std::cerr << "error: IR invalido apos otimizacao\n";
                        return 1;
                    }
                }
            }
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
        Codegen cg("mycc_module", diag);
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
        {
            llvm::OptimizationLevel OL;
            if (optProvided) {
                if (!parseOptLevel(optLevel, OL)) {
                    std::cerr << "error: nivel de otimizacao invalido: " << optLevel
                              << " (use O0,O1,O2,O3,Os,Oz)\n";
                    return 1;
                }
                if (!module->getFunctionList().empty()) {
                    runOptimizations(*module, OL);
                    if (llvm::verifyModule(*module, &llvm::errs())) {
                        std::cerr << "error: IR invalido apos otimizacao\n";
                        return 1;
                    }
                }
            }
        }

        // 5) Caminhos: .o temporário e exe final
        std::string base = file;
        auto pos = base.find_last_of('/');
        std::string leaf = (pos==std::string::npos) ? base : base.substr(pos+1);
        auto dot = leaf.find_last_of('.');
        if (dot != std::string::npos) leaf = leaf.substr(0, dot);

        std::string objTmp = std::string("/tmp/") + leaf + ".o";
        if (outExePath.empty()) {
            outExePath = std::string("/tmp/") + leaf;
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

        // 7) Linkar com clang + runtime
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

#ifndef MYCC_RUNTIME_LIB
#  error "MYCC_RUNTIME_LIB nao definido pelo CMake"
#endif

        std::string cmd = clangBin
            + archFlag
            + " -o " + shellQuote(outExePath)
            + " " + shellQuote(objTmp)
            + " " + shellQuote(MYCC_RUNTIME_LIB);
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
        Codegen cg("mycc_module", diag);
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        if (llvm::verifyModule(*module, &llvm::errs())) {
            std::cerr << "error: IR invalido\n";
            return 1;
        }

        // Otimização opcional
        {
            llvm::OptimizationLevel OL;
            if (optLevel.rfind("O", 0) == 0) {
                if (!parseOptLevel(optLevel, OL)) {
                    std::cerr << "error: nivel de otimizacao invalido: " << optLevel
                              << " (use O0,O1,O2,O3,Os,Oz)\n";
                    return 1;
                }
                if (!module->getFunctionList().empty()) {
                    runOptimizations(*module, OL);
                    if (llvm::verifyModule(*module, &llvm::errs())) {
                        std::cerr << "error: IR invalido apos otimizacao\n";
                        return 1;
                    }
                }
            }
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
