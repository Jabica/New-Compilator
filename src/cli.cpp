#include "cli.hpp"
#include "diagnostics.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "ast.hpp"
#include "semantics.hpp"
#include "codegen.hpp"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include <fstream>
#include <sstream>
#include <iostream>
#include <string>

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
        std::cout << "Uso: mycc_cli [--help|--parse-only|--dump-ast|--dump-ir|--check] <arquivo.my>\n";
        return 1;
    }

    std::string mode = argv[1];
    if (mode == "--help") {
        std::cout << "mycc-pt - Compilador Educacional\n";
        std::cout << "Flags disponíveis:\n";
        std::cout << "  --help        Exibe esta mensagem\n";
        std::cout << "  --parse-only  Executa apenas análise léxica/sintática\n";
        std::cout << "  --dump-ast    Mostra a AST gerada\n";
        std::cout << "  --dump-ir     Mostra o IR LLVM\n";
        std::cout << "  --check       Verifica semantica (tabela de simbolos e tipos)\n";
        std::cout << "  --emit-ll     Gera o IR LLVM em formato .ll (stdout ou com -o)\n";
        std::cout << "  -o <arquivo>  Especifica arquivo de saída (para --emit-ll)\n";
        return 0;
    }

    bool emitLL = false;
    std::string outPath;

    std::string file;

    if (mode == "--emit-ll") {
        emitLL = true;
        if (argc < 3) {
            std::cerr << "error: faltou o caminho do arquivo .my\n";
            return 1;
        }
        if (std::string(argv[2]) == "-o") {
            if (argc < 5) {
                std::cerr << "error: faltou o arquivo de saída ou o arquivo de entrada\n";
                return 1;
            }
            outPath = argv[3];
            file = argv[4];
        } else {
            file = argv[2];
        }
    } else {
        if (argc < 3) {
            std::cerr << "error: faltou o caminho do arquivo .my\n";
            return 1;
        }
        file = argv[2];
    }

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
        // Gera e imprime IR real do programa
        Codegen cg("mycc_module", diag);
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;
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
        Codegen cg("mycc_module", diag);
        auto module = cg.run(prog.get());
        if (diag.hadError || !module) return 1;

        if (outPath.empty()) {
            module->print(llvm::outs(), nullptr);
        } else {
            std::error_code ec;
            llvm::raw_fd_ostream out(outPath, ec);
            if (ec) {
                std::cerr << outPath << ": error: " << ec.message() << "\n";
                return 1;
            }
            module->print(out, nullptr);
            out.flush();
        }
        return 0;
    }

    std::cerr << "error: flag desconhecida '" << mode << "'\n";
    return 1;
}

} // namespace cli
} // namespace mycc