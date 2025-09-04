#pragma once
#include <string>
#include <vector>

namespace mycc {

// Nível/severidade de cada diagnóstico
enum class DiagLevel { Error, Warning, Info };

// Um item de diagnóstico armazenado para impressão posterior
struct DiagItem {
    DiagLevel    level;
    std::string  file;
    int          line = 0;
    int          col  = 0;
    std::string  msg;
    std::string  lineText; // opcional: linha fonte para destaque
};

// Coletor/emissor de diagnósticos
class Diag {
public:
    Diag() = default;
    explicit Diag(std::string defaultFile) : defaultFile_(std::move(defaultFile)) {}

    // Configuração
    void setDefaultFile(std::string file);
    void setOptions(int maxErr, bool color);

    // Atalhos de emissão (file/lineText são opcionais; usa defaultFile_ se vazio)
    void error(int line, int col, const std::string& msg);
    void error(int line, int col, const std::string& msg,
               const std::string& file, const std::string& lineText = "");
    void warn (int line, int col, const std::string& msg);
    void warn (int line, int col, const std::string& msg,
               const std::string& file, const std::string& lineText = "");
    void info (int line, int col, const std::string& msg);
    void info (int line, int col, const std::string& msg,
               const std::string& file, const std::string& lineText = "");

    // Saída/estado
    void printAll() const;
    void clear();

    // Consulta
    bool hasErrors() const { return hadError; }
    const std::vector<DiagItem>& all() const { return items; }

    // Flag pública para checagens rápidas (mantida por error())
    bool hadError = false;

private:
    void add(DiagLevel level, int line, int col, const std::string& msg,
             const std::string& file, const std::string& lineText);

    std::vector<DiagItem> items;
    std::string defaultFile_;

    int  maxErrors_ = 50;
    bool useColor_  = true;
};

} // namespace mycc