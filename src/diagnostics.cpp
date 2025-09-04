#include "diagnostics.hpp"
#include <iostream>
#include <iomanip>

namespace mycc {

static const char* levelStr(DiagLevel lv) {
    switch (lv) {
        case DiagLevel::Error:   return "error";
        case DiagLevel::Warning: return "warning";
        case DiagLevel::Info:    return "info";
    }
    return "info";
}

void Diag::setDefaultFile(std::string file) {
    defaultFile_ = std::move(file);
}

void Diag::setOptions(int maxErr, bool color) {
    maxErrors_ = (maxErr > 0 ? maxErr : 1);
    useColor_  = color;
}

void Diag::add(DiagLevel level, int line, int col,
               const std::string& msg,
               const std::string& file,
               const std::string& lineText)
{
    if (level == DiagLevel::Error) {
        hadError = true;
        // limite de erros
        int currentErrors = 0;
        for (const auto& it : items)
            if (it.level == DiagLevel::Error) ++currentErrors;
        if (currentErrors >= maxErrors_) return;
    }

    DiagItem it;
    it.level    = level;
    it.file     = file.empty() ? defaultFile_ : file;
    it.line     = line;
    it.col      = col;
    it.msg      = msg;
    it.lineText = lineText;

    items.push_back(std::move(it));
}

// -------- Atalhos sem "file" (usa defaultFile_) --------
void Diag::error(int line, int col, const std::string& msg) {
    add(DiagLevel::Error, line, col, msg, /*file*/"", /*lineText*/"");
}
void Diag::warn (int line, int col, const std::string& msg) {
    add(DiagLevel::Warning, line, col, msg, "", "");
}
void Diag::info (int line, int col, const std::string& msg) {
    add(DiagLevel::Info, line, col, msg, "", "");
}

// -------- Overloads com "file" e "lineText" --------
void Diag::error(int line, int col, const std::string& msg,
                 const std::string& file, const std::string& lineText) {
    add(DiagLevel::Error, line, col, msg, file, lineText);
}
void Diag::warn (int line, int col, const std::string& msg,
                 const std::string& file, const std::string& lineText) {
    add(DiagLevel::Warning, line, col, msg, file, lineText);
}
void Diag::info (int line, int col, const std::string& msg,
                 const std::string& file, const std::string& lineText) {
    add(DiagLevel::Info, line, col, msg, file, lineText);
}

void Diag::printAll() const {
    const char* RED    = useColor_ ? "\033[31m" : "";
    const char* YELLOW = useColor_ ? "\033[33m" : "";
    const char* BLUE   = useColor_ ? "\033[34m" : "";
    const char* RESET  = useColor_ ? "\033[0m"  : "";

    for (const auto& it : items) {
        const char* color = "";
        switch (it.level) {
            case DiagLevel::Error:   color = RED;    break;
            case DiagLevel::Warning: color = YELLOW; break;
            case DiagLevel::Info:    color = BLUE;   break;
        }

        // cabecalho estilo "file:line:col: level: msg"
        if (!it.file.empty())
            std::cerr << it.file << ":" << it.line << ":" << it.col << ": ";
        else
            std::cerr << it.line << ":" << it.col << ": ";

        std::cerr << color << levelStr(it.level) << RESET << ": " << it.msg << "\n";

        // linha de código + caret opcional
        if (!it.lineText.empty()) {
            std::cerr << "  " << it.lineText << "\n";
            if (it.col > 0) {
                std::cerr << "  ";
                for (int i = 1; i < it.col; ++i) std::cerr << ' ';
                std::cerr << color << "^\n" << RESET;
            }
        }
    }

    // Se estourou o limite de erros, avisa
    int errCount = 0;
    for (const auto& it : items) if (it.level == DiagLevel::Error) ++errCount;
    if (errCount >= maxErrors_) {
        std::cerr << RED << "(interrompido apos " << maxErrors_
                  << " erros)" << RESET << "\n";
    }
}

void Diag::clear() {
    items.clear();
    hadError = false;
}

} // namespace mycc