#pragma once
#include <string>
#include <iostream>

namespace mycc {

struct Diag {
    std::string file;
    bool hadError = false;

    explicit Diag(std::string fileName) : file(std::move(fileName)) {}

    void error(int line, int col, const std::string& msg) {
        hadError = true;
        std::cerr << file << ":" << line << ":" << col << ": error: " << msg << "\n";
    }

    void warning(int line, int col, const std::string& msg) {
        std::cerr << file << ":" << line << ":" << col << ": warning: " << msg << "\n";
    }
};

} // namespace mycc