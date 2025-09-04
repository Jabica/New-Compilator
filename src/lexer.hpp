#pragma once
#include "token.hpp"
#include "diagnostics.hpp"
#include <vector>
#include <string>
#include <cctype>
#include <unordered_map>

namespace mycc {

class Lexer {
public:
    Lexer(const std::string& src, Diag& d) : text(src), diag(d) {}
    std::vector<Token> tokenize();

    // --- [ADICIONADO] helpers de inspeção (úteis no lexer.cpp) ---
    bool atEnd() const { return i >= text.size(); }
    char peekNext() const { return (i + 1 < text.size()) ? text[i + 1] : '\0'; }
    int currentLine() const { return line; }
    int currentCol() const { return col; }

private:
    const std::string& text;
    Diag& diag;
    size_t i = 0;
    int line = 1, col = 1;

    char peek() const { return i < text.size() ? text[i] : '\0'; }
    char get() {
        char c = peek();
        if (c == '\0') return c;
        i++;
        if (c == '\n') { line++; col = 1; }
        else { col++; }
        return c;
    }

    void skipSpacesAndComments();

    Token makeToken(TokenKind k, const std::string& lex, int l, int c) {
        return Token{ k, lex, l, c };
    }

    // --- [NOVO] helpers para arrays ---
    Token makeLBracket(int l, int c) {
        return Token{ TokenKind::LBracket, "[", l, c };
    }
    Token makeRBracket(int l, int c) {
        return Token{ TokenKind::RBracket, "]", l, c };
    }
};

} // namespace mycc