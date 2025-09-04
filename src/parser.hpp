#pragma once
#include "token.hpp"
#include "ast.hpp"
#include "diagnostics.hpp"
#include <vector>
#include <memory>

namespace mycc {

class Parser {
public:
    Parser(const std::vector<Token>& t, Diag& d): toks(t), diag(d) {}
    std::unique_ptr<Program> parse();

    // (Opcional) acessores para o tamanho de array lido por último em parseType()
    int  pendingArray() const { return pendingArrayLen; }
    void clearPendingArray()  { pendingArrayLen = 0; }
    // Se preferir consumir e zerar de uma vez:
    int  takePendingArray()   { int n = pendingArrayLen; pendingArrayLen = 0; return n; }

private:
    const std::vector<Token>& toks;
    Diag& diag;
    size_t pos = 0;

    // Guarda o último tamanho de array lido em parseType() (0 = escalar)
    int pendingArrayLen = 0; // <- NOVO

    const Token& peek(size_t off=0) const {
        static Token eof{TokenKind::End,"",0,0};
        return pos+off < toks.size()? toks[pos+off] : eof;
    }
    bool match(TokenKind k);
    const Token& expect(TokenKind k, const char* what);
    bool atEnd() const { return peek().kind == TokenKind::End; }

    // Regras (declarações/estatements)
    std::unique_ptr<FuncDecl> parseFuncDecl();
    std::vector<Param> parseParamsOpt();
    Type parseType();
    std::unique_ptr<Block> parseBlock();
    std::unique_ptr<Stmt> parseStmt();
    std::unique_ptr<Stmt> parseIf();        // se (expr) bloco [senao bloco]
    std::unique_ptr<Stmt> parseWhile();     // enquanto (expr) bloco
    std::unique_ptr<Stmt> parseAssignment(); // IDENT '=' expr ';'

    // Expressões com precedência
    std::unique_ptr<Expr> parseExpr();        // = parseEquality()
    std::unique_ptr<Expr> parseEquality();    // == !=
    std::unique_ptr<Expr> parseComparison();  // < > <= >=
    std::unique_ptr<Expr> parseTerm();        // + -
    std::unique_ptr<Expr> parseFactor();      // * / %
    std::unique_ptr<Expr> parseUnary();       // ! -
    std::unique_ptr<Expr> parsePrimary();
};

} // namespace mycc