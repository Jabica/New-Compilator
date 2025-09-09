#pragma once
#include "token.hpp"
#include "ast.hpp"
#include "diagnostics.hpp"
#include <vector>
#include <memory>

namespace mycc {

class Parser {
public:
    Parser(const std::vector<Token>& t, Diag& d, std::string filename="")
        : toks(t), diag(d), filename(std::move(filename)) {}
    std::unique_ptr<Program> parse();

    // (Opcional) acessores para o tamanho de array lido por último em parseType()
    int  pendingArray() const { return pendingArrayLen; }
    void clearPendingArray()  { pendingArrayLen = 0; }
    // Se preferir consumir e zerar de uma vez:
    int  takePendingArray()   { int n = pendingArrayLen; pendingArrayLen = 0; return n; }
    std::unique_ptr<Expr> takePendingArrayLenExpr(){ return std::move(pendingArrayLenExpr); }
    std::vector<std::unique_ptr<Expr>> takePendingArrayLenList(){ return std::move(pendingArrayLenList); }

private:
    const std::vector<Token>& toks;
    Diag& diag;
    size_t pos = 0;
    std::string filename;

    // Guarda tamanhos de array lidos em parseType() (ND)
    int pendingArrayLen = 0; // legado 1D
    std::unique_ptr<Expr> pendingArrayLenExpr; // legado 1D
    std::vector<std::unique_ptr<Expr>> pendingArrayLenList; // R2-07: ND

    const Token& peek(size_t off=0) const {
        static Token eof{TokenKind::End,"",0,0};
        return pos+off < toks.size()? toks[pos+off] : eof;
    }
    bool match(TokenKind k);
    const Token& expect(TokenKind k, const char* what);
    bool atEnd() const { return peek().kind == TokenKind::End; }

    // Regras (declarações/estatements)
    std::unique_ptr<FuncDecl> parseFuncDecl();
    std::unique_ptr<VarDecl>  parseGlobalDecl();
    std::unique_ptr<Expr>     parseLiteralExpr();
    std::vector<Param> parseParamsOpt();
    Type parseType();
    std::unique_ptr<Block> parseBlock();
    std::unique_ptr<Stmt> parseStmt();
    std::unique_ptr<Stmt> parseIf();        // se (expr) bloco [senao bloco]
    std::unique_ptr<Stmt> parseWhile();     // enquanto (expr) bloco
    std::unique_ptr<Stmt> parseFor();       // for (init; cond; step) bloco
    std::unique_ptr<Stmt> parseBreak();
    std::unique_ptr<Stmt> parseContinue();
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

inline SourceLoc LFrom(const Token& t, const std::string& file) {
    return SourceLoc(file, (unsigned)t.line, (unsigned)t.col);
}

} // namespace mycc
