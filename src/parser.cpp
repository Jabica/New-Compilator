#include "parser.hpp"
#include <cstdlib>

namespace mycc {

// util de consumo/expectativa
bool Parser::match(TokenKind k){
    if (peek().kind == k){ pos++; return true; }
    return false;
}

const Token& Parser::expect(TokenKind k, const char* what){
    if (peek().kind != k){
        diag.error(peek().line, peek().col, std::string("esperado '") + what + "', encontrado '" + peek().lexeme + "'");
        return toks[pos < toks.size()? pos : toks.size()-1];
    }
    return toks[pos++];
}

// programa := { funcao }
std::unique_ptr<Program> Parser::parse(){
    auto prog = std::make_unique<Program>();
    while (!atEnd()){
        if (peek().kind == TokenKind::KwFuncao){
            auto f = parseFuncDecl();
            if (f) prog->funcs.push_back(std::move(f));
            else break;
        } else {
            diag.error(peek().line, peek().col, "esperado 'funcao' no nivel superior");
            break;
        }
    }
    return prog;
}

// funcao IDENT '(' [params] ')' ':' tipo bloco
std::unique_ptr<FuncDecl> Parser::parseFuncDecl(){
    expect(TokenKind::KwFuncao, "funcao");
    auto nameTok = expect(TokenKind::Identifier, "nome da funcao");
    auto fn = std::make_unique<FuncDecl>();
    fn->name = nameTok.lexeme;

    expect(TokenKind::LParen, "(");
    fn->params = parseParamsOpt();
    expect(TokenKind::RParen, ")");

    expect(TokenKind::Colon, ":");
    fn->ret = parseType();
    fn->body = parseBlock();
    return fn;
}

// parametros: IDENT ':' tipo {',' IDENT ':' tipo}
std::vector<Param> Parser::parseParamsOpt(){
    std::vector<Param> ps;
    if (peek().kind == TokenKind::RParen) return ps;
    while (true){
        auto nameTok = expect(TokenKind::Identifier, "nome do parametro");
        expect(TokenKind::Colon, ":");
        Param p; p.name = nameTok.lexeme; p.type = parseType();
        ps.push_back(p);
        if (!match(TokenKind::Comma)) break;
    }
    return ps;
}

// tipo = inteiro [ '[' INT ']' ]* | logico [ '[' INT ']' ]* | vazio
Type Parser::parseType(){
    Type base;
    if (match(TokenKind::KwInteiro)) base = Type::inteiro();
    else if (match(TokenKind::KwLogico)) base = Type::logico();
    else if (match(TokenKind::KwVazio))  base = Type::vazio();
    else {
        diag.error(peek().line, peek().col, "tipo invalido (esperado: inteiro|logico|vazio)");
        return Type::inteiro();
    }

    // zera e consome sufixos [INT] [INT] ...
    pendingArrayLen = 0;
    while (peek().kind == TokenKind::LBracket) {
        pos++; // '['
        if (peek().kind != TokenKind::IntLiteral) {
            diag.error(peek().line, peek().col, "esperado tamanho inteiro dentro de '[]'");
            while (!atEnd() && peek().kind != TokenKind::RBracket && peek().kind != TokenKind::Semicolon) pos++;
        } else {
            pendingArrayLen = std::atoi(peek().lexeme.c_str());
            pos++; // INT
        }
        expect(TokenKind::RBracket, "]");
    }

    // aplica no Type (suportamos 1D por enquanto: usa o último)
    if (pendingArrayLen > 0) base.arrayLen = pendingArrayLen;
    return base;
}

// bloco = '{' { stmt } '}'
std::unique_ptr<Block> Parser::parseBlock(){
    expect(TokenKind::LBrace, "{");
    auto b = std::make_unique<Block>();
    while (peek().kind != TokenKind::RBrace && !atEnd()){
        b->stmts.push_back(parseStmt());
    }
    expect(TokenKind::RBrace, "}");
    return b;
}

// stmt
//  = 'variavel' IDENT ':' tipo [ '=' expr ] ';'
//  | IDENT '=' expr ';'
//  | 'retorna' [expr] ';'
//  | se '(' expr ')' bloco [ 'senao' bloco ]
//  | enquanto '(' expr ')' bloco
//  | bloco
std::unique_ptr<Stmt> Parser::parseStmt(){
    // declaracao de variavel
    if (peek().kind == TokenKind::KwVariavel){
        pos++; // consome 'variavel'
        auto nameTok = expect(TokenKind::Identifier, "identificador");
        expect(TokenKind::Colon, ":");
        auto ty = parseType();

        std::unique_ptr<Expr> init;
        if (match(TokenKind::Assign)){
            init = parseExpr();
        }
        expect(TokenKind::Semicolon, ";");
        auto v = std::make_unique<VarDecl>();
        v->name = nameTok.lexeme; v->type = ty; v->init = std::move(init);
        return v;
    }

    // retorno
    if (peek().kind == TokenKind::KwRetorna){
        pos++; // consome 'retorna'
        std::unique_ptr<Expr> e;
        if (peek().kind != TokenKind::Semicolon){
            e = parseExpr();
        }
        expect(TokenKind::Semicolon, ";");
        auto r = std::make_unique<ReturnStmt>();
        r->value = std::move(e);
        return r;
    }

    // bloco aninhado
    if (peek().kind == TokenKind::LBrace){
        return parseBlock();
    }

    // atribuicao (IDENT '=' ...)
    if (peek().kind == TokenKind::Identifier && peek(1).kind == TokenKind::Assign) {
        return parseAssignment();
    }

    // comando como expressao generica: expr ['=' expr] [';']
    // cobre: chamadas, exprs puras e atribuicoes como v[i] = x
    if (peek().kind != TokenKind::KwSe &&
        peek().kind != TokenKind::KwEnquanto &&
        peek().kind != TokenKind::LBrace) {

        auto lhs = parseExpr();

        // --- se depois da expr vier '=', trate como atribuicao ---
        if (match(TokenKind::Assign)) {
            auto rhs = parseExpr();

            // Consome ';' se houver; senao, aceita implicito em contextos seguros
            if (!match(TokenKind::Semicolon)) {
                TokenKind k = peek().kind;
                bool nextStartsStmt =
                    atEnd() ||
                    k == TokenKind::RBrace ||
                    k == TokenKind::KwSe ||
                    k == TokenKind::KwEnquanto ||
                    k == TokenKind::KwVariavel ||
                    k == TokenKind::KwRetorna ||
                    // outra atribuicao logo em seguida: IDENT '='
                    (k == TokenKind::Identifier && peek(1).kind == TokenKind::Assign) ||
                    // chamada logo em seguida: IDENT '('
                    (k == TokenKind::Identifier && peek(1).kind == TokenKind::LParen);

                if (!nextStartsStmt) {
                    diag.error(peek().line, peek().col, "esperado ';' apos a expressao");
                    while (!atEnd() && peek().kind != TokenKind::Semicolon && peek().kind != TokenKind::RBrace) pos++;
                    match(TokenKind::Semicolon);
                }
            }

            // Se o lvalue for um identificador simples, crie AssignStmt normal
            if (auto vr = dynamic_cast<VarRef*>(lhs.get())) {
                auto a = std::make_unique<AssignStmt>();
                a->name = vr->name;
                a->value = std::move(rhs);
                return a;
            }

            // Se o lvalue for uma indexação (Index), crie AssignIndex
            if (auto ix = dynamic_cast<Index*>(lhs.get())) {
                // Tomamos posse dos filhos de Index
                std::unique_ptr<Expr> baseExpr;
                std::unique_ptr<Expr> idxExpr;
                {
                    Index* raw = static_cast<Index*>(lhs.release());
                    baseExpr = std::move(raw->base);
                    idxExpr  = std::move(raw->idx);
                    delete raw;
                }
                auto a = std::make_unique<AssignIndex>(std::move(baseExpr), std::move(idxExpr), std::move(rhs));
                return a;
            }

            // Caso contrário, degrade para ExprStmt do RHS (fallback)
            return std::make_unique<ExprStmt>(std::move(rhs));
        }

        // Caso padrao: exige ';' OU aceita implicito se for chamada
        if (match(TokenKind::Semicolon)) {
            return std::make_unique<ExprStmt>(std::move(lhs));
        }

        if (dynamic_cast<Call*>(lhs.get()) != nullptr) {
            return std::make_unique<ExprStmt>(std::move(lhs));
        }

        // Se chegou aqui, faltou ';' mesmo
        diag.error(peek().line, peek().col, "esperado ';' apos a expressao");
        while (!atEnd() && peek().kind != TokenKind::Semicolon && peek().kind != TokenKind::RBrace) pos++;
        match(TokenKind::Semicolon);
        return std::make_unique<Block>();
    }

    // controle de fluxo
    if (peek().kind == TokenKind::KwSe){
        return parseIf();
    }
    if (peek().kind == TokenKind::KwEnquanto){
        return parseWhile();
    }

    // fallback mínimo
    diag.warn(peek().line, peek().col, "comando nao suportado ainda; ignorando ate ';'");
    while (!atEnd() && peek().kind != TokenKind::Semicolon) pos++;
    match(TokenKind::Semicolon);
    return std::make_unique<Block>(); // no-op
}

// se '(' expr ')' bloco [ 'senao' bloco ]
std::unique_ptr<Stmt> Parser::parseIf(){
    expect(TokenKind::KwSe, "se");
    expect(TokenKind::LParen, "(");
    auto cond = parseExpr();
    expect(TokenKind::RParen, ")");
    auto thenB = parseBlock();

    std::unique_ptr<Block> elseB;
    if (match(TokenKind::KwSenao)) {
        elseB = parseBlock();
    }
    auto node = std::make_unique<IfStmt>();
    node->cond = std::move(cond);
    node->thenBlk = std::move(thenB);
    node->elseBlk = std::move(elseB);
    return node;
}

// enquanto '(' expr ')' bloco
std::unique_ptr<Stmt> Parser::parseWhile(){
    expect(TokenKind::KwEnquanto, "enquanto");
    expect(TokenKind::LParen, "(");
    auto cond = parseExpr();
    expect(TokenKind::RParen, ")");
    auto body = parseBlock();

    auto node = std::make_unique<WhileStmt>();
    node->cond = std::move(cond);
    node->body = std::move(body);
    return node;
}

// IDENT '=' expr ';'       (agora ';' pode ser implícito em contextos seguros)
std::unique_ptr<Stmt> Parser::parseAssignment(){
    auto nameTok = expect(TokenKind::Identifier, "identificador");
    expect(TokenKind::Assign, "=");
    auto rhs = parseExpr();

    // Tenta consumir ';' normalmente
    if (match(TokenKind::Semicolon)) {
        auto a = std::make_unique<AssignStmt>();
        a->name = nameTok.lexeme;
        a->value = std::move(rhs);
        return a;
    }

    // Aceita ';' implícito quando próximo token começa um novo statement
    TokenKind k = peek().kind;
    bool nextStartsStmt =
        atEnd() ||
        k == TokenKind::RBrace ||
        k == TokenKind::KwSe ||
        k == TokenKind::KwEnquanto ||
        k == TokenKind::KwVariavel ||
        k == TokenKind::KwRetorna ||
        // outra atribuicao logo em seguida: IDENT '='
        (k == TokenKind::Identifier && peek(1).kind == TokenKind::Assign) ||
        // chamada logo em seguida: IDENT '('
        (k == TokenKind::Identifier && peek(1).kind == TokenKind::LParen);

    if (nextStartsStmt) {
        auto a = std::make_unique<AssignStmt>();
        a->name = nameTok.lexeme;
        a->value = std::move(rhs);
        return a;
    }

    // Caso contrário, é realmente falta de ';'
    diag.error(peek().line, peek().col, "esperado ';' apos a expressao");
    // sincroniza até um ponto seguro
    while (!atEnd() && peek().kind != TokenKind::Semicolon && peek().kind != TokenKind::RBrace) pos++;
    match(TokenKind::Semicolon);

    auto a = std::make_unique<AssignStmt>();
    a->name = nameTok.lexeme;
    a->value = std::move(rhs);
    return a;
}

// ==========================
// EXPRESSÕES COM PRECEDÊNCIA
// ==========================

static bool isEqualityOp(TokenKind k) {
    return k == TokenKind::EqEq || k == TokenKind::BangEq;
}
static bool isComparisonOp(TokenKind k) {
    return k == TokenKind::Lt || k == TokenKind::Gt || k == TokenKind::Le || k == TokenKind::Ge;
}
static bool isTermOp(TokenKind k) {
    return k == TokenKind::Plus || k == TokenKind::Minus;
}
static bool isFactorOp(TokenKind k) {
    return k == TokenKind::Star || k == TokenKind::Slash || k == TokenKind::Percent;
}

std::unique_ptr<Expr> Parser::parseExpr(){
    return parseEquality();
}

std::unique_ptr<Expr> Parser::parseEquality(){
    auto lhs = parseComparison();
    while (isEqualityOp(peek().kind)) {
        std::string op = peek().lexeme; pos++;
        auto rhs = parseComparison();
        lhs = std::make_unique<Binary>(std::move(lhs), op, std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<Expr> Parser::parseComparison(){
    auto lhs = parseTerm();
    while (isComparisonOp(peek().kind)) {
        std::string op = peek().lexeme; pos++;
        auto rhs = parseTerm();
        lhs = std::make_unique<Binary>(std::move(lhs), op, std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<Expr> Parser::parseTerm(){
    auto lhs = parseFactor();
    while (isTermOp(peek().kind)) {
        std::string op = peek().lexeme; pos++;
        auto rhs = parseFactor();
        lhs = std::make_unique<Binary>(std::move(lhs), op, std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<Expr> Parser::parseFactor(){
    auto lhs = parseUnary();
    while (isFactorOp(peek().kind)) {
        std::string op = peek().lexeme; pos++;
        auto rhs = parseUnary();
        lhs = std::make_unique<Binary>(std::move(lhs), op, std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<Expr> Parser::parseUnary(){
    if (peek().kind == TokenKind::Bang || peek().kind == TokenKind::Minus) {
        std::string op = peek().lexeme; pos++;
        auto rhs = parseUnary();
        return std::make_unique<Unary>(op, std::move(rhs));
    }
    return parsePrimary();
}

// primário: INT | IDENT | '(' expr ')' | verdadeiro | falso
std::unique_ptr<Expr> Parser::parsePrimary(){
    // literal inteiro
    if (peek().kind == TokenKind::IntLiteral){
        int v = std::atoi(peek().lexeme.c_str());
        pos++;
        return std::make_unique<IntLit>(v);
    }
    // identificador (variável, chamada de função e/ou indexação de vetor)
    if (peek().kind == TokenKind::Identifier){
        std::string name = peek().lexeme;
        pos++; // consome IDENT

        // Base: ou uma chamada (IDENT '(' args ')') ou apenas referência (IDENT)
        std::unique_ptr<Expr> base;
        if (peek().kind == TokenKind::LParen) {
            // chamada de funcao
            pos++; // '('
            std::vector<std::unique_ptr<Expr>> args;
            if (peek().kind != TokenKind::RParen) {
                while (true) {
                    args.push_back(parseExpr());
                    if (!match(TokenKind::Comma)) break;
                }
            }
            expect(TokenKind::RParen, ")");
            base = std::make_unique<Call>(std::move(name), std::move(args));
        } else {
            // referencia simples
            base = std::make_unique<VarRef>(name);
        }

        // Sufixos: uma ou mais indexações [expr]
        while (peek().kind == TokenKind::LBracket) {
            pos++; // '['
            auto idx = parseExpr();
            expect(TokenKind::RBracket, "]");
            base = std::make_unique<Index>(std::move(base), std::move(idx));
        }

        return base;
    }
    // ( expressao )
    if (peek().kind == TokenKind::LParen){
        pos++; // '('
        auto e = parseExpr();
        expect(TokenKind::RParen, ")");
        return e;
    }
    // booleanos (placeholders como inteiros 1/0)
    if (peek().kind == TokenKind::KwVerdadeiro){ pos++; return std::make_unique<IntLit>(1); }
    if (peek().kind == TokenKind::KwFalso){ pos++; return std::make_unique<IntLit>(0); }

    diag.error(peek().line, peek().col, "expressao primaria invalida");
    return std::make_unique<IntLit>(0);
}

} // namespace mycc