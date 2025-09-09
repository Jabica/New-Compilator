#pragma once
#include "ast.hpp"
#include "diagnostics.hpp"
#include <unordered_map>
#include <string>
#include <vector>
#include <algorithm>
#include <unordered_set>

namespace mycc {

// Conversões simples (sem info de array no Type ainda)
inline bool isImplicitlyConvertible(const Type& from, const Type& to) {
    // Arrays precisam casar exatamente (tipo base e tamanho). Nada de conversão implícita envolvendo arrays.
    if (from.isArray() || to.isArray()) {
        return from.isArray() && to.isArray() &&
               from.kind == to.kind &&
               from.arrayLen == to.arrayLen;
    }

    // escalares: mesmo tipo OK
    if (from.kind == to.kind) return true;

    // promoção permitida apenas: Logico -> Inteiro (escalares)
    if (from.kind == Type::Logico && to.kind == Type::Inteiro) return true;

    // Texto não é conversível implicitamente
    if (from.kind == Type::Texto || to.kind == Type::Texto) return false;

    return false;
}

// Nome da função chamado em Call (seu AST usa `callee`)
inline std::string getCallName(const Call& c) {
    return c.callee;
}


static inline bool isIntLiteral01(Expr* e) {
    if (auto lit = dynamic_cast<IntLit*>(e)) return lit->value == 0 || lit->value == 1;
    if (auto bl  = dynamic_cast<BoolLit*>(e)) return true;
    return false;
}

// Avalia (recursivamente) se a expressão é uma constante inteira (literal ou expressão aritmética constante)
static inline bool tryEvalIntConst(Expr* e, long long& out) {
    // Literal inteiro
    if (auto lit = dynamic_cast<IntLit*>(e)) { out = lit->value; return true; }
    if (auto bl  = dynamic_cast<BoolLit*>(e)) { out = bl->value?1:0; return true; }

    // Unário '-'
    if (auto u = dynamic_cast<Unary*>(e)) {
        long long v;
        if (!tryEvalIntConst(u->rhs.get(), v)) return false;
        if (u->op == "-") { out = -v; return true; }
        // operador '!' não é inteiro aqui; deixe o checker normal lidar
        return false;
    }

    // Binários aritméticos de inteiros com ambos operandos constantes
    if (auto b = dynamic_cast<Binary*>(e)) {
        long long L, R;
        if (!tryEvalIntConst(b->lhs.get(), L)) return false;
        if (!tryEvalIntConst(b->rhs.get(), R)) return false;
        if      (b->op == "+") { out = L + R; return true; }
        else if (b->op == "-") { out = L - R; return true; }
        else if (b->op == "*") { out = L * R; return true; }
        else if (b->op == "/") { if (R == 0) return false; out = L / R; return true; }
        else if (b->op == "%") { if (R == 0) return false; out = L % R; return true; }
        // Comparações produzem lógico no seu tipo system; não tratar aqui
        return false;
    }

    // Qualquer outra expressão não é constante inteira pura
    return false;
}

static inline bool isIntConst01(Expr* e) {
    long long v; if (!tryEvalIntConst(e, v)) return false; return v == 0 || v == 1;
}

struct Scope {
    std::unordered_map<std::string, Type> vars;
    // Marca inteiros que sabemos ser 0/1 (ex.: inicializados com 0/1 ou recebendo booleanos)
    std::unordered_map<std::string, bool> boolLike;
    // Patch 16: marca constantes (verdadeiro se o símbolo é const neste escopo)
    std::unordered_map<std::string, bool> isConst;
    // R2-07: dimensões de arrays por símbolo
    std::unordered_map<std::string, std::vector<int>> dims;
    Scope* parent = nullptr;

    explicit Scope(Scope* p=nullptr) : parent(p) {}

    bool declare(const std::string& name, const Type& ty){
        bool inserted = vars.emplace(name, ty).second;
        if (inserted) {
            // reset flag local ao declarar
            boolLike.erase(name);
            isConst[name] = false;
            dims.erase(name);
        }
        return inserted;
    }

    const Type* lookup(const std::string& name) const{
        for (auto s=this; s; s=s->parent){
            auto it = s->vars.find(name);
            if (it != s->vars.end()) return &it->second;
        }
        return nullptr;
    }

    // Dimensões: set/get no escopo local ou no que declarou
    void setDimsHere(const std::string& name, const std::vector<int>& v){ dims[name] = v; }
    void setDimsWhereDeclared(const std::string& name, const std::vector<int>& v){
        for (auto s=this; s; s=s->parent){
            auto it = s->vars.find(name);
            if (it != s->vars.end()) { s->dims[name] = v; return; }
        }
        dims[name] = v;
    }
    std::vector<int> getDims(const std::string& name) const{
        for (auto s=this; s; s=s->parent){
            auto it = s->dims.find(name);
            if (it != s->dims.end()) return it->second;
        }
        return {};
    }

    // Lê a flag "boolLike" procurando no escopo atual e ancestrais
    bool getBoolLike(const std::string& name) const {
        for (auto s=this; s; s=s->parent){
            auto it = s->boolLike.find(name);
            if (it != s->boolLike.end()) return it->second;
        }
        return false;
    }

    // Define a flag "boolLike" SOMENTE neste escopo (útil para VarDecl local)
    void setBoolLikeHere(const std::string& name, bool v){
        boolLike[name] = v;
    }

    // Define a flag "boolLike" no escopo onde a variável foi declarada
    void setBoolLikeWhereDeclared(const std::string& name, bool v){
        for (auto s=this; s; s=s->parent){
            auto it = s->vars.find(name);
            if (it != s->vars.end()){
                s->boolLike[name] = v;
                return;
            }
        }
        // fallback: se não achar, grava aqui mesmo
        boolLike[name] = v;
    }

    // Define const no escopo onde a variável foi declarada
    void setConstWhereDeclared(const std::string& name, bool v){
        for (auto s=this; s; s=s->parent){
            auto it = s->vars.find(name);
            if (it != s->vars.end()){
                s->isConst[name] = v;
                return;
            }
        }
        isConst[name] = v;
    }
    bool getConst(const std::string& name) const{
        for (auto s=this; s; s=s->parent){
            auto it = s->isConst.find(name);
            if (it != s->isConst.end()) return it->second;
            // If name not present in this scope's map, continue searching
        }
        return false;
    }
};

struct FuncSig {
    Type ret;
    std::vector<Type> params;
    std::vector<std::vector<int>> paramDims; // R2-08: dims por parâmetro (vazia => escalar)
};

class SemanticChecker {
public:
    explicit SemanticChecker(Diag& d) : diag(d) {}

    bool run(Program* prog){
        funcs.clear();
        globals.clear();
        seedBuiltins();     // registra built-ins
        checkGlobals(prog); // valida globais e preenche tabela
        collectFuncs(prog); // registra funções do usuário + detecta redefinição

        bool ok = true;
        for (auto& fptr : prog->funcs) ok &= checkFunc(fptr.get());
        return ok && !diag.hadError;
    }

private:
    Diag& diag;
    int loopDepth = 0;
    int switchDepth = 0; // R2-03
    std::vector<int> caseArmsRemaining; // R2-03
    int insideCase = 0; // R2-03
    std::unordered_map<std::string, FuncSig> funcs;
    std::unordered_map<std::string, Type> globals;
    std::unordered_map<std::string, bool> globalsConst;
    std::unordered_map<std::string, std::vector<int>> globalsDims; // R2-07
    struct ConstVal { bool isConst=false; Type ty=Type::inteiro(); long long i=0; };
    std::unordered_map<std::string, ConstVal> constScalars; // apenas globais const escalares
    std::unordered_map<std::string, std::vector<long long>> constArrays; // globais const vetores (valores normalizados)

    // Avaliador de const-expr
    ConstVal toBool(ConstVal x){
        if (x.ty.kind == Type::Inteiro) { x.ty = Type::logico(); x.i = (x.i!=0)?1:0; return x; }
        if (x.ty.kind == Type::Logico) { x.i = (x.i!=0)?1:0; return x; }
        return ConstVal{};
    }
    ConstVal toInt(ConstVal x){
        if (x.ty.kind == Type::Logico) { x.ty = Type::inteiro(); x.i = (x.i!=0)?1:0; return x; }
        if (x.ty.kind == Type::Inteiro) { return x; }
        return ConstVal{};
    }
    ConstVal evalConst(Expr* e){
        if (auto lit = dynamic_cast<IntLit*>(e)) { return ConstVal{true, Type::inteiro(), (long long)lit->value}; }
        if (auto vr = dynamic_cast<VarRef*>(e)) {
            auto it = constScalars.find(vr->name);
            if (it != constScalars.end()) return it->second;
            return ConstVal{}; // arrays sem index nao sao const-expr
        }
        if (auto bl = dynamic_cast<BoolLit*>(e)) { return ConstVal{true, Type::logico(), bl->value?1:0}; }
        if (auto u = dynamic_cast<Unary*>(e)) {
            ConstVal r = evalConst(u->rhs.get()); if (!r.isConst) return r;
            if (u->op == "-") { r = toInt(r); r.i = -r.i; r.ty = Type::inteiro(); return r; }
            if (u->op == "!") { r = toBool(r); r.i = r.i?0:1; r.ty = Type::logico(); return r; }
            return ConstVal{};
        }
        if (auto b = dynamic_cast<Binary*>(e)) {
            ConstVal L = evalConst(b->lhs.get()); if (!L.isConst) return L;
            ConstVal R = evalConst(b->rhs.get()); if (!R.isConst) return R;
            const std::string& op = b->op;
            if (op=="+"||op=="-"||op=="*"||op=="/"||op=="%"){
                L = toInt(L); R = toInt(R);
                if (op=="+") L.i = L.i + R.i;
                else if (op=="-") L.i = L.i - R.i;
                else if (op=="*") L.i = L.i * R.i;
                else if (op=="/") { if (R.i==0) return ConstVal{}; L.i = L.i / R.i; }
                else if (op=="%") { if (R.i==0) return ConstVal{}; L.i = L.i % R.i; }
                L.ty = Type::inteiro(); return L;
            }
            if (op=="<"||op=="<="||op==">"||op==">="||op=="=="||op=="!="){
                L = toInt(L); R = toInt(R);
                bool res=false;
                if (op=="<") res = L.i < R.i; else if (op=="<=") res = L.i <= R.i; else if (op==">") res = L.i > R.i;
                else if (op==">=") res = L.i >= R.i; else if (op=="==") res = L.i == R.i; else if (op=="!=") res = L.i != R.i;
                return ConstVal{true, Type::logico(), res?1:0};
            }
            if (op=="&&"||op=="||"){
                L = toBool(L);
                if (op=="&&") {
                    if (L.i==0) return ConstVal{true, Type::logico(), 0};
                    R = toBool(R); return ConstVal{true, Type::logico(), (R.i!=0)?1:0};
                } else {
                    if (L.i!=0) return ConstVal{true, Type::logico(), 1};
                    R = toBool(R); return ConstVal{true, Type::logico(), (R.i!=0)?1:0};
                }
            }
            return ConstVal{};
        }
        if (auto ix = dynamic_cast<Index*>(e)) {
            auto br = dynamic_cast<VarRef*>(ix->base.get()); if (!br) return ConstVal{};
            auto itA = constArrays.find(br->name); if (itA==constArrays.end()) return ConstVal{};
            ConstVal ci = evalConst(ix->idx.get()); if (!ci.isConst) return ConstVal{}; ci = toInt(ci);
            if (ci.i < 0 || (size_t)ci.i >= itA->second.size()) return ConstVal{};
            // tipo do elemento desconhecido aqui; coerção depois
            return ConstVal{true, Type::inteiro(), itA->second[(size_t)ci.i]};
        }
        // call or others: not const
        return ConstVal{};
    }

    // Built-ins mínimos (lado semântico)
    void seedBuiltins() {
        // printi(int), printb(logico)
        funcs["printi"] = FuncSig{ Type::vazio(), { Type::inteiro() } };
        funcs["printb"] = FuncSig{ Type::vazio(), { Type::logico()  } };
        funcs["prints"] = FuncSig{ Type::vazio(), { Type::texto()   } };
    }

    void collectFuncs(Program* prog){
        for (auto& fptr : prog->funcs){
            FuncSig sig; sig.ret = fptr->ret;
            sig.params.reserve(fptr->params.size());
            sig.paramDims.resize(fptr->params.size());
            for (auto& p : fptr->params) sig.params.push_back(p.type);

            // R2-08: resolve dims dos parâmetros (const-expr > 0)
            for (size_t i=0;i<fptr->params.size();++i){
                auto& P = fptr->params[i];
                if (!P.arrayDimsExpr.empty()){
                    P.arrayDims.clear();
                    for (auto& ex : P.arrayDimsExpr){
                        auto cv = evalConst(ex.get());
                        if (!cv.isConst || cv.ty.kind != Type::Inteiro){
                            diag.error(0,0, "tamanho de parametro array deve ser const-expr em '" + fptr->name + "'");
                            continue;
                        }
                        if (cv.i < 1 || cv.i > INT_MAX){
                            diag.error(0,0, "tamanho de parametro array invalido (>=1) em '" + fptr->name + "'");
                            continue;
                        }
                        P.arrayDims.push_back((int)cv.i);
                    }
                }
                sig.paramDims[i] = P.arrayDims;
            }

            // Proíbe retorno de array por valor (não suportado)
            if (sig.ret.isArray()){
                diag.error(0,0, "retorno de array por valor nao suportado na funcao '" + fptr->name + "'");
            }

            // Redeclaração: erro SEMPRE (mesma ou diferente assinatura)
            auto it = funcs.find(fptr->name);
            if (it != funcs.end()) {
                std::string msg = "redefinicao de funcao '" + fptr->name + "'";
                if (it->second.params.size() != sig.params.size() ||
                    it->second.ret.kind != sig.ret.kind ||
                    it->second.ret.arrayLen != sig.ret.arrayLen) {
                    msg += " com assinatura diferente";
                } else {
                    bool same = true;
                    for (size_t i=0;i<sig.params.size();++i){
                        same &= (sig.params[i].kind == it->second.params[i].kind) &&
                                (sig.params[i].arrayLen == it->second.params[i].arrayLen);
                    }
                    if (same) msg += " (mesma assinatura)";
                    else      msg += " com assinatura diferente";
                }
                diag.error(0,0, msg);
                // mantém a primeira assinatura
                continue;
            }

            funcs.emplace(fptr->name, std::move(sig));
        }
    }

    void checkGlobals(Program* prog){
        for (auto& gptr : prog->globals) {
            VarDecl* g = gptr.get();
            // Resolve dimensões (ND) via const-expr, se presentes
            if (!g->arrayDimsExpr.empty()) {
                g->arrayDims.clear();
                long long total = 1;
                for (auto& ex : g->arrayDimsExpr) {
                    auto cv = evalConst(ex.get());
                    if (!cv.isConst || cv.ty.kind != Type::Inteiro) {
                        diag.error(0,0, "tamanho de vetor deve ser const-expr em '" + g->name + "'");
                        continue;
                    }
                    if (cv.i < 1 || cv.i > INT_MAX) {
                        diag.error(0,0, "tamanho de vetor invalido (>=1) em '" + g->name + "'");
                        continue;
                    }
                    g->arrayDims.push_back((int)cv.i);
                    total *= cv.i;
                }
                if (!g->arrayDims.empty()) {
                    g->arrayLen = (int)total;
                    g->type.arrayLen = g->arrayLen;
                }
            } else if (g->arrayLenExpr) {
                // Legado 1D
                auto cv = evalConst(g->arrayLenExpr.get());
                if (!cv.isConst || cv.ty.kind != Type::Inteiro) {
                    diag.error(0,0, "tamanho de vetor deve ser const-expr em '" + g->name + "'");
                } else {
                    long long val = cv.i;
                    if (val < 1 || val > INT_MAX) {
                        diag.error(0,0, "tamanho de vetor invalido (>=1) em '" + g->name + "'");
                    } else {
                        g->arrayLen = (int)val;
                        g->type.arrayLen = g->arrayLen;
                        g->arrayDims = { g->arrayLen };
                    }
                }
            }
            // Redeclaração de global
            if (globals.find(g->name) != globals.end()) {
                diag.error(0,0, "variavel global redeclarada: " + g->name);
                continue;
            }
            // Tamanho de vetor deve ser literal > 0 (já vem como int literal na gramática)
            if (g->arrayLen < 0) {
                diag.error(0,0, "tamanho de vetor invalido em global: " + g->name);
            }
            // Init: apenas literal simples quando presente
            if (g->arrayLen > 0) {
                // Lista de inicialização com const-expr
                if (!g->initList.empty()) {
                    if ((int)g->initList.size() > g->arrayLen) {
                        diag.error(0,0, "lista de inicializacao maior que o tamanho do vetor em '" + g->name + "'");
                    }
                    g->constInitList.clear();
                    g->constInitList.reserve(g->initList.size());
                    for (auto& elt : g->initList) {
                        auto cv = evalConst(elt.get());
                        if (!cv.isConst) {
                            diag.error(0,0, "init de vetor global deve ser const-expr em '" + g->name + "'");
                            continue;
                        }
                        // cast seguro
                        long long val = cv.i;
                        if (g->type.kind == Type::Logico) {
                            if (cv.ty.kind == Type::Inteiro && !(cv.i==0 || cv.i==1)) {
                                diag.error(0,0, "elemento da lista deve ser booleano (0/1) em '" + g->name + "'");
                            }
                            val = (cv.ty.kind == Type::Logico) ? (cv.i?1:0) : (cv.i!=0?1:0);
                        } else {
                            val = (cv.ty.kind == Type::Logico) ? (cv.i?1:0) : cv.i;
                        }
                        g->constInitList.push_back(val);
                    }
                    // não preenche aqui; padding no codegen
                }
            } else {
                if (g->init) {
                    auto cv = evalConst(g->init.get());
                    if (!cv.isConst) {
                        diag.error(0,0, "init global deve ser const-expr em '" + g->name + "'");
                    } else {
                        long long val = cv.i;
                        if (g->type.kind == Type::Logico) {
                            val = (cv.ty.kind == Type::Logico) ? (cv.i?1:0) : (cv.i!=0?1:0);
                        } else {
                            val = (cv.ty.kind == Type::Logico) ? (cv.i?1:0) : cv.i;
                        }
                        g->hasConstInit = true; g->constInit = val;
                    }
                } else if (g->isConst) {
                    // opcional: exigir init; mantemos zero por padrão
                }
            }
            globals.emplace(g->name, g->type);
            globalsConst.emplace(g->name, g->isConst);
            if (!g->arrayDims.empty()) globalsDims[g->name] = g->arrayDims;
            // salva valor para dependencias entre const
            if (g->isConst) {
                if (g->arrayLen == 0) {
                    ConstVal cv; cv.isConst = true; cv.ty = g->type; cv.i = g->hasConstInit ? g->constInit : 0;
                    constScalars[g->name] = cv;
                } else {
                    // lista (ou zeros)
                    std::vector<long long> vals = g->constInitList;
                    while ((int)vals.size() < g->arrayLen) vals.push_back(0);
                    constArrays[g->name] = std::move(vals);
                }
            }
        }
    }

    // Checa bloco; seta didReturn se todos os caminhos retornam
    bool checkBlock(Block* b, Scope& scope, const Type& currentRet, bool& didReturn){
        bool ok = true;
        Scope local(&scope);
        bool blockReturns = false;

        for (auto& sptr : b->stmts){
            if (blockReturns) {
                // opcional: avisar não-alcançável
                continue;
            }

            if (auto v = dynamic_cast<VarDecl*>(sptr.get())){
                // Resolve dimensões locais (ND) via const-expr
                if (!v->arrayDimsExpr.empty()) {
                    v->arrayDims.clear();
                    long long total = 1;
                    for (auto& ex : v->arrayDimsExpr) {
                        auto cv = evalConst(ex.get());
                        if (!cv.isConst || cv.ty.kind != Type::Inteiro) {
                            diag.error(0,0, "tamanho de vetor deve ser const-expr em '" + v->name + "'");
                            continue;
                        }
                        if (cv.i < 1 || cv.i > INT_MAX) {
                            diag.error(0,0, "tamanho de vetor invalido (>=1) em '" + v->name + "'");
                            continue;
                        }
                        v->arrayDims.push_back((int)cv.i);
                        total *= cv.i;
                    }
                    if (!v->arrayDims.empty()) {
                        v->arrayLen = (int)total;
                        v->type.arrayLen = v->arrayLen;
                    }
                } else if (v->arrayLenExpr) {
                    auto cv = evalConst(v->arrayLenExpr.get());
                    if (!cv.isConst || cv.ty.kind != Type::Inteiro) {
                        diag.error(0,0, "tamanho de vetor deve ser const-expr em '" + v->name + "'");
                    } else {
                        long long val = cv.i;
                        if (val < 1 || val > INT_MAX) {
                            diag.error(0,0, "tamanho de vetor invalido (>=1) em '" + v->name + "'");
                        } else {
                            v->arrayLen = (int)val;
                            v->type.arrayLen = v->arrayLen;
                            v->arrayDims = { v->arrayLen };
                        }
                    }
                }
                if (!local.declare(v->name, v->type)){
                    diag.error(0,0, "variavel redeclarada: " + v->name);
                    ok = false;
                }
                if (!v->arrayDims.empty()) local.setDimsHere(v->name, v->arrayDims);
                if (v->init){
                    Type ti = checkExpr(v->init.get(), local);
                    bool okConv = isImplicitlyConvertible(ti, v->type)
                               || (v->type.kind == Type::Logico && ti.kind == Type::Inteiro && (isIntLiteral01(v->init.get()) || isIntConst01(v->init.get())));
                    if (!okConv){
                        diag.error(0,0, "tipo incompativel na inicializacao de '" + v->name
                                         + "' ("+v->type.str()+" <- "+ti.str()+")");
                        ok = false;
                    }
                }
                // Atualiza a flag "boolLike" para inteiros locais
                if (v->type.kind == Type::Inteiro) {
                    bool like = false;
                    if (v->init) {
                        Type ti2 = checkExpr(v->init.get(), local);
                        if (!ti2.isArray()) {
                            if (ti2.kind == Type::Logico) {
                                like = true;
                            } else if (ti2.kind == Type::Inteiro) {
                                if (isIntLiteral01(v->init.get()) || isIntConst01(v->init.get())) {
                                    like = true;
                                } else if (auto vr = dynamic_cast<VarRef*>(v->init.get())) {
                                    if (local.getBoolLike(vr->name)) {
                                        like = true;
                                    }
                                }
                            }
                        }
                    }
                    local.setBoolLikeHere(v->name, like);
                }
            }
            else if (auto a = dynamic_cast<AssignStmt*>(sptr.get())){
                const Type* tv = local.lookup(a->name);
                if (!tv){
                    diag.error(0,0, "variavel nao declarada: " + a->name);
                    ok = false;
                } else {
                    // Proíbe atribuir a const
                    if (local.getConst(a->name)) {
                        diag.error(0,0, "nao e permitido atribuir a constante: " + a->name);
                        ok = false;
                    }
                    if (tv->isArray()){
                        diag.error(0,0, "atribuicao direta a array nao suportada (use indexacao)");
                        ok = false;
                    }
                }
                Type te = checkExpr(a->value.get(), local);
                bool okConv = tv && ( isImplicitlyConvertible(te, *tv)
                                || (tv->kind == Type::Logico && te.kind == Type::Inteiro && (isIntLiteral01(a->value.get()) || isIntConst01(a->value.get()))));
                if (tv && !okConv){
                    diag.error(0,0, "tipo incompativel na atribuicao de '" + a->name
                                    + "' ("+tv->str()+" <- "+te.str()+")");
                    ok = false;
                }
                // Atualiza a flag "boolLike" em atribuicoes a inteiros
                if (tv && tv->kind == Type::Inteiro) {
                    bool like = false;
                    if (!te.isArray()) {
                        if (te.kind == Type::Logico) {
                            like = true;
                        } else if (te.kind == Type::Inteiro) {
                            if (isIntLiteral01(a->value.get()) || isIntConst01(a->value.get())) {
                                like = true;
                            } else if (auto vr = dynamic_cast<VarRef*>(a->value.get())) {
                                if (local.getBoolLike(vr->name)) {
                                    like = true;
                                }
                            }
                        }
                    }
                    local.setBoolLikeWhereDeclared(a->name, like);
                }
            }
            else if (auto ai = dynamic_cast<AssignIndex*>(sptr.get())){
                // Flatten base indices
                std::vector<Expr*> idxExprs;
                Expr* cur = ai->base.get();
                std::string name;
                while (auto ix = dynamic_cast<Index*>(cur)) { idxExprs.push_back(ix->idx.get()); cur = ix->base.get(); }
                if (auto vr = dynamic_cast<VarRef*>(cur)) { name = vr->name; }
                else { diag.error(0,0,"atribuicao indexada invalida"); ok=false; continue; }
                // append last index from AssignIndex
                idxExprs.push_back(ai->index.get());
                for (auto* ex : idxExprs) {
                    Type ti = checkExpr(ex, local);
                    if (ti.kind != Type::Inteiro || ti.isArray()) { diag.error(0,0, "indice de array deve ser inteiro"); ok=false; }
                }
                // const check
                if (local.getConst(name)) { diag.error(0,0, "nao e permitido atribuir a constante: " + name); ok=false; }
                // dims check
                std::vector<int> dims = local.getDims(name);
                if (dims.empty()) { const Type* tvar = local.lookup(name); if (!tvar || !tvar->isArray()) { diag.error(0,0, "indexacao em tipo nao-array"); ok=false; } }
                else if (idxExprs.size() != dims.size()) { diag.error(0,0, "numero de indices diferente das dimensoes do array"); ok=false; }
                // type compat
                Type tval = checkExpr(ai->value.get(), local);
                const Type* tvar = local.lookup(name);
                if (tvar) {
                    Type elem = *tvar; elem.arrayLen = 0;
                    if (!isImplicitlyConvertible(tval, elem)) {
                        diag.error(0,0, "tipo incompativel na atribuicao ao elemento do array ("+elem.str()+" <- "+tval.str()+")");
                        ok = false;
                    }
                }
            }
            else if (auto r = dynamic_cast<ReturnStmt*>(sptr.get())){
                if (currentRet.kind == Type::Vazio){
                    if (r->value){
                        diag.error(0,0, "retorno com valor em funcao 'vazio'");
                        ok = false;
                    }
                } else {
                    if (!r->value){
                        diag.error(0,0, "retorno sem valor em funcao '" + currentRet.str() + "'");
                        ok = false;
                    } else {
                        Type tr = checkExpr(r->value.get(), local);

                        // Permitir as mesmas conversões dos parâmetros:
                        // - bool -> int (escalares)
                        // - int -> bool SOMENTE se for 0/1 literal/const ou variável marcada como boolLike
                        bool okConv = isImplicitlyConvertible(tr, currentRet);
                        if (!currentRet.isArray() && !tr.isArray()) {
                            if (currentRet.kind == Type::Inteiro && tr.kind == Type::Logico) {
                                okConv = true; // bool -> int
                            } else if (currentRet.kind == Type::Logico && tr.kind == Type::Inteiro) {
                                bool isConvertible = false;
                                long long _tmpConst;
                                if (tryEvalIntConst(r->value.get(), _tmpConst) && (_tmpConst == 0 || _tmpConst == 1)) {
                                    isConvertible = true; // apenas 0/1
                                } else if (auto vr = dynamic_cast<VarRef*>(r->value.get())) {
                                    if (local.getBoolLike(vr->name)) isConvertible = true;
                                }
                                okConv = isConvertible;
                            }
                        }

                        if (!okConv){
                            diag.error(0,0, "tipo incompativel no retorno ("+currentRet.str()+" <- "+tr.str()+")");
                            ok = false;
                        }
                    }
                }
                didReturn = true;
                blockReturns = true; // nada depois
            }
            else if (auto iff = dynamic_cast<IfStmt*>(sptr.get())){
                {
                    Type tc = checkExpr(iff->cond.get(), local);
                    if (tc.isArray() || tc.kind != Type::Logico) {
                        diag.error(0,0, "condicao de 'se' deve ser logico");
                        ok = false;
                    }
                }

                bool retThen=false, retElse=false;
                ok &= checkBlock(iff->thenBlk.get(), local, currentRet, retThen);
                if (iff->elseBlk) ok &= checkBlock(iff->elseBlk.get(), local, currentRet, retElse);

                if (retThen && retElse) {
                    didReturn = true;
                    blockReturns = true;
                }
            }
            else if (auto wh = dynamic_cast<WhileStmt*>(sptr.get())){
                {
                    Type tc = checkExpr(wh->cond.get(), local);
                    if (tc.isArray() || tc.kind != Type::Logico) {
                        diag.error(0,0, "condicao de 'enquanto' deve ser logico");
                        ok = false;
                    }
                }
                loopDepth++;
                bool retBody=false;
                ok &= checkBlock(wh->body.get(), local, currentRet, retBody);
                loopDepth--;
                // não assumimos laço infinito
            }
            else if (auto dw = dynamic_cast<DoWhileStmt*>(sptr.get())){
                // Semântica: condição deve ser lógica (mesma regra de 'enquanto')
                loopDepth++;
                bool retBody=false;
                ok &= checkBlock(dw->body.get(), local, currentRet, retBody);
                loopDepth--;
                {
                    Type tc = checkExpr(dw->cond.get(), local);
                    if (tc.isArray() || tc.kind != Type::Logico) {
                        diag.error(0,0, "condicao de 'do-while' deve ser logico");
                        ok = false;
                    }
                }
            }
            else if (auto fr = dynamic_cast<ForStmt*>(sptr.get())){
                // For tem escopo próprio para init
                Scope forScope(&local);
                if (fr->init) {
                    if (auto v = dynamic_cast<VarDecl*>(fr->init.get())){
                        if (v->arrayLenExpr) {
                            auto cv = evalConst(v->arrayLenExpr.get());
                            if (!cv.isConst) diag.error(0,0, "tamanho de vetor deve ser const-expr em '"+v->name+"'");
                            else { long long val=(v->type.kind==Type::Logico)?((cv.i!=0)?1:0):cv.i; if (val<1) diag.error(0,0,"tamanho de vetor invalido (>=1) em '"+v->name+"'"); else { v->arrayLen=(int)val; v->type.arrayLen=v->arrayLen; }}
                        }
                        if (!forScope.declare(v->name, v->type)) diag.error(0,0, "variavel redeclarada: "+v->name);
                        if (v->init){
                            Type ti = checkExpr(v->init.get(), forScope);
                            bool okConv = isImplicitlyConvertible(ti, v->type)
                                || (v->type.kind==Type::Logico && ti.kind==Type::Inteiro && (isIntLiteral01(v->init.get()) || isIntConst01(v->init.get())));
                            if (!okConv) diag.error(0,0, "tipo incompativel na inicializacao de '"+v->name+"' (");
                        }
                    } else if (auto a = dynamic_cast<AssignStmt*>(fr->init.get())){
                        const Type* tv = forScope.lookup(a->name);
                        if (!tv) diag.error(0,0, "variavel nao declarada: "+a->name);
                        else {
                            Type te = checkExpr(a->value.get(), forScope);
                            bool okConv = isImplicitlyConvertible(te, *tv) || (tv->kind==Type::Logico && te.kind==Type::Inteiro && (isIntLiteral01(a->value.get())||isIntConst01(a->value.get())));
                            if (!okConv) diag.error(0,0, "tipo incompativel na atribuicao de '"+a->name+"'");
                        }
                    } else if (auto es = dynamic_cast<ExprStmt*>(fr->init.get())){
                        (void)checkExpr(es->expr.get(), forScope);
                    }
                }
                if (fr->cond) {
                    Type tc = checkExpr(fr->cond.get(), forScope);
                    if (tc.isArray() || tc.kind != Type::Logico) diag.error(0,0, "condicao de 'for' deve ser logico");
                }
                if (fr->step) {
                    if (auto a = dynamic_cast<AssignStmt*>(fr->step.get())){
                        const Type* tv = forScope.lookup(a->name);
                        if (!tv) diag.error(0,0, "variavel nao declarada: "+a->name);
                        else {
                            Type te = checkExpr(a->value.get(), forScope);
                            bool okConv = isImplicitlyConvertible(te, *tv) || (tv->kind==Type::Logico && te.kind==Type::Inteiro && (isIntLiteral01(a->value.get())||isIntConst01(a->value.get())));
                            if (!okConv) diag.error(0,0, "tipo incompativel na atribuicao de '"+a->name+"'");
                        }
                    } else if (auto es = dynamic_cast<ExprStmt*>(fr->step.get())){
                        (void)checkExpr(es->expr.get(), forScope);
                    }
                }
                loopDepth++;
                bool retBody=false;
                ok &= checkBlock(fr->body.get(), forScope, currentRet, retBody);
                loopDepth--;
            }
            else if (auto sw = dynamic_cast<SwitchStmt*>(sptr.get())){
                // scrutinee deve ser inteiro (escalares)
                Type ts = checkExpr(sw->scrutinee.get(), local);
                if (ts.isArray() || ts.kind != Type::Inteiro) {
                    diag.error(0,0, "expressao de 'switch' deve ser inteiro");
                }
                // cases: sem duplicatas
                std::unordered_set<int> seen;
                for (auto& c : sw->cases) {
                    if (!seen.insert(c.value).second) {
                        diag.error(0,0, std::string("valor de 'case' duplicado: ") + std::to_string(c.value));
                    }
                }
                // contexto de switch para 'break' e verificação de fallthrough
                switchDepth++;
                int totalArms = (int)sw->cases.size() + (sw->deflt ? 1 : 0);
                int remaining = totalArms;

                for (size_t i=0;i<sw->cases.size();++i) {
                    --remaining;
                    insideCase++;
                    caseArmsRemaining.push_back(remaining);
                    // 'fallthrough' deve ser última instrução se presente
                    if (!sw->cases[i].body->stmts.empty()) {
                        for (size_t k=0;k+1<sw->cases[i].body->stmts.size();++k) {
                            if (dynamic_cast<FallthroughStmt*>(sw->cases[i].body->stmts[k].get())) {
                                diag.error(0,0, "'fallthrough' deve ser a ultima instrucao do case");
                                ok = false;
                                break;
                            }
                        }
                    }
                    bool retArm=false;
                    ok &= checkBlock(sw->cases[i].body.get(), local, currentRet, retArm);
                    caseArmsRemaining.pop_back();
                    insideCase--;
                }
                if (sw->deflt) {
                    --remaining;
                    insideCase++;
                    caseArmsRemaining.push_back(remaining);
                    if (!sw->deflt->stmts.empty()) {
                        for (size_t k=0;k+1<sw->deflt->stmts.size();++k) {
                            if (dynamic_cast<FallthroughStmt*>(sw->deflt->stmts[k].get())) {
                                diag.error(0,0, "'fallthrough' deve ser a ultima instrucao do case");
                                ok = false;
                                break;
                            }
                        }
                    }
                    bool retDef=false;
                    ok &= checkBlock(sw->deflt.get(), local, currentRet, retDef);
                    caseArmsRemaining.pop_back();
                    insideCase--;
                }
                switchDepth--;
            }
            else if (dynamic_cast<BreakStmt*>(sptr.get())){
                if (loopDepth <= 0 && switchDepth <= 0) diag.error(0,0, "'break' fora de laco ou switch");
            }
            else if (dynamic_cast<ContinueStmt*>(sptr.get())){
                if (loopDepth <= 0) diag.error(0,0, "'continue' fora de laco");
            }
            else if (dynamic_cast<FallthroughStmt*>(sptr.get())){
                if (insideCase <= 0) {
                    diag.error(0,0, "'fallthrough' so pode aparecer dentro de um 'case' de 'switch'");
                    ok = false;
                } else {
                    int remain = caseArmsRemaining.empty() ? 0 : caseArmsRemaining.back();
                    if (remain <= 0) {
                        diag.error(0,0, "'fallthrough' no ultimo braco do switch");
                        ok = false;
                    }
                }
            }
            else if (auto blk = dynamic_cast<Block*>(sptr.get())){
                bool retInner=false;
                ok &= checkBlock(blk, local, currentRet, retInner);
                if (retInner) {
                    didReturn = true;
                    blockReturns = true;
                }
            }
            else if (auto es = dynamic_cast<ExprStmt*>(sptr.get())){
                (void)checkExpr(es->expr.get(), local);
            }
        }
        return ok;
    }

    bool checkFunc(FuncDecl* f){
        Scope top(nullptr);
        // injeta globais no escopo
        for (auto& [name, ty] : globals) {
            top.declare(name, ty);
            auto itC = globalsConst.find(name);
            if (itC != globalsConst.end()) top.setConstWhereDeclared(name, itC->second);
            auto itD = globalsDims.find(name);
            if (itD != globalsDims.end()) top.setDimsHere(name, itD->second);
        }
        // declara parâmetros
        for (auto& p : f->params){
            if (!top.declare(p.name, p.type)){
                diag.error(0,0, "variavel redeclarada: " + p.name);
            }
            // Não marque parâmetros inteiros automaticamente como booleanos.
            // Uma variável só vira "boolLike" quando for claramente 0/1
            // (literal/const) ou receber um valor lógico em alguma atribuição.
        }

        bool didReturn = false;
        bool ok = checkBlock(f->body.get(), top, f->ret, didReturn);
        if (f->ret.kind != Type::Vazio && !didReturn){
            diag.error(0,0, "retorno ausente para funcao '" + f->name + "'");
            ok = false;
        }
        return ok;
    }

    Type checkExpr(Expr* e, Scope& scope){
        // literais
        if (dynamic_cast<IntLit*>(e)) return Type::inteiro();
        if (dynamic_cast<StringLit*>(e)) return Type::texto();

        // variáveis
        if (auto v = dynamic_cast<VarRef*>(e)){
            auto tv = scope.lookup(v->name);
            if (!tv){
                diag.error(0,0, "variavel nao declarada: " + v->name);
                return Type::inteiro();
            }
            return *tv;
        }

        // unário
        if (auto u = dynamic_cast<Unary*>(e)){
            if (u->op == "!") {
                Type rt = checkExpr(u->rhs.get(), scope);
                if (rt.kind == Type::Logico) return Type::logico();
                if (rt.kind == Type::Inteiro) {
                    if (auto lit = dynamic_cast<IntLit*>(u->rhs.get())) {
                        int v = lit->value; // ajuste se seu AST usa outro nome
                        if (v == 0 || v == 1) return Type::logico();
                    }
                }
                diag.error(0,0, "operador '!' requer logico");
                return Type::logico();
            }
            if (u->op == "-") {
                Type rt = checkExpr(u->rhs.get(), scope);
                if (rt.isArray() || rt.kind != Type::Inteiro){
                    diag.error(0,0, "operador unario '-' requer inteiro");
                }
                return Type::inteiro();
            }
            // fallback: tipo do operando
            return checkExpr(u->rhs.get(), scope);
        }

            // binário
            if (auto b = dynamic_cast<Binary*>(e)){
                Type lt = checkExpr(b->lhs.get(), scope);
                Type rt = checkExpr(b->rhs.get(), scope);
                const std::string& op = b->op;

                if (op=="+"||op=="-"||op=="*"||op=="/"||op=="%"){
                    if (lt.kind == Type::Texto || rt.kind == Type::Texto){
                        diag.error(0,0, "operacao aritmetica nao suportada para texto");
                    }
                    if (lt.isArray() || rt.isArray()){
                        diag.error(0,0, "operacao aritmetica requer escalares");
                    }
                    // aceita inteiro ou logico (coerção para inteiro)
                    if (!((lt.kind==Type::Inteiro||lt.kind==Type::Logico) && (rt.kind==Type::Inteiro||rt.kind==Type::Logico))){
                        diag.error(0,0, "operacao aritmetica requer inteiros/logicos");
                    }
                    return Type::inteiro();
                }
                if (op=="<"||op==">"||op=="<="||op==">="){
                    if (lt.kind == Type::Texto || rt.kind == Type::Texto){
                        diag.error(0,0, "comparacao nao suportada para texto");
                    }
                    if (lt.isArray() || rt.isArray() ||
                        lt.kind != Type::Inteiro || rt.kind != Type::Inteiro){
                        diag.error(0,0, "comparacao requer inteiros escalares");
                    }
                    return Type::logico();
                }
                if (op=="=="||op=="!="){
                    if (lt.kind == Type::Texto || rt.kind == Type::Texto){
                        diag.error(0,0, "igualdade nao suportada para texto");
                    }
                    if (lt.kind != rt.kind ||
                        lt.isArray() != rt.isArray() ||
                        lt.arrayLen != rt.arrayLen){
                        diag.error(0,0, "igualdade entre tipos diferentes ("+lt.str()+" vs "+rt.str()+")");
                    }
                    return Type::logico();
                }
                if (op=="&&"||op=="||"){
                    if (lt.isArray() || rt.isArray()){
                        diag.error(0,0, "operador logico requer escalares");
                    }
                    // aceita inteiro/logico como bool-like
                    if (!((lt.kind==Type::Inteiro||lt.kind==Type::Logico) && (rt.kind==Type::Inteiro||rt.kind==Type::Logico))){
                        diag.error(0,0, "operador logico requer inteiro/logico");
                    }
                    return Type::logico();
                }
                return lt; // fallback
            }

    // chamada de função
    if (auto c = dynamic_cast<Call*>(e)){
        std::string fname = getCallName(*c);
        auto it = funcs.find(fname);
        if (it == funcs.end()){
            diag.error(0,0, "funcao nao declarada: " + fname);
            return Type::inteiro();
        }
        const FuncSig& sig = it->second;
        if (sig.params.size() != c->args.size()){
            diag.error(0,0, "chamada a '" + fname + "' com aridade incorreta (esperado "
                              + std::to_string(sig.params.size()) + ", obtido "
                              + std::to_string(c->args.size()) + ")");
        }
        size_t n = std::min(sig.params.size(), c->args.size());
        for (size_t i=0;i<n;++i){
            Type ta = checkExpr(c->args[i].get(), scope);
            const Type& tp = sig.params[i];
            // R2-08: se o formal for array (tem dims), exigir argumento array por VarRef com mesmo rank
            if (!sig.paramDims.empty() && i < sig.paramDims.size() && !sig.paramDims[i].empty()){
                auto vr = dynamic_cast<VarRef*>(c->args[i].get());
                if (!vr){
                    diag.error(0,0, "argumento deve ser array passado por referencia no parametro " + std::to_string(i+1));
                } else {
                    auto dimsArg = scope.getDims(vr->name);
                    if (dimsArg.empty()){
                        diag.error(0,0, "argumento nao eh array no parametro " + std::to_string(i+1));
                    } else if (dimsArg.size() != sig.paramDims[i].size()){
                        diag.error(0,0, "rank de array incompatível no argumento " + std::to_string(i+1));
                    }
                }
                // Skip scalar conversion checks for arrays
                continue;
            }

            // Regra para argumentos:
            // - Permite bool -> int (escalares)
            // - Permite int -> bool APENAS se o inteiro for "booleano" (0/1 literal/const)
            //   ou uma variável marcada como boolLike (ex.: recebeu um bool ou 0/1 antes)
            bool okConv = isImplicitlyConvertible(ta, tp);

            if (!tp.isArray() && !ta.isArray()) {
                if (tp.kind == Type::Inteiro && ta.kind == Type::Logico) {
                    // bool -> int: permitido
                    okConv = true;
                } else if (tp.kind == Type::Logico && ta.kind == Type::Inteiro) {
                    // int -> bool: permitir somente 0 ou 1 (literal/const) ou variável marcada como "boolLike".
                    bool isConvertible = false;
                    long long _tmpConst;
                    if (tryEvalIntConst(c->args[i].get(), _tmpConst) && (_tmpConst == 0 || _tmpConst == 1)) {
                        isConvertible = true; // apenas 0/1
                    } else if (auto vr = dynamic_cast<VarRef*>(c->args[i].get())) {
                        if (scope.getBoolLike(vr->name)) isConvertible = true;
                    }
                    okConv = isConvertible;
                }
            }

            if (!okConv){
                diag.error(0,0, "argumento " + std::to_string(i+1) + " incompatível ("
                                 + tp.str() + " <- " + ta.str() + ")");
            }
        }
        if (sig.ret.isArray()){
            diag.error(0,0, "retorno de array por valor nao suportado em '" + fname + "'");
        }
        return sig.ret;
    }

        // indexação a[i][j] ... (ND)
        if (auto ixTop = dynamic_cast<Index*>(e)){
            // achata cadeia
            std::vector<Expr*> idxExprs;
            Expr* cur = e;
            std::string name;
            while (auto ix = dynamic_cast<Index*>(cur)) {
                idxExprs.push_back(ix->idx.get());
                cur = ix->base.get();
            }
            if (auto vr = dynamic_cast<VarRef*>(cur)) {
                name = vr->name;
            } else {
                diag.error(0,0, "indexacao invalida");
                return Type::inteiro();
            }
            std::reverse(idxExprs.begin(), idxExprs.end());
            // checa tipos dos índices
            for (auto* ex : idxExprs) {
                Type ti = checkExpr(ex, scope);
                if (ti.kind != Type::Inteiro || ti.isArray()) {
                    diag.error(0,0, "indice de array deve ser inteiro");
                }
            }
            // checa dimensões
            std::vector<int> dims = scope.getDims(name);
            if (dims.empty()) {
                // se não há dims registradas, aceite 1D legado (var escalar não permite index)
                const Type* tv = scope.lookup(name);
                if (!tv || !tv->isArray()) {
                    diag.error(0,0, "indexacao em tipo nao-array");
                }
            } else {
                if (idxExprs.size() != dims.size()) {
                    diag.error(0,0, "numero de indices diferente das dimensoes do array");
                }
            }
            // tipo do elemento = base escalar
            const Type* tv = scope.lookup(name);
            if (tv) {
                Type base = *tv; base.arrayLen = 0; return base;
            }
            return Type::inteiro();
        }

        // fallback
        return Type::inteiro();
    }
};

} // namespace mycc
