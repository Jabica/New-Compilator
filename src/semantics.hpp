#pragma once
#include "ast.hpp"
#include "diagnostics.hpp"
#include <unordered_map>
#include <string>
#include <vector>
#include <algorithm>

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

    return false;
}

// Nome da função chamado em Call (seu AST usa `callee`)
inline std::string getCallName(const Call& c) {
    return c.callee;
}


static inline bool isIntLiteral01(Expr* e) {
    if (auto lit = dynamic_cast<IntLit*>(e)) return lit->value == 0 || lit->value == 1;
    return false;
}

// Avalia (recursivamente) se a expressão é uma constante inteira (literal ou expressão aritmética constante)
static inline bool tryEvalIntConst(Expr* e, long long& out) {
    // Literal inteiro
    if (auto lit = dynamic_cast<IntLit*>(e)) { out = lit->value; return true; }

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
    Scope* parent = nullptr;

    explicit Scope(Scope* p=nullptr) : parent(p) {}

    bool declare(const std::string& name, const Type& ty){
        bool inserted = vars.emplace(name, ty).second;
        if (inserted) {
            // reset flag local ao declarar
            boolLike.erase(name);
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
};

struct FuncSig {
    Type ret;
    std::vector<Type> params;
};

class SemanticChecker {
public:
    explicit SemanticChecker(Diag& d) : diag(d) {}

    bool run(Program* prog){
        funcs.clear();
        seedBuiltins();     // registra built-ins
        collectFuncs(prog); // registra funções do usuário + detecta redefinição

        bool ok = true;
        for (auto& fptr : prog->funcs) ok &= checkFunc(fptr.get());
        return ok && !diag.hadError;
    }

private:
    Diag& diag;
    std::unordered_map<std::string, FuncSig> funcs;

    // Built-ins mínimos (lado semântico)
    void seedBuiltins() {
        // printi(int), printb(logico)
        funcs["printi"] = FuncSig{ Type::vazio(), { Type::inteiro() } };
        funcs["printb"] = FuncSig{ Type::vazio(), { Type::logico()  } };
    }

    void collectFuncs(Program* prog){
        for (auto& fptr : prog->funcs){
            FuncSig sig; sig.ret = fptr->ret;
            sig.params.reserve(fptr->params.size());
            for (auto& p : fptr->params) sig.params.push_back(p.type);

            // Proíbe retorno de array por valor
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
                if (!local.declare(v->name, v->type)){
                    diag.error(0,0, "variavel redeclarada: " + v->name);
                    ok = false;
                }
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
                Type tbase = checkExpr(ai->base.get(), local);
                Type tidx  = checkExpr(ai->index.get(), local); // seu AST usa 'index'
                if (!tbase.isArray()){
                    diag.error(0,0, "indexacao em tipo nao-array");
                    ok = false;
                }
                if (tidx.kind != Type::Inteiro || tidx.isArray()){
                    diag.error(0,0, "indice de array deve ser inteiro");
                    ok = false;
                }
                Type tval = checkExpr(ai->value.get(), local);
                if (tbase.isArray()){
                    Type elem = tbase.elem();
                    if (!isImplicitlyConvertible(tval, elem)){
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
                        if (!isImplicitlyConvertible(tr, currentRet)){
                            diag.error(0,0, "tipo incompativel no retorno ("+currentRet.str()+" <- "+tr.str()+")");
                            ok = false;
                        }
                    }
                }
                didReturn = true;
                blockReturns = true; // nada depois
            }
            else if (auto iff = dynamic_cast<IfStmt*>(sptr.get())){
                Type tc = checkExpr(iff->cond.get(), local);
                // Pelos testes mais recentes: condição deve ser lógica
                if (tc.isArray() || tc.kind != Type::Logico){
                    diag.error(0,0, "condicao de 'se' deve ser logico");
                    ok = false;
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
                Type tc = checkExpr(wh->cond.get(), local);
                if (tc.isArray() || tc.kind != Type::Logico){
                    diag.error(0,0, "condicao de 'enquanto' deve ser logico");
                    ok = false;
                }
                bool retBody=false;
                ok &= checkBlock(wh->body.get(), local, currentRet, retBody);
                // não assumimos laço infinito
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
        // declara parâmetros
        for (auto& p : f->params){
            if (!top.declare(p.name, p.type)){
                diag.error(0,0, "variavel redeclarada: " + p.name);
            }
            // Heurística: parâmetros inteiros começam como "boolLike" (0/1) até que alguma
            // atribuição posterior os desmarque. Isso permite a conversão implícita
            // int->logico ao repassar parâmetros que vêm de 0/1 ou de booleanos.
            if (p.type.kind == Type::Inteiro) {
                top.setBoolLikeHere(p.name, true);
            }
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
                    if (lt.isArray() || rt.isArray() ||
                        lt.kind != Type::Inteiro || rt.kind != Type::Inteiro){
                        diag.error(0,0, "operacao aritmetica requer inteiros escalares");
                    }
                    return Type::inteiro();
                }
                if (op=="<"||op==">"||op=="<="||op==">="){
                    if (lt.isArray() || rt.isArray() ||
                        lt.kind != Type::Inteiro || rt.kind != Type::Inteiro){
                        diag.error(0,0, "comparacao requer inteiros escalares");
                    }
                    return Type::logico();
                }
                if (op=="=="||op=="!="){
                    if (lt.kind != rt.kind ||
                        lt.isArray() != rt.isArray() ||
                        lt.arrayLen != rt.arrayLen){
                        diag.error(0,0, "igualdade entre tipos diferentes ("+lt.str()+" vs "+rt.str()+")");
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

        // indexação a[b]
        if (auto ix = dynamic_cast<Index*>(e)){
            Type tb = checkExpr(ix->base.get(), scope);
            Type ti = checkExpr(ix->idx.get(),  scope); // seu AST usa 'idx'
            if (!tb.isArray()){
                diag.error(0,0, "indexacao em tipo nao-array");
            }
            if (ti.kind != Type::Inteiro || ti.isArray()){
                diag.error(0,0, "indice de array deve ser inteiro");
            }
            return tb.isArray() ? tb.elem() : Type::inteiro();
        }

        // fallback
        return Type::inteiro();
    }
};

} // namespace mycc