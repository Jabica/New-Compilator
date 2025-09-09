#pragma once
#include <string>
#include <vector>
#include <memory>
#include <iostream>

namespace mycc {

// Informações de origem (arquivo/linha/coluna)
struct SourceLoc {
    std::string file; // opcional
    unsigned line = 0;
    unsigned col  = 0;
    SourceLoc() = default;
    SourceLoc(std::string f, unsigned l, unsigned c)
        : file(std::move(f)), line(l), col(c) {}
    bool valid() const { return line != 0; }
};

struct Node {
    SourceLoc loc; // posição no código-fonte
    virtual ~Node() = default;
    virtual void dump(std::ostream& os, int depth=0) const = 0;
};

inline void indent(std::ostream& os, int d){ for(int i=0;i<d;i++) os << "  "; }

// --- ast.hpp (trecho) ---
struct Type {
    enum Kind { Inteiro, Logico, Vazio, Texto } kind;
    // Suporte simples a array 1D: 0 = escalar; >0 = tamanho fixo
    int arrayLen = 0;

    static Type inteiro() { return Type{Inteiro, 0}; }
    static Type logico()  { return Type{Logico,  0}; }
    static Type vazio()   { return Type{Vazio,   0}; }
    static Type texto()   { return Type{Texto,   0}; }

    bool isArray() const { return arrayLen > 0; }

    // Tipo do elemento (remove o sufixo [N])
    Type elem() const { Type t = *this; t.arrayLen = 0; return t; }

    std::string str() const {
        std::string s = (kind==Inteiro? "inteiro" : kind==Logico? "logico" : kind==Vazio? "vazio" : "texto");
        if (arrayLen > 0) s += "[" + std::to_string(arrayLen) + "]";
        return s;
    }
};

struct Expr : Node {};
struct Stmt : Node {};
struct Block; // forward declaration

// ===== EXPRESSÕES =====
struct IntLit : Expr {
    int value;
    explicit IntLit(int v, SourceLoc L = {}) : value(v) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override { indent(os,d); os<<"IntLit("<<value<<")\n"; }
};

struct BoolLit : Expr {
    bool value;
    explicit BoolLit(bool v, SourceLoc L = {}) : value(v) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os<<"BoolLit("<<(value?"true":"false")<<")\n";
    }
};

struct StringLit : Expr {
    std::string value;
    explicit StringLit(std::string v, SourceLoc L = {}) : value(std::move(v)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os<<"StringLit(\"";
        for (unsigned char c : value) {
            if (c=='\n') os << "\\n";
            else if (c=='\t') os << "\\t";
            else if (c=='\"') os << "\\\"";
            else if (c=='\\') os << "\\\\";
            else if (c=='\0') os << "\\0";
            else os << c;
        }
        os << "\")\n";
    }
};

struct VarRef : Expr {
    std::string name;
    explicit VarRef(std::string n, SourceLoc L = {}) : name(std::move(n)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "VarRef(" << name << ")\n";
    }
};

struct Unary : Expr {
    std::string op;                 // "!" ou "-"
    std::unique_ptr<Expr> rhs;
    Unary(std::string o, std::unique_ptr<Expr> e, SourceLoc L = {})
        : op(std::move(o)), rhs(std::move(e)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "Unary(" << op << ")\n";
        if (rhs) rhs->dump(os, d+1);
    }
};

struct Binary : Expr {
    std::string op;                 // "+", "-", "*", "/", "%", "<", "<=", ...
    std::unique_ptr<Expr> lhs;
    std::unique_ptr<Expr> rhs;
    Binary(std::unique_ptr<Expr> L, std::string o, std::unique_ptr<Expr> R, SourceLoc Lc = {})
        : op(std::move(o)), lhs(std::move(L)), rhs(std::move(R)) { loc = std::move(Lc); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "Binary(" << op << ")\n";
        if (lhs) lhs->dump(os, d+1);
        if (rhs) rhs->dump(os, d+1);
    }
};

// chamada de funcao
struct Call : Expr {
    std::string callee;
    std::vector<std::unique_ptr<Expr>> args;
    Call(std::string n, std::vector<std::unique_ptr<Expr>> as, SourceLoc L = {})
        : callee(std::move(n)), args(std::move(as)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "Call " << callee << "\n";
        for (auto& a : args) a->dump(os, d+1);
    }
};

// indexacao de array: base[idx]
struct Index : Expr {
    std::unique_ptr<Expr> base;
    std::unique_ptr<Expr> idx;
    Index(std::unique_ptr<Expr> b, std::unique_ptr<Expr> i, SourceLoc L = {})
        : base(std::move(b)), idx(std::move(i)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "Index\n";
        if (base) { indent(os,d+1); os << "base:\n"; base->dump(os, d+2); }
        if (idx)  { indent(os,d+1); os << "idx:\n";  idx->dump(os,  d+2); }
    }
};

// ===== COMANDOS =====
struct VarDecl : Stmt {
    std::string name;
    Type type;
    int arrayLen = 0;                 // <-- NOVO: 0 = escalar; >0 = tamanho do vetor
    std::unique_ptr<Expr> arrayLenExpr; // Patch 18: expr de tamanho (const-expr)
    std::unique_ptr<Expr> init;       // opcional
    bool isConst = false;             // Patch 16: apenas globais neste patch
    std::vector<std::unique_ptr<Expr>> initList; // Patch 16: lista para vetores globais
    // Patch 17: valores constantes normalizados pelo semântico (para codegen)
    bool hasConstInit = false;
    long long constInit = 0;                  // escalar (0/1 para logico)
    std::vector<long long> constInitList;     // elementos normalizados (tam <= arrayLen; padding no codegen)
    VarDecl() = default;
    VarDecl(std::string n, Type t, int len, std::unique_ptr<Expr> i, SourceLoc L = {})
        : name(std::move(n)), type(t), arrayLen(len), init(std::move(i)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d);
        os<<"VarDecl "<<name<<": "<<type.str();
        if (arrayLen > 0) os<<"["<<arrayLen<<"]";
        if (isConst) os<<" const";
        os<<"\n";
        if (init){ indent(os,d+1); os<<"init:\n"; init->dump(os,d+2); }
        if (!initList.empty()){
            indent(os,d+1); os<<"initList:"<<"\n";
            for (auto& e : initList) { if (e) e->dump(os,d+2); }
        }
    }
};

struct ReturnStmt : Stmt {
    std::unique_ptr<Expr> value; // opcional
    ReturnStmt() = default;
    explicit ReturnStmt(std::unique_ptr<Expr> v, SourceLoc L = {}) : value(std::move(v)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os<<"Return\n";
        if(value){ value->dump(os,d+1); }
    }
};

struct AssignStmt : Stmt {
    std::string name;
    std::unique_ptr<Expr> value;
    AssignStmt() = default;
    AssignStmt(std::string n, std::unique_ptr<Expr> v, SourceLoc L = {}) : name(std::move(n)), value(std::move(v)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "Assign " << name << "\n";
        if (value) value->dump(os, d+1);
    }
};

// atribuicao v[i] = expr
struct AssignIndex : Stmt {
    std::unique_ptr<Expr> base;   // normalmente VarRef
    std::unique_ptr<Expr> index;  // expr do índice
    std::unique_ptr<Expr> value;  // valor
    AssignIndex(std::unique_ptr<Expr> b, std::unique_ptr<Expr> i, std::unique_ptr<Expr> v, SourceLoc L = {})
        : base(std::move(b)), index(std::move(i)), value(std::move(v)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "AssignIndex\n";
        if (base)  { indent(os,d+1); os<<"base:\n";  base->dump(os,d+2); }
        if (index) { indent(os,d+1); os<<"index:\n"; index->dump(os,d+2); }
        if (value) { indent(os,d+1); os<<"value:\n"; value->dump(os,d+2); }
    }
};

struct ExprStmt : Stmt {
    std::unique_ptr<Expr> expr;
    explicit ExprStmt(std::unique_ptr<Expr> e, SourceLoc L = {}) : expr(std::move(e)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "ExprStmt\n";
        if (expr) expr->dump(os, d+1);
    }
};

struct Block : Stmt {
    std::vector<std::unique_ptr<Stmt>> stmts;
    Block() = default;
    explicit Block(SourceLoc L) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os<<"Block\n";
        for (auto& s : stmts) s->dump(os,d+1);
    }
};

struct IfStmt : Stmt {
    std::unique_ptr<Expr>  cond;
    std::unique_ptr<Block> thenBlk;
    std::unique_ptr<Block> elseBlk; // opcional
    IfStmt() = default;
    IfStmt(std::unique_ptr<Expr> c, std::unique_ptr<Block> t, std::unique_ptr<Block> e, SourceLoc L = {})
        : cond(std::move(c)), thenBlk(std::move(t)), elseBlk(std::move(e)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "IfStmt\n";
        if (cond){ indent(os,d+1); os << "cond:\n"; cond->dump(os,d+2); }
        if (thenBlk){ indent(os,d+1); os << "then:\n"; thenBlk->dump(os,d+2); }
        if (elseBlk){ indent(os,d+1); os << "else:\n"; elseBlk->dump(os,d+2); }
    }
};

struct WhileStmt : Stmt {
    std::unique_ptr<Expr>  cond;
    std::unique_ptr<Block> body;
    WhileStmt() = default;
    WhileStmt(std::unique_ptr<Expr> c, std::unique_ptr<Block> b, SourceLoc L = {})
        : cond(std::move(c)), body(std::move(b)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "WhileStmt\n";
        if (cond){ indent(os,d+1); os << "cond:\n"; cond->dump(os,d+2); }
        if (body){ indent(os,d+1); os << "body:\n"; body->dump(os,d+2); }
    }
};

// R2-01: do { ... } while (cond)
struct DoWhileStmt : Stmt {
    std::unique_ptr<Block> body;
    std::unique_ptr<Expr>  cond;
    DoWhileStmt() = default;
    DoWhileStmt(std::unique_ptr<Block> b, std::unique_ptr<Expr> c, SourceLoc L = {})
        : body(std::move(b)), cond(std::move(c)) { loc = std::move(L); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "DoWhile\n";
        if (body){ indent(os,d+1); os << "body:\n"; body->dump(os,d+2); }
        if (cond){ indent(os,d+1); os << "cond:\n"; cond->dump(os,d+2); }
    }
};

// Patch 20: novos nós de controle de fluxo
struct BreakStmt : Stmt {
    void dump(std::ostream& os, int d=0) const override { indent(os,d); os << "Break\n"; }
};
struct ContinueStmt : Stmt {
    void dump(std::ostream& os, int d=0) const override { indent(os,d); os << "Continue\n"; }
};

// R2-03: fallthrough;
struct FallthroughStmt : Stmt {
    void dump(std::ostream& os, int d=0) const override { indent(os,d); os << "Fallthrough\n"; }
};

struct ForStmt : Stmt {
    std::unique_ptr<Stmt>  init; // VarDecl/Assign/ExprStmt ou null
    std::unique_ptr<Expr>  cond; // null => true
    std::unique_ptr<Stmt>  step; // Assign ou ExprStmt; opcional
    std::unique_ptr<Block> body;
    ForStmt(std::unique_ptr<Stmt> i,
            std::unique_ptr<Expr> c,
            std::unique_ptr<Stmt> s,
            std::unique_ptr<Block> b){ init=std::move(i); cond=std::move(c); step=std::move(s); body=std::move(b); }
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "For\n";
        if (init) { indent(os,d+1); os << "init:\n"; init->dump(os,d+2); }
        if (cond) { indent(os,d+1); os << "cond:\n"; cond->dump(os,d+2); }
        if (step) { indent(os,d+1); os << "step:\n"; step->dump(os,d+2); }
        if (body) { indent(os,d+1); os << "body:\n"; body->dump(os,d+2); }
    }
};

// R2-02: switch/case/default (sem fallthrough)
struct CaseArm {
    int value;
    std::unique_ptr<Block> body;
    CaseArm(int v, std::unique_ptr<Block> b) : value(v), body(std::move(b)) {}
};

struct SwitchStmt : Stmt {
    std::unique_ptr<Expr>  scrutinee;
    std::vector<CaseArm>   cases;
    std::unique_ptr<Block> deflt; // opcional
    explicit SwitchStmt(std::unique_ptr<Expr> e) : scrutinee(std::move(e)) {}
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os << "Switch\n";
        if (scrutinee) {
            indent(os,d+1); os << "scrutinee:\n"; scrutinee->dump(os, d+2);
        }
        for (auto const& c : cases) {
            indent(os,d+1); os << "case " << c.value << ":\n";
            if (c.body) c.body->dump(os, d+2);
        }
        if (deflt) { indent(os,d+1); os << "default:\n"; deflt->dump(os, d+2); }
    }
};

// ===== FUNÇÕES / PROGRAMA =====
struct Param { std::string name; Type type; };

struct FuncDecl : Node {
    std::string name;
    std::vector<Param> params;
    Type ret;
    std::unique_ptr<Block> body;
    FuncDecl() = default;
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os<<"FuncDecl "<<name<<"(";
        for (size_t i=0;i<params.size();++i){
            os<<params[i].name<<": "<<params[i].type.str();
            if(i+1<params.size()) os<<", ";
        }
        os<<"): "<<ret.str()<<"\n";
        if(body) body->dump(os,d+1);
    }
};

struct Program : Node {
    std::vector<std::unique_ptr<VarDecl>>  globals; // globais no topo do arquivo
    std::vector<std::unique_ptr<FuncDecl>> funcs;
    void dump(std::ostream& os, int d=0) const override {
        indent(os,d); os<<"Program\n";
        for (auto& g : globals) { indent(os,d+1); os<<"global "; g->dump(os, d+2); }
        for (auto& f : funcs) f->dump(os,d+1);
    }
};

} // namespace mycc
