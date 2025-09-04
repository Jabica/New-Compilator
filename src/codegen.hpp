#pragma once
#include "ast.hpp"
#include "diagnostics.hpp"

#include <memory>
#include <string>
#include <unordered_map>

// LLVM
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

namespace mycc {

class Codegen {
public:
    Codegen(std::string moduleName, Diag& d);
    // Gera IR para um Programa. Retorna ponteiro para Module pronto.
    std::unique_ptr<llvm::Module> run(Program* prog);

private:
    Diag& diag;
    llvm::LLVMContext ctx;
    std::unique_ptr<llvm::Module>      mod;
    std::unique_ptr<llvm::IRBuilder<>> builder;

    // Escopo de variáveis (nome -> alocação)
    struct Scope {
        std::unordered_map<std::string, llvm::AllocaInst*> locals;
        Scope* parent = nullptr;
        explicit Scope(Scope* p=nullptr) : parent(p) {}
        llvm::AllocaInst* lookup(const std::string& n) const {
            for (auto s=this; s; s=s->parent) {
                auto it = s->locals.find(n);
                if (it!=s->locals.end()) return it->second;
            }
            return nullptr;
        }
        bool declare(const std::string& n, llvm::AllocaInst* a) {
            return locals.emplace(n, a).second;
        }
    };

    // ---- Helpers/infra ----
    void seedBuiltins();                         // declarações das funções auxiliares
    llvm::Type*       ty(const Type& t);
    llvm::Function*   emitFuncDecl(FuncDecl* f);
    llvm::AllocaInst* createEntryAlloca(llvm::Function* F, llvm::Type* T, const std::string& name);

    // Conversions helpers
    llvm::Value* toBool(llvm::Value* v);        // iN -> i1
    llvm::Value* toInt32(llvm::Value* v);       // i1/iN -> i32
    llvm::Value* castForParam(llvm::Value* v, llvm::Type* paramTy);
    llvm::Value* castForReturn(llvm::Value* v, llvm::Type* retTy);

    // ---- Emissão por nó ----
    void emitFuncBody(FuncDecl* f);
    void emitBlock(Block* b, Scope& scope);
    void emitStmt(Stmt* s, Scope& scope);
    void emitIf(IfStmt* s, Scope& scope);
    void emitWhile(WhileStmt* s, Scope& scope);

    // ---- Expressões ----
    llvm::Value* emitExpr(Expr* e, Scope& scope);
    llvm::Value* emitUnary(Unary* u, Scope& scope);
    llvm::Value* emitBinary(Binary* b, Scope& scope);
};

} // namespace mycc