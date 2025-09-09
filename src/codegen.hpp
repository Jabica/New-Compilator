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
#include <llvm/IR/DIBuilder.h>

namespace mycc {

class Codegen {
public:
    Codegen(std::string moduleName, Diag& d, bool enableDebug = false, const std::string& srcPath = "", bool ubsanEnabled = false, bool asanEnabled = false);
    // Gera IR para um Programa. Retorna ponteiro para Module pronto.
    std::unique_ptr<llvm::Module> run(Program* prog);

private:
    void seedBuiltins();
    void setupDebug(const std::string& moduleName, const std::string& srcPath);
    llvm::DISubprogram* createDISubprogram(FuncDecl* f, llvm::Function* F);

    Diag& diag;
    llvm::LLVMContext ctx;
    std::unique_ptr<llvm::Module>      mod;
    std::unique_ptr<llvm::IRBuilder<>> builder;

    struct VarSlot {
        llvm::Value* ptr = nullptr;     // alloca*, GlobalVariable*, ou ptr para elemento
        llvm::Type*  elemTy = nullptr;  // tipo do elemento (i32/i1)
        bool isGlobal = false;
        bool isArray  = false;
        size_t arrayLen = 0;            // 0 = escalar
    };

    // Escopo de variáveis (nome -> slot)
    struct Scope {
        std::unordered_map<std::string, VarSlot> locals;
        Scope* parent = nullptr;
        explicit Scope(Scope* p=nullptr) : parent(p) {}
        VarSlot* lookup(const std::string& n) {
            for (auto s=this; s; s=s->parent) {
                auto it = s->locals.find(n);
                if (it!=s->locals.end()) return &it->second;
            }
            return nullptr;
        }
        void declare(const std::string& n, llvm::Value* p, llvm::Type* ty,
                     bool isGlob=false, bool isArr=false, size_t nElem=0) {
            locals[n] = VarSlot{p, ty, isGlob, isArr, nElem};
        }
    };

    // ---- Helpers/infra ----
    // declarações das funções auxiliares (se usadas no projeto)
    llvm::Type*       ty(const Type& t);
    llvm::Function*   emitFuncDecl(FuncDecl* f);
    llvm::AllocaInst* createEntryAlloca(llvm::Function* F, llvm::Type* T, const std::string& name);
    void emitGlobals(Program* p);

    // ---- Conversions helpers ----
    llvm::Value* toBool  (llvm::Value* v);                 // iN -> i1
    llvm::Value* toInt32 (llvm::Value* v);                 // i1/iN -> i32
    llvm::Value* castForParam (llvm::Value* v, llvm::Type* paramTy);
    llvm::Value* castForReturn(llvm::Value* v, llvm::Type* retTy);
    llvm::Value* emitUBDivCheck(llvm::Value* denom, const SourceLoc& loc);

    // ---- Emissão por nó ----
    void emitFuncBody(FuncDecl* f);
    void emitBlock   (Block* b, Scope& scope);
    void emitStmt    (Stmt* s, Scope& scope);
    void emitIf      (IfStmt* s, Scope& scope);
    void emitWhile   (WhileStmt* s, Scope& scope);

    // ---- Expressões ----
    llvm::Value* emitExpr  (Expr* e, Scope& scope);
    llvm::Value* emitUnary (Unary* u, Scope& scope);
    llvm::Value* emitBinary(Binary* b, Scope& scope);

    // ---- Debug info ----
    void setLoc(const SourceLoc& L);
    bool debug = false;
    bool ubsan = false;
    bool asan = false;
    std::unique_ptr<llvm::DIBuilder> dib;
    llvm::DICompileUnit* cu = nullptr;
    llvm::DIFile* difile = nullptr;
    llvm::DIType* diI32 = nullptr;
    llvm::DIType* diI1  = nullptr;
    llvm::DIType* diVoid= nullptr;
    llvm::DIScope* curScope = nullptr; // subprogram ou lexical block atual
    // tabela de globais
    std::unordered_map<std::string, VarSlot> globalSlots;
};

} // namespace mycc
