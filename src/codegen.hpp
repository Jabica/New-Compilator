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
    enum class Fast2DMode { Off, Auto, Always };
    enum class Vec2DMode  { Off, Auto, Always };
    Codegen(std::string moduleName, Diag& d, bool enableDebug = false, const std::string& srcPath = "", bool ubsanEnabled = false, bool asanEnabled = false);
    void setFast2DMode(Fast2DMode m) { fast2DMode = m; }
    void setVec2DMode(Vec2DMode m)   { vec2DMode  = m; }
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
    void emitDoWhile (DoWhileStmt* s, Scope& scope);
    void emitFor     (ForStmt* s, Scope& scope);
    void emitSwitch  (SwitchStmt* s, Scope& scope);

    // ---- Expressões ----
    llvm::Value* emitExpr  (Expr* e, Scope& scope);
    llvm::Value* emitUnary (Unary* u, Scope& scope);
    llvm::Value* emitBinary(Binary* b, Scope& scope);
    llvm::Value* emitStringLiteral(const std::string& s);
    // R2-07: helpers para ND
    std::pair<std::string, std::vector<llvm::Value*>> flattenIndexChain(Expr* e, Scope& scope);
    llvm::Value* linearizeOffset(const std::vector<int>& dims,
                                 const std::vector<llvm::Value*>& idxs);

    // R2-11: info de view contiguo 1D
    struct ViewInfo {
        llvm::Value* basePtrI8 = nullptr; // i8*
        llvm::Value* lenBytes  = nullptr; // i32
        unsigned elemBytes     = 4;       // inteiro = 4
    };
    ViewInfo getContiguous1DView(Expr* e, Scope& scope);

    // R2-12: slices 1D com stride (linhas/colunas)
    struct Slice1D {
        llvm::Value* baseI8   = nullptr; // i8* do primeiro elemento
        llvm::Value* lenElems = nullptr; // i32 numero de elementos
        llvm::Value* strideB  = nullptr; // i32 stride em bytes
        unsigned elemBytes    = 4;
        bool isValid() const { return baseI8 && lenElems && strideB; }
        bool isContiguous() const {
            if (!isValid()) return false;
            if (auto C = llvm::dyn_cast<llvm::ConstantInt>(strideB))
                return C->getSExtValue() == (long long)elemBytes;
            return false; // conservador
        }
    };
    Slice1D getSlice1D(Expr* e, Scope& scope);
    void emitCopySlice(const Slice1D& dst, const Slice1D& src);
    void emitFillSlice(const Slice1D& dst, llvm::Value* v);
    // R2-14: fast-path + micro-tiling (unrollx4)
    void emitCopySliceSmart(const Slice1D& dst, const Slice1D& src);
    void emitFillSliceSmart(const Slice1D& dst, llvm::Value* scalar32);

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

    // Patch 20: controle de laços para break/continue
    struct LoopCtx { llvm::BasicBlock* condBB; llvm::BasicBlock* stepBB; llvm::BasicBlock* endBB; };
    std::vector<LoopCtx> loopStack;

    // R2-03: destinos de break/fallthrough
    std::vector<llvm::BasicBlock*> breakTargets;      // topo: destino de 'break'
    std::vector<llvm::BasicBlock*> fallthroughTargets; // topo: proximo case/default
    // R2-04: destino de 'continue'
    std::vector<llvm::BasicBlock*> continueTargets;    // topo: volta para cond

    // R2-07: dimensões por variável (globais/locais)
    std::unordered_map<std::string, std::vector<int>> arrayDimsByName;

    // R2-17: modo de fast-path 2D
    Fast2DMode fast2DMode = Fast2DMode::Auto;
    Vec2DMode  vec2DMode  = Vec2DMode::Auto;
};

} // namespace mycc
