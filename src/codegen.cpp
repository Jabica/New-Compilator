#include "codegen.hpp"
#include <cassert>
#include <string>

using namespace llvm;

namespace mycc {

// ------------------------------------------------------------
Codegen::Codegen(std::string moduleName, Diag& d)
    : diag(d) {
    mod = std::make_unique<llvm::Module>(moduleName, ctx);
    builder = std::make_unique<llvm::IRBuilder<>>(ctx);
}

std::unique_ptr<llvm::Module> Codegen::run(Program* p) {
    for (auto& fptr : p->funcs) emitFuncDecl(fptr.get());
    for (auto& fptr : p->funcs) emitFuncBody(fptr.get());
    return std::move(mod);
}

// ------------------------------------------------------------
llvm::Type* Codegen::ty(const mycc::Type& t) {
    switch (t.kind) {
        case mycc::Type::Inteiro: return llvm::Type::getInt32Ty(ctx);
        case mycc::Type::Logico:  return llvm::Type::getInt1Ty(ctx);
        case mycc::Type::Vazio:   return llvm::Type::getVoidTy(ctx);
    }
    return llvm::Type::getInt32Ty(ctx);
}

// ------------------------------------------------------------
Function* Codegen::emitFuncDecl(FuncDecl* f) {
    std::vector<llvm::Type*> params;
    params.reserve(f->params.size());
    for (auto& p : f->params) params.push_back(ty(p.type));
    llvm::ArrayRef<llvm::Type*> paramRef(params);
    llvm::FunctionType* FT = llvm::FunctionType::get(ty(f->ret), paramRef, /*isVarArg=*/false);
    Function* F = Function::Create(FT, Function::ExternalLinkage, f->name, mod.get());

    unsigned i=0;
    for (auto& arg : F->args()) {
        arg.setName(f->params[i++].name);
    }
    return F;
}

llvm::AllocaInst* Codegen::createEntryAlloca(llvm::Function* F, llvm::Type* T, const std::string& name) {
    IRBuilder<> tmp(&F->getEntryBlock(), F->getEntryBlock().begin());
    return tmp.CreateAlloca(T, nullptr, name);
}

// ------------------------------------------------------------
void Codegen::emitFuncBody(FuncDecl* f) {
    Function* F = mod->getFunction(f->name);
    if (!F) { F = emitFuncDecl(f); }

    if (F->empty()) {
        BasicBlock* entry = BasicBlock::Create(ctx, "entry", F);
        builder->SetInsertPoint(entry);
    } else {
        builder->SetInsertPoint(&F->getEntryBlock(), F->getEntryBlock().end());
    }

    Scope scope(nullptr);
    for (auto& arg : F->args()) {
        auto* A = createEntryAlloca(F, arg.getType(), std::string(arg.getName()));
        builder->CreateStore(&arg, A);
        scope.declare(std::string(arg.getName()), A);
    }

    emitBlock(f->body.get(), scope);

    if (f->ret.kind != mycc::Type::Vazio) {
        if (!builder->GetInsertBlock()->getTerminator()) {
            auto* RTy = ty(f->ret);
            if (RTy->isIntegerTy())
                builder->CreateRet(llvm::ConstantInt::get(RTy, 0));
        }
    } else {
        if (!builder->GetInsertBlock()->getTerminator()) {
            builder->CreateRetVoid();
        }
    }
}

// ------------------------------------------------------------
void Codegen::emitBlock(Block* b, Scope& parent) {
    Scope local(&parent);
    for (auto& sp : b->stmts) {
        emitStmt(sp.get(), local);
        if (builder->GetInsertBlock()->getTerminator()) break;
    }
}

void Codegen::emitStmt(Stmt* s, Scope& scope) {
    if (auto v = dynamic_cast<VarDecl*>(s)) {
        auto* baseTy = ty(v->type);
        auto* F = builder->GetInsertBlock()->getParent();

        AllocaInst* A = nullptr;
        if (v->arrayLen > 0) {
            // aloca "arrayLen" inteiros na pilha: alloca i32, i32 arrayLen
            auto* lenVal = ConstantInt::get(llvm::Type::getInt32Ty(ctx), v->arrayLen);
            IRBuilder<> tmp(&F->getEntryBlock(), F->getEntryBlock().begin());
            A = tmp.CreateAlloca(baseTy, lenVal, v->name); // tipo alocado = i32, resultado = i32*
        } else {
            A = createEntryAlloca(F, baseTy, v->name);     // escalar: alloca i32
        }

        scope.declare(v->name, A);

        if (v->init) {
            auto* initV = emitExpr(v->init.get(), scope);
            auto* T = A->getAllocatedType(); // i32
            if (T->isIntegerTy() && initV->getType() != T) {
                initV = builder->CreateZExtOrTrunc(initV, T);
            }
            // Para vetor, vamos inicializar a posição 0 (opcional). Para já, apenas escalar:
            if (v->arrayLen == 0) {
                builder->CreateStore(initV, A);
            }
        }
        return;
    }

    if (auto a = dynamic_cast<AssignStmt*>(s)) {
        auto* A = scope.lookup(a->name);
        if (!A) {
            diag.error(0,0,"codegen: variavel nao declarada: " + a->name);
            return;
        }
        auto* rhs = emitExpr(a->value.get(), scope);
        auto* T = A->getAllocatedType(); // i32
        if (T->isIntegerTy() && rhs->getType() != T) {
            rhs = builder->CreateZExtOrTrunc(rhs, T);
        }
        builder->CreateStore(rhs, A);
        return;
    }

    if (auto ai = dynamic_cast<AssignIndex*>(s)) {
        // base deve ser VarRef (suporte 1D)
        auto* baseRef = dynamic_cast<VarRef*>(ai->base.get());
        if (!baseRef) {
            diag.error(0,0,"codegen: atribuicao indexada com base nao suportada");
            (void)emitExpr(ai->value.get(), scope);
            return;
        }
        AllocaInst* A = scope.lookup(baseRef->name);
        if (!A) {
            diag.error(0,0,"codegen: variavel nao declarada: " + baseRef->name);
            (void)emitExpr(ai->value.get(), scope);
            return;
        }

        // calcula índice
        Value* idxV = emitExpr(ai->index.get(), scope);
        if (!idxV->getType()->isIntegerTy(32)) {
            idxV = builder->CreateZExtOrTrunc(idxV, llvm::Type::getInt32Ty(ctx));
        }

        // ponteiro para o elemento: gep (base i32*, idx)
        auto* elemPtr = builder->CreateInBoundsGEP(
            A->getAllocatedType(), // i32
            A,                     // i32*
            idxV,                  // i32
            baseRef->name + ".elem.ptr"
        );

        // valor a gravar
        Value* val = emitExpr(ai->value.get(), scope);
        if (val->getType() != A->getAllocatedType()) {
            val = builder->CreateZExtOrTrunc(val, A->getAllocatedType());
        }
        builder->CreateStore(val, elemPtr);
        return;
    }

    if (auto r = dynamic_cast<ReturnStmt*>(s)) {
        if (r->value) {
            auto* val = emitExpr(r->value.get(), scope);
            builder->CreateRet(val);
        } else {
            builder->CreateRetVoid();
        }
        return;
    }

    if (auto blk = dynamic_cast<Block*>(s)) {
        emitBlock(blk, scope);
        return;
    }

    if (auto es = dynamic_cast<ExprStmt*>(s)) {
        (void)emitExpr(es->expr.get(), scope);
        return;
    }
    // IfStmt / WhileStmt: ainda não
}

// ------------------------------------------------------------
Value* Codegen::emitExpr(Expr* e, Scope& scope) {
    if (auto i = dynamic_cast<IntLit*>(e)) {
        return ConstantInt::get(llvm::Type::getInt32Ty(ctx), i->value, /*isSigned=*/true);
    }

    if (auto v = dynamic_cast<VarRef*>(e)) {
        AllocaInst* A = scope.lookup(v->name);
        if (!A) {
            diag.error(0,0,"codegen: variavel nao declarada: " + v->name);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        // Carrega escalar. Se for um vetor e o usuário usar 'v' nu, faremos load do elemento 0.
        // (caso raro; o normal é usar v[i])
        Value* ptr = A;
        return builder->CreateLoad(A->getAllocatedType(), ptr, v->name + ".val");
    }

    if (auto u = dynamic_cast<Unary*>(e)) {
        return emitUnary(u, scope);
    }

    if (auto b = dynamic_cast<Binary*>(e)) {
        return emitBinary(b, scope);
    }

    if (auto c = dynamic_cast<Call*>(e)) {
        llvm::Function* callee = mod->getFunction(c->callee);
        if (!callee) {
            diag.error(0,0, std::string("codegen: funcao nao encontrada: ") + c->callee);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        std::vector<llvm::Value*> argsV;
        argsV.reserve(c->args.size());
        auto* FT = callee->getFunctionType();
        for (size_t i = 0; i < c->args.size(); ++i) {
            llvm::Value* v = emitExpr(c->args[i].get(), scope);
            if (i < FT->getNumParams()) {
                llvm::Type* PT = FT->getParamType((unsigned)i);
                if (v->getType() != PT && PT->isIntegerTy() && v->getType()->isIntegerTy()) {
                    v = builder->CreateZExtOrTrunc(v, PT);
                }
            }
            argsV.push_back(v);
        }
        if (FT->getReturnType()->isVoidTy()) {
            builder->CreateCall(callee, argsV);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        } else {
            return builder->CreateCall(callee, argsV, c->callee + ".call");
        }
    }

    if (auto idx = dynamic_cast<Index*>(e)) {
        // suporte 1D: base deve ser VarRef
        auto* baseRef = dynamic_cast<VarRef*>(idx->base.get());
        if (!baseRef) {
            diag.error(0,0,"codegen: indexacao com base nao suportada");
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        AllocaInst* A = scope.lookup(baseRef->name);
        if (!A) {
            diag.error(0,0,"codegen: variavel nao declarada: " + baseRef->name);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        Value* idxV = emitExpr(idx->idx.get(), scope);
        if (!idxV->getType()->isIntegerTy(32)) {
            idxV = builder->CreateZExtOrTrunc(idxV, llvm::Type::getInt32Ty(ctx));
        }
        auto* elemPtr = builder->CreateInBoundsGEP(
            A->getAllocatedType(), // i32
            A,                     // i32*
            idxV,
            baseRef->name + ".elem.ptr"
        );
        return builder->CreateLoad(A->getAllocatedType(), elemPtr, baseRef->name + ".elem");
    }

    return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
}

Value* Codegen::emitUnary(Unary* u, Scope& scope) {
    auto* rhs = emitExpr(u->rhs.get(), scope);
    if (u->op == "!") {
        if (!rhs->getType()->isIntegerTy(1)) {
            rhs = builder->CreateICmpNE(rhs, ConstantInt::get(rhs->getType(), 0));
        }
        Value* notv = builder->CreateNot(rhs);
        return builder->CreateZExt(notv, llvm::Type::getInt32Ty(ctx));
    }
    if (u->op == "-") {
        return builder->CreateNeg(rhs);
    }
    return rhs;
}

Value* Codegen::emitBinary(Binary* b, Scope& scope) {
    auto* L = emitExpr(b->lhs.get(), scope);
    auto* R = emitExpr(b->rhs.get(), scope);

    auto* i32 = llvm::Type::getInt32Ty(ctx);
    if (!L->getType()->isIntegerTy(32)) L = builder->CreateZExtOrTrunc(L, i32);
    if (!R->getType()->isIntegerTy(32)) R = builder->CreateZExtOrTrunc(R, i32);

    const std::string& op = b->op;
    if (op=="+")  return builder->CreateAdd(L, R, "addtmp");
    if (op=="-")  return builder->CreateSub(L, R, "subtmp");
    if (op=="*")  return builder->CreateMul(L, R, "multmp");
    if (op=="/")  return builder->CreateSDiv(L, R, "divtmp");
    if (op=="%")  return builder->CreateSRem(L, R, "modtmp");

    Value* cmp = nullptr;
    if (op=="<")   cmp = builder->CreateICmpSLT(L, R, "cmptmp");
    else if (op=="<=") cmp = builder->CreateICmpSLE(L, R, "cmptmp");
    else if (op==">")  cmp = builder->CreateICmpSGT(L, R, "cmptmp");
    else if (op==">=") cmp = builder->CreateICmpSGE(L, R, "cmptmp");
    else if (op=="==") cmp = builder->CreateICmpEQ (L, R, "cmptmp");
    else if (op=="!=") cmp = builder->CreateICmpNE (L, R, "cmptmp");

    if (cmp) return builder->CreateZExt(cmp, llvm::Type::getInt32Ty(ctx), "bool2i32");
    return L;
}

} // namespace mycc