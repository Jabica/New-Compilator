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
    seedBuiltins();
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
void Codegen::seedBuiltins() {
    // void printi(i32)
    auto *FTi = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx),
                                        { llvm::Type::getInt32Ty(ctx) }, false);
    if (!mod->getFunction("printi"))
        llvm::Function::Create(FTi, llvm::Function::ExternalLinkage, "printi", mod.get());

    // void printb(i1)
    auto *FTb = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx),
                                        { llvm::Type::getInt1Ty(ctx) }, false);
    if (!mod->getFunction("printb"))
        llvm::Function::Create(FTb, llvm::Function::ExternalLinkage, "printb", mod.get());
}

llvm::Value* Codegen::toBool(llvm::Value* v) {
    if (v->getType()->isIntegerTy(1)) return v;
    return builder->CreateICmpNE(v, llvm::ConstantInt::get(v->getType(), 0), "tobool");
}

llvm::Value* Codegen::toInt32(llvm::Value* v) {
    if (v->getType()->isIntegerTy(32)) return v;
    if (v->getType()->isIntegerTy(1))  return builder->CreateZExt(v, llvm::Type::getInt32Ty(ctx), "b2i32");
    if (v->getType()->isIntegerTy())   return builder->CreateZExtOrTrunc(v, llvm::Type::getInt32Ty(ctx), "iN2i32");
    return v;
}

llvm::Value* Codegen::castForParam(llvm::Value* v, llvm::Type* paramTy) {
    if (paramTy->isIntegerTy(1))  return toBool(v);
    if (paramTy->isIntegerTy(32)) return toInt32(v);
    return v;
}

llvm::Value* Codegen::castForReturn(llvm::Value* v, llvm::Type* retTy) {
    if (retTy->isVoidTy()) return nullptr;
    if (retTy->isIntegerTy(1))  return toBool(v);
    if (retTy->isIntegerTy(32)) return toInt32(v);
    return v;
}

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
            auto* F = builder->GetInsertBlock()->getParent();
            val = castForReturn(val, F->getFunctionType()->getReturnType());
            builder->CreateRet(val);
        } else {
            builder->CreateRetVoid();
        }
        return;
    }

    if (auto iff = dynamic_cast<IfStmt*>(s)) { emitIf(iff, scope); return; }
    if (auto wh  = dynamic_cast<WhileStmt*>(s)) { emitWhile(wh, scope); return; }

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
                v = castForParam(v, FT->getParamType((unsigned)i));
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
    // Primeiro, avalia operandos (sem forçar para i32 ainda)
    Value* L = emitExpr(b->lhs.get(), scope);
    Value* R = nullptr; // R só será avaliado imediatamente para operadores não-curto-circuito

    const std::string& op = b->op;

    // --- Curto-circuito para operadores lógicos && e || ---
    if (op == "&&" || op == "||") {
        llvm::Function* F = builder->GetInsertBlock()->getParent();
        // Bloco atual é o bloco do cond (onde testamos LHS)
        llvm::BasicBlock* condBB = builder->GetInsertBlock();
        auto* rhsBB  = llvm::BasicBlock::Create(ctx, "logic.rhs", F);
        auto* endBB  = llvm::BasicBlock::Create(ctx, "logic.end");

        // Converte LHS para i1
        Value* lhsBool = toBool(L);
        if (op == "&&")
            builder->CreateCondBr(lhsBool, rhsBB, endBB);
        else
            builder->CreateCondBr(lhsBool, endBB, rhsBB);

        // RHS
        builder->SetInsertPoint(rhsBB);
        Value* rhsBool = toBool( emitExpr(b->rhs.get(), scope) );
        builder->CreateBr(endBB);

        // END + PHI i1
        F->insert(F->end(), endBB);
        builder->SetInsertPoint(endBB);
        PHINode* phi = builder->CreatePHI(llvm::Type::getInt1Ty(ctx), 2, "logic.phi");
        if (op == "&&") {
            phi->addIncoming(ConstantInt::getFalse(ctx), condBB);
            phi->addIncoming(rhsBool, rhsBB);
        } else { // "||"
            phi->addIncoming(ConstantInt::getTrue(ctx),  condBB);
            phi->addIncoming(rhsBool, rhsBB);
        }
        // Nossa linguagem retorna i32 em expressões; faça ZExt de i1 -> i32
        return builder->CreateZExt(phi, llvm::Type::getInt32Ty(ctx), "bool2i32");
    }

    // Para operadores aritméticos e de comparação, promova para i32 quando necessário
    auto* i32 = llvm::Type::getInt32Ty(ctx);
    if (!L->getType()->isIntegerTy(32)) L = builder->CreateZExtOrTrunc(L, i32);

    // Somente agora avalie R para os demais operadores
    R = emitExpr(b->rhs.get(), scope);
    if (!R->getType()->isIntegerTy(32)) R = builder->CreateZExtOrTrunc(R, i32);

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

void Codegen::emitIf(IfStmt* s, Scope& scope) {
    llvm::Function* F = builder->GetInsertBlock()->getParent();

    auto* thenBB  = llvm::BasicBlock::Create(ctx, "if.then", F);
    auto* elseBB  = llvm::BasicBlock::Create(ctx, "if.else");
    auto* mergeBB = llvm::BasicBlock::Create(ctx, "if.end");

    llvm::Value* cond = toBool(emitExpr(s->cond.get(), scope));
    builder->CreateCondBr(cond, thenBB, s->elseBlk ? elseBB : mergeBB);

    builder->SetInsertPoint(thenBB);
    emitBlock(s->thenBlk.get(), scope);
    if (!builder->GetInsertBlock()->getTerminator()) builder->CreateBr(mergeBB);

    if (s->elseBlk) {
        F->insert(F->end(), elseBB);
        builder->SetInsertPoint(elseBB);
        emitBlock(s->elseBlk.get(), scope);
        if (!builder->GetInsertBlock()->getTerminator()) builder->CreateBr(mergeBB);
    }

    F->insert(F->end(), mergeBB);
    builder->SetInsertPoint(mergeBB);
}

void Codegen::emitWhile(WhileStmt* s, Scope& scope) {
    llvm::Function* F = builder->GetInsertBlock()->getParent();

    auto* condBB = llvm::BasicBlock::Create(ctx, "while.cond", F);
    auto* bodyBB = llvm::BasicBlock::Create(ctx, "while.body");
    auto* endBB  = llvm::BasicBlock::Create(ctx, "while.end");

    builder->CreateBr(condBB);

    builder->SetInsertPoint(condBB);
    llvm::Value* cond = toBool(emitExpr(s->cond.get(), scope));
    builder->CreateCondBr(cond, bodyBB, endBB);

    F->insert(F->end(), bodyBB);
    builder->SetInsertPoint(bodyBB);
    emitBlock(s->body.get(), scope);
    if (!builder->GetInsertBlock()->getTerminator()) builder->CreateBr(condBB);

    F->insert(F->end(), endBB);
    builder->SetInsertPoint(endBB);
}

} // namespace mycc