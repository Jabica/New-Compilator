#include "codegen.hpp"
#include <cassert>
#include <string>

using namespace llvm;

namespace mycc {

    
// ------------------------------------------------------------
Codegen::Codegen(std::string moduleName, Diag& d, bool enableDebug, const std::string& srcPath, bool ubsanEnabled, bool asanEnabled)
    : diag(d), debug(enableDebug), ubsan(ubsanEnabled), asan(asanEnabled) {
    mod = std::make_unique<llvm::Module>(moduleName, ctx);
    builder = std::make_unique<llvm::IRBuilder<>>(ctx);
    if (debug) setupDebug(moduleName, srcPath);
    seedBuiltins();
}

std::unique_ptr<llvm::Module> Codegen::run(Program* p) {
    emitGlobals(p);
    for (auto& fptr : p->funcs) emitFuncDecl(fptr.get());
    for (auto& fptr : p->funcs) emitFuncBody(fptr.get());
    if (debug && dib) dib->finalize();
    return std::move(mod);
}

void Codegen::emitGlobals(Program* p) {
    using namespace llvm;
    for (auto& gptr : p->globals) {
        VarDecl* g = gptr.get();
        llvm::Type* elemTy = ty(g->type.elem());
        if (g->arrayLen > 0) {
            if (!g->arrayDims.empty()) arrayDimsByName[g->name] = g->arrayDims;
            auto* arrTy = llvm::ArrayType::get(elemTy, (uint64_t)g->arrayLen);
            llvm::Constant* initC = nullptr;
            if (!g->constInitList.empty()) {
                std::vector<llvm::Constant*> items;
                items.reserve((size_t)g->arrayLen);
                for (auto v : g->constInitList) {
                    if (elemTy->isIntegerTy(1)) items.push_back(v?llvm::ConstantInt::getTrue(ctx):llvm::ConstantInt::getFalse(ctx));
                    else items.push_back(llvm::ConstantInt::get(elemTy, v));
                }
                while (items.size() < (size_t)g->arrayLen) {
                    items.push_back(elemTy->isIntegerTy(1) ? llvm::ConstantInt::getFalse(ctx)
                                                           : llvm::ConstantInt::get(elemTy, 0));
                }
                initC = llvm::ConstantArray::get(arrTy, items);
            } else if (!g->initList.empty()) {
                std::vector<llvm::Constant*> items;
                items.reserve((size_t)g->arrayLen);
                for (auto& elt : g->initList) {
                    if (auto lit = dynamic_cast<IntLit*>(elt.get())) {
                        if (elemTy->isIntegerTy(1)) {
                            items.push_back(lit->value ? llvm::ConstantInt::getTrue(ctx)
                                                        : llvm::ConstantInt::getFalse(ctx));
                        } else {
                            items.push_back(llvm::ConstantInt::get(elemTy, lit->value));
                        }
                    }
                }
                while (items.size() < (size_t)g->arrayLen) {
                    items.push_back(elemTy->isIntegerTy(1) ? llvm::ConstantInt::getFalse(ctx)
                                                           : llvm::ConstantInt::get(elemTy, 0));
                }
                initC = llvm::ConstantArray::get(arrTy, items);
            } else {
                initC = llvm::ConstantAggregateZero::get(arrTy);
            }
            auto* GV = new llvm::GlobalVariable(
                *mod, arrTy, /*isConstant=*/g->isConst,
                llvm::GlobalValue::ExternalLinkage,
                initC,
                g->name);
            globalSlots[g->name] = VarSlot{GV, elemTy, /*isGlobal*/true, /*isArray*/true, (size_t)g->arrayLen};
        } else {
            llvm::Constant* initC = nullptr;
            if (g->hasConstInit) {
                if (elemTy->isIntegerTy(1)) initC = g->constInit?llvm::ConstantInt::getTrue(ctx):llvm::ConstantInt::getFalse(ctx);
                else initC = llvm::ConstantInt::get(elemTy, g->constInit);
            } else if (g->init) {
                if (auto lit = dynamic_cast<IntLit*>(g->init.get())) {
                    if (elemTy->isIntegerTy(1)) {
                        initC = lit->value ? llvm::ConstantInt::getTrue(ctx)
                                           : llvm::ConstantInt::getFalse(ctx);
                    } else {
                        initC = llvm::ConstantInt::get(elemTy, lit->value);
                    }
                }
            }
            if (!initC) {
                initC = elemTy->isIntegerTy(1) ? (llvm::Constant*)llvm::ConstantInt::getFalse(ctx)
                                               : (llvm::Constant*)llvm::ConstantInt::get(elemTy, 0);
            }
            auto* GV = new llvm::GlobalVariable(
                *mod, elemTy, /*isConstant=*/g->isConst,
                llvm::GlobalValue::ExternalLinkage,
                initC, g->name);
            globalSlots[g->name] = VarSlot{GV, elemTy, /*isGlobal*/true, /*isArray*/false, 0};
        }
    }
}

void Codegen::setupDebug(const std::string& moduleName, const std::string& srcPath) {
    using namespace llvm;
    // Versões de debug (DWARF v5, metadata v)
    mod->addModuleFlag(Module::Warning, "Debug Info Version", DEBUG_METADATA_VERSION);
    mod->addModuleFlag(Module::Warning, "Dwarf Version", 5);

    dib = std::make_unique<DIBuilder>(*mod);

    std::string dir = ".";
    std::string base = srcPath;
    if (!srcPath.empty()) {
        auto pos = srcPath.find_last_of('/');
        if (pos != std::string::npos) {
            dir = srcPath.substr(0, pos);
            base = srcPath.substr(pos + 1);
        }
    }

    difile = dib->createFile(base, dir);

    cu = dib->createCompileUnit(
        llvm::dwarf::DW_LANG_C, // idioma "neutro" o suficiente
        difile,
        "mycc-pt",
        false, // optimized
        "",
        0
    );

    diVoid = dib->createUnspecifiedType("void");
    diI32  = dib->createBasicType("int", 32, llvm::dwarf::DW_ATE_signed);
    diI1   = dib->createBasicType("bool", 1,  llvm::dwarf::DW_ATE_boolean);
}

void Codegen::setLoc(const SourceLoc& L) {
    if (!debug || !dib || !L.valid() || !curScope) return;
    auto *DL = llvm::DILocation::get(ctx, L.line, L.col, curScope);
    builder->SetCurrentDebugLocation(DL);
}

static llvm::DIType* diFromType(llvm::DIBuilder* D, llvm::DIType* diI32, llvm::DIType* diI1, llvm::DIType* diVoid, const mycc::Type& T) {
    switch (T.kind) {
        case mycc::Type::Inteiro: return diI32;
        case mycc::Type::Logico:  return diI1;
        case mycc::Type::Vazio:   return diVoid;
        case mycc::Type::Texto:   return diI32; // aproximação
    }
    return diI32;
}

// ------------------------------------------------------------
llvm::Type* Codegen::ty(const mycc::Type& t) {
    switch (t.kind) {
        case mycc::Type::Inteiro: return llvm::Type::getInt32Ty(ctx);
        case mycc::Type::Logico:  return llvm::Type::getInt1Ty(ctx);
        case mycc::Type::Vazio:   return llvm::Type::getVoidTy(ctx);
        case mycc::Type::Texto:   return llvm::PointerType::get(ctx, 0); // i8*
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

    // void prints(i8*)
    auto *FTs = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx),
                                        { llvm::PointerType::get(ctx, 0) }, false);
    if (!mod->getFunction("prints"))
        llvm::Function::Create(FTs, llvm::Function::ExternalLinkage, "prints", mod.get());
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

llvm::Value* Codegen::emitUBDivCheck(llvm::Value* denom, const SourceLoc& loc) {
    if (!ubsan) return nullptr;
    // if (denom == 0) { puts("runtime error: division by zero"); exit(1); }
    llvm::Function* F = builder->GetInsertBlock()->getParent();
    auto* i32 = llvm::Type::getInt32Ty(ctx);
    llvm::Value* d = denom;
    if (!d->getType()->isIntegerTy(32)) d = builder->CreateZExtOrTrunc(d, i32);

    llvm::Value* isZero = builder->CreateICmpEQ(d, llvm::ConstantInt::get(i32, 0), "iszero");
    auto* contBB = llvm::BasicBlock::Create(ctx, "div.cont");
    auto* errBB  = llvm::BasicBlock::Create(ctx, "div.err");
    builder->CreateCondBr(isZero, errBB, contBB);

    // errBB
    F->insert(F->end(), errBB);
    builder->SetInsertPoint(errBB);
    if (debug && loc.valid() && curScope) {
        builder->SetCurrentDebugLocation(llvm::DILocation::get(ctx, loc.line, loc.col, curScope));
    }
    // declare i32 @puts(ptr)
    llvm::Function* putsFn = mod->getFunction("puts");
    if (!putsFn) {
        auto* putsTy = llvm::FunctionType::get(i32, { llvm::PointerType::get(ctx, 0) }, false);
        putsFn = llvm::Function::Create(putsTy, llvm::Function::ExternalLinkage, "puts", mod.get());
    }
    // declare void @exit(i32)
    llvm::Function* exitFn = mod->getFunction("exit");
    if (!exitFn) {
        auto* exitTy = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), { i32 }, false);
        exitFn = llvm::Function::Create(exitTy, llvm::Function::ExternalLinkage, "exit", mod.get());
    }
    auto* msg = builder->CreateGlobalStringPtr("runtime error: division by zero\n");
    builder->CreateCall(putsFn, { msg });
    builder->CreateCall(exitFn, { llvm::ConstantInt::get(i32, 1) });
    builder->CreateUnreachable();

    // contBB
    F->insert(F->end(), contBB);
    builder->SetInsertPoint(contBB);
    if (debug && loc.valid() && curScope) {
        builder->SetCurrentDebugLocation(llvm::DILocation::get(ctx, loc.line, loc.col, curScope));
    }
    return nullptr;
}

Function* Codegen::emitFuncDecl(FuncDecl* f) {
    std::vector<llvm::Type*> params;
    params.reserve(f->params.size());
    for (auto& p : f->params) {
        llvm::Type* PT = ty(p.type);
        if (!p.arrayDims.empty()) {
            PT = llvm::PointerType::get(ctx, 0); // array param -> opaque ptr
        }
        params.push_back(PT);
    }
    llvm::ArrayRef<llvm::Type*> paramRef(params);
    llvm::FunctionType* FT = llvm::FunctionType::get(ty(f->ret), paramRef, /*isVarArg=*/false);
    Function* F = Function::Create(FT, Function::ExternalLinkage, f->name, mod.get());

    if (debug && cu && difile && dib) {
        // Monta assinatura DI
        std::vector<llvm::Metadata*> paramsMD;
        paramsMD.reserve(f->params.size() + 1);
        paramsMD.push_back(diFromType(dib.get(), diI32, diI1, diVoid, f->ret));
        for (auto& p : f->params) paramsMD.push_back(diFromType(dib.get(), diI32, diI1, diVoid, p.type));
        auto *diSubTy = dib->createSubroutineType(dib->getOrCreateTypeArray(paramsMD));

        auto *SP = dib->createFunction(
            cu,
            f->name,
            f->name,
            difile,
            1, // line
            diSubTy,
            1, // scope line
            llvm::DINode::FlagZero,
            llvm::DISubprogram::SPFlagDefinition
        );
        F->setSubprogram(SP);
    }

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

    if (debug) {
        if (auto *SP = F->getSubprogram()) {
            curScope = SP;
            // Cria um bloco léxico para o corpo da função, ancorado na posição do corpo
            unsigned bl = (f->body && f->body->loc.valid()) ? f->body->loc.line : (f->loc.valid()? f->loc.line : 1);
            unsigned bc = (f->body && f->body->loc.valid()) ? f->body->loc.col  : (f->loc.valid()? f->loc.col  : 1);
            curScope = dib->createLexicalBlock(curScope, difile, bl, bc);
            builder->SetCurrentDebugLocation(llvm::DILocation::get(ctx, bl, bc, curScope));
        }
    }

    Scope scope(nullptr);
    // Torna globais visíveis
    for (auto& kv : globalSlots) {
        scope.declare(kv.first, kv.second.ptr, kv.second.elemTy, /*isGlob=*/true, kv.second.isArray, kv.second.arrayLen);
    }
    {
        unsigned i=0;
        for (auto& arg : F->args()) {
            std::string pname = std::string(arg.getName());
            const Param& P = f->params[i];
            if (!P.arrayDims.empty()) {
                // param array: registre ponteiro base diretamente e dims
                llvm::Type* elemTy = ty(P.type);
                scope.declare(pname, &arg, elemTy, /*isGlob=*/false, /*isArr=*/true, 0);
                arrayDimsByName[pname] = P.arrayDims;
            } else {
                auto* A = createEntryAlloca(F, arg.getType(), pname);
                builder->CreateStore(&arg, A);
                scope.declare(pname, A, arg.getType(), /*isGlob=*/false, /*isArr=*/false, 0);
            }
            ++i;
        }
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
    // Cria novo escopo léxico de debug para o bloco, se habilitado
    llvm::DIScope* saved = curScope;
    if (debug && dib) {
        unsigned bl = b && b->loc.valid() ? b->loc.line : 1;
        unsigned bc = b && b->loc.valid() ? b->loc.col  : 1;
        if (curScope)
            curScope = dib->createLexicalBlock(curScope, difile, bl, bc);
    }

    Scope local(&parent);
    for (auto& sp : b->stmts) {
        emitStmt(sp.get(), local);
        if (builder->GetInsertBlock()->getTerminator()) break;
    }
    curScope = saved;
}

void Codegen::emitStmt(Stmt* s, Scope& scope) {
    setLoc(s->loc);
    if (auto v = dynamic_cast<VarDecl*>(s)) {
        auto* baseTy = ty(v->type);
        auto* F = builder->GetInsertBlock()->getParent();

        AllocaInst* A = nullptr;
        if (v->arrayLen > 0) {
            // ND: aloca totalSize() elementos contíguos
            int total = v->arrayLen;
            auto* lenVal = ConstantInt::get(llvm::Type::getInt32Ty(ctx), total);
            IRBuilder<> tmp(&F->getEntryBlock(), F->getEntryBlock().begin());
            A = tmp.CreateAlloca(baseTy, lenVal, v->name); // tipo alocado = i32, resultado = i32*
            if (!v->arrayDims.empty()) arrayDimsByName[v->name] = v->arrayDims;
        } else {
            A = createEntryAlloca(F, baseTy, v->name);     // escalar: alloca i32
        }

        if (debug && v->loc.valid() && curScope) {
            A->setDebugLoc(llvm::DebugLoc(llvm::DILocation::get(ctx, v->loc.line, v->loc.col, curScope)));
        }

        scope.declare(v->name, A, baseTy, /*isGlob=*/false, v->arrayLen>0, (size_t)v->arrayLen);

        if (v->init) {
            auto* initV = emitExpr(v->init.get(), scope);
            auto* T = baseTy;
            if (T->isIntegerTy(1)) {
                initV = toBool(initV);
            } else if (T->isIntegerTy(32)) {
                initV = toInt32(initV);
            }
            if (v->arrayLen == 0) {
                builder->CreateStore(initV, A);
            }
        }
        return;
    }

    if (auto a = dynamic_cast<AssignStmt*>(s)) {
        setLoc(a->loc);
        VarSlot* S = scope.lookup(a->name);
        if (!S) {
            diag.error(0,0,"codegen: variavel nao declarada: " + a->name);
            return;
        }
        auto* rhs = emitExpr(a->value.get(), scope);
        auto* T = S->elemTy;
        if (T->isIntegerTy(1)) {
            rhs = toBool(rhs);
        } else if (T->isIntegerTy(32)) {
            rhs = toInt32(rhs);
        }
        llvm::Value* dst = S->ptr;
        if (S->isArray && S->isGlobal) {
            auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            dst = builder->CreateInBoundsGEP(
                llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(),
                S->ptr,
                {zero, zero}, a->name + ".g0");
        }
        builder->CreateStore(rhs, dst);
        return;
    }

    if (auto ai = dynamic_cast<AssignIndex*>(s)) {
        setLoc(ai->loc);
        auto p = flattenIndexChain(ai->base.get(), scope);
        const std::string& name = p.first;
        auto idxs = std::move(p.second);
        if (name.empty()) { diag.error(0,0, "codegen: atribuicao indexada invalida"); (void)emitExpr(ai->value.get(), scope); return; }
        VarSlot* S = scope.lookup(name);
        if (!S) { diag.error(0,0, std::string("codegen: variavel nao declarada: ") + name); (void)emitExpr(ai->value.get(), scope); return; }
        Value* last = emitExpr(ai->index.get(), scope);
        if (!last->getType()->isIntegerTy(32)) last = builder->CreateZExtOrTrunc(last, llvm::Type::getInt32Ty(ctx));
        idxs.push_back(last);
        auto it = arrayDimsByName.find(name);
        if (it == arrayDimsByName.end()) { diag.error(0,0, "codegen: variavel escalar nao suporta indexacao"); (void)emitExpr(ai->value.get(), scope); return; }
        if (idxs.size() != it->second.size()) { diag.error(0,0, "codegen: numero de indices diferente das dimensoes do array"); (void)emitExpr(ai->value.get(), scope); return; }
        llvm::Value* off = linearizeOffset(it->second, idxs);
        // Optional ASan-like OOB check (educational)
        if (asan) {
            auto* i32 = llvm::Type::getInt32Ty(ctx);
            // off is i32; compute bounds using known total length
            int totalLen = 0;
            auto itLen = arrayDimsByName.find(name);
            if (itLen != arrayDimsByName.end()) {
                long long acc = 1; for (int d : itLen->second) acc *= d; totalLen = (int)acc;
            } else if (S->isArray) {
                totalLen = (int)S->arrayLen;
            }
            if (totalLen > 0) {
                llvm::Value* zero = llvm::ConstantInt::get(i32, 0);
                llvm::Value* lenV = llvm::ConstantInt::get(i32, totalLen);
                llvm::Value* lt0  = builder->CreateICmpSLT(off, zero, "oob.lt0");
                llvm::Value* geN  = builder->CreateICmpSGE(off, lenV, "oob.geN");
                llvm::Value* oob  = builder->CreateOr(lt0, geN, "oob");

                llvm::Function* Fun = builder->GetInsertBlock()->getParent();
                auto* okBB  = llvm::BasicBlock::Create(ctx, "idx.ok");
                auto* errBB = llvm::BasicBlock::Create(ctx, "idx.err");
                builder->CreateCondBr(oob, errBB, okBB);

                // errBB
                Fun->insert(Fun->end(), errBB);
                builder->SetInsertPoint(errBB);
                if (debug && ai->loc.valid() && curScope) builder->SetCurrentDebugLocation(llvm::DILocation::get(ctx, ai->loc.line, ai->loc.col, curScope));
                llvm::Function* putsFn = mod->getFunction("puts");
                if (!putsFn) {
                    auto* putsTy = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), { llvm::PointerType::get(ctx, 0) }, false);
                    putsFn = llvm::Function::Create(putsTy, llvm::Function::ExternalLinkage, "puts", mod.get());
                }
                llvm::Function* exitFn = mod->getFunction("exit");
                if (!exitFn) {
                    auto* exitTy = llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), { i32 }, false);
                    exitFn = llvm::Function::Create(exitTy, llvm::Function::ExternalLinkage, "exit", mod.get());
                }
                auto* msg = builder->CreateGlobalStringPtr("AddressSanitizer: out of bounds index\n");
                builder->CreateCall(putsFn, { msg });
                builder->CreateCall(exitFn, { llvm::ConstantInt::get(i32, 1) });
                builder->CreateUnreachable();

                // okBB
                Fun->insert(Fun->end(), okBB);
                builder->SetInsertPoint(okBB);
                if (debug && ai->loc.valid() && curScope) builder->SetCurrentDebugLocation(llvm::DILocation::get(ctx, ai->loc.line, ai->loc.col, curScope));
            }
        }

        llvm::Value* elemPtr = nullptr;
        if (S->isGlobal && S->isArray) {
            auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            elemPtr = builder->CreateInBoundsGEP(
                llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(),
                S->ptr,
                {zero, off}, name + ".g.elem.ptr");
        } else {
            elemPtr = builder->CreateInBoundsGEP(S->elemTy, S->ptr, off, name + ".elem.ptr");
        }
        Value* val = emitExpr(ai->value.get(), scope);
        llvm::Type* elemTy = S->elemTy;
        if (elemTy->isIntegerTy(1)) val = toBool(val);
        else if (elemTy->isIntegerTy(32)) val = toInt32(val);
        builder->CreateStore(val, elemPtr);
        return;
    }

    if (auto r = dynamic_cast<ReturnStmt*>(s)) {
        setLoc(r->loc);
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
    if (auto dw  = dynamic_cast<DoWhileStmt*>(s)) { emitDoWhile(dw, scope); return; }
    if (auto fr  = dynamic_cast<ForStmt*>(s))   { emitFor(fr, scope); return; }
    if (auto sw  = dynamic_cast<SwitchStmt*>(s)) { emitSwitch(sw, scope); return; }
    if (auto brk = dynamic_cast<BreakStmt*>(s)) {
        if (!breakTargets.empty()) {
            builder->CreateBr(breakTargets.back());
        } else if (!loopStack.empty()) {
            builder->CreateBr(loopStack.back().endBB);
        } else {
            diag.error(0,0, "codegen: 'break' fora de laco/switch");
        }
        return;
    }
    if (auto cont = dynamic_cast<ContinueStmt*>(s)) {
        if (!continueTargets.empty()) {
            builder->CreateBr(continueTargets.back());
        } else if (!loopStack.empty()) {
            builder->CreateBr(loopStack.back().stepBB);
        } else {
            diag.error(0,0, "codegen: 'continue' fora de laco");
        }
        return;
    }
    if (auto ft = dynamic_cast<FallthroughStmt*>(s)) {
        if (fallthroughTargets.empty()) { diag.error(0,0, "codegen: 'fallthrough' fora de case"); return; }
        builder->CreateBr(fallthroughTargets.back());
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
    setLoc(e->loc);
    if (auto i = dynamic_cast<IntLit*>(e)) {
        return ConstantInt::get(llvm::Type::getInt32Ty(ctx), i->value, /*isSigned=*/true);
    }
    if (auto s = dynamic_cast<StringLit*>(e)) {
        return emitStringLiteral(s->value); // i8*
    }

    if (auto v = dynamic_cast<VarRef*>(e)) {
        VarSlot* S = scope.lookup(v->name);
        if (!S) {
            diag.error(0,0,"codegen: variavel nao declarada: " + v->name);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        // Carrega escalar; se vetor, pega elemento 0
        Value* ptr = S->ptr;
        if (S->isArray && S->isGlobal) {
            auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            ptr = builder->CreateInBoundsGEP(
                llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(),
                S->ptr,
                {zero, zero}, v->name + ".g0");
        }
        return builder->CreateLoad(S->elemTy, ptr, v->name + ".val");
    }

    if (auto u = dynamic_cast<Unary*>(e)) {
        setLoc(u->loc);
        return emitUnary(u, scope);
    }

    if (auto b = dynamic_cast<Binary*>(e)) {
        setLoc(b->loc);
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
            llvm::Type* PT = (i < FT->getNumParams()) ? FT->getParamType((unsigned)i) : nullptr;
            llvm::Value* v = nullptr;
            if (PT && PT->isPointerTy()) {
                // Callee espera ponteiro (param array). Decay argumento se for VarRef de array.
                if (auto vr = dynamic_cast<VarRef*>(c->args[i].get())) {
                    VarSlot* S = scope.lookup(vr->name);
                    if (!S) { diag.error(0,0, "codegen: variavel nao declarada: "+vr->name); v = ConstantInt::get(llvm::Type::getInt32Ty(ctx),0); }
                    else {
                        llvm::Value* basePtr = nullptr;
                        // S->ptr pode ser i32* (local array) ou argumento i32* (param array)
                        basePtr = S->ptr;
                        // Globais: S->ptr é GlobalVariable do array; precise GEP [0,0]
                        if (S->isGlobal && S->isArray) {
                            auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                            basePtr = builder->CreateInBoundsGEP(
                                llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(),
                                S->ptr,
                                std::array<llvm::Value*,2>{zero, zero},
                                vr->name + ".g.base");
                        }
                        v = basePtr;
                    }
                } else {
                    // fallback: avalia e espera que semântica tenha barrado
                    v = emitExpr(c->args[i].get(), scope);
                }
            } else {
                v = emitExpr(c->args[i].get(), scope);
                if (PT) v = castForParam(v, PT);
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
        // ND + slice 1D: achata cadeia
        auto pair = flattenIndexChain(e, scope);
        const std::string& name = pair.first;
        auto idxs = std::move(pair.second);
        if (name.empty() || idxs.empty()) {
            diag.error(0,0, "codegen: indexacao invalida");
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        VarSlot* S = scope.lookup(name);
        if (!S) {
            diag.error(0,0, std::string("codegen: variavel nao declarada: ") + name);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        auto it = arrayDimsByName.find(name);
        if (it == arrayDimsByName.end()) {
            diag.error(0,0, "codegen: variavel escalar nao suporta indexacao");
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        size_t rank = it->second.size();
        if (idxs.size() == rank) {
            // elemento escalar
            llvm::Value* off = linearizeOffset(it->second, idxs);
            llvm::Value* elemPtr = nullptr;
            if (S->isGlobal && S->isArray) {
                auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                elemPtr = builder->CreateInBoundsGEP(
                    llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(),
                    S->ptr,
                    {zero, off}, name + ".g.elem.ptr");
            } else {
                elemPtr = builder->CreateInBoundsGEP(S->elemTy, S->ptr, off, name + ".elem.ptr");
            }
            return builder->CreateLoad(S->elemTy, elemPtr, name + ".elem");
        }
        if (idxs.size() < rank) {
            // slice ND: retorna i32* para o inicio do sub-array (sem load)
            llvm::Value* off = linearizeOffset(it->second, idxs);
            if (S->isGlobal && S->isArray) {
                auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                return builder->CreateInBoundsGEP(
                    llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(),
                    S->ptr,
                    {zero, off}, name + ".g.slice.ptr");
            } else {
                return builder->CreateInBoundsGEP(S->elemTy, S->ptr, off, name + ".slice.ptr");
            }
        }
        diag.error(0,0, "codegen: indices em excesso para este array");
        return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
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
    if (op=="/")  {
        (void)emitUBDivCheck(R, b->loc);
        return builder->CreateSDiv(L, R, "divtmp");
    }
    if (op=="%")  {
        (void)emitUBDivCheck(R, b->loc);
        return builder->CreateSRem(L, R, "modtmp");
    }

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

llvm::Value* Codegen::emitStringLiteral(const std::string& s) {
    using namespace llvm;
    std::string withZero = s; withZero.push_back('\0');
    auto* arrTy = ArrayType::get(llvm::Type::getInt8Ty(ctx), (uint64_t)withZero.size());
    auto* data = ConstantDataArray::getString(ctx, withZero, /*AddNull*/ false);
    auto* g = new GlobalVariable(*mod, arrTy, /*isConstant*/true,
                                 GlobalValue::PrivateLinkage,
                                 data, ".str");
    g->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
    g->setAlignment(MaybeAlign(1));
    Value* zero = ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
    Value* idxs[] = { zero, zero };
    return ConstantExpr::getInBoundsGetElementPtr(arrTy, g, idxs);
}

void Codegen::emitIf(IfStmt* s, Scope& scope) {
    setLoc(s->loc);
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

// R2-07 helpers
std::pair<std::string, std::vector<llvm::Value*>>
Codegen::flattenIndexChain(Expr* e, Scope& scope) {
    std::vector<llvm::Value*> idxs;
    Expr* cur = e;
    std::string name;
    while (auto ix = dynamic_cast<Index*>(cur)) {
        llvm::Value* iv = emitExpr(ix->idx.get(), scope);
        if (!iv->getType()->isIntegerTy(32)) iv = builder->CreateZExtOrTrunc(iv, llvm::Type::getInt32Ty(ctx));
        idxs.push_back(iv);
        cur = ix->base.get();
    }
    if (auto vr = dynamic_cast<VarRef*>(cur)) {
        name = vr->name;
        std::reverse(idxs.begin(), idxs.end());
    }
    return {name, std::move(idxs)};
}

llvm::Value* Codegen::linearizeOffset(const std::vector<int>& dims,
                                      const std::vector<llvm::Value*>& idxs) {
    auto* i32 = llvm::Type::getInt32Ty(ctx);
    llvm::Value* off = llvm::ConstantInt::get(i32, 0);
    if (dims.empty()) return off;
    std::vector<int> stride(dims.size(), 1);
    for (int d = (int)dims.size()-2; d>=0; --d) stride[d] = stride[d+1]*dims[d+1];
    for (size_t n=0;n<idxs.size();++n) {
        llvm::Value* term = idxs[n];
        if (n < stride.size() && stride[n] != 1) term = builder->CreateMul(term, llvm::ConstantInt::get(i32, stride[n]));
        off = builder->CreateAdd(off, term);
    }
    return off;
}

void Codegen::emitWhile(WhileStmt* s, Scope& scope) {
    setLoc(s->loc);
    llvm::Function* F = builder->GetInsertBlock()->getParent();

    auto* condBB = llvm::BasicBlock::Create(ctx, "while.cond", F);
    auto* bodyBB = llvm::BasicBlock::Create(ctx, "while.body");
    auto* endBB  = llvm::BasicBlock::Create(ctx, "while.end");

    builder->CreateBr(condBB);

    loopStack.push_back({condBB, condBB, endBB});

    builder->SetInsertPoint(condBB);
    llvm::Value* cond = toBool(emitExpr(s->cond.get(), scope));
    builder->CreateCondBr(cond, bodyBB, endBB);

    F->insert(F->end(), bodyBB);
    builder->SetInsertPoint(bodyBB);
    // R2-03/04: permitir break/continue dentro do while
    breakTargets.push_back(endBB);
    continueTargets.push_back(condBB);
    emitBlock(s->body.get(), scope);
    continueTargets.pop_back();
    breakTargets.pop_back();
    if (!builder->GetInsertBlock()->getTerminator()) builder->CreateBr(condBB);

    loopStack.pop_back();

    F->insert(F->end(), endBB);
    builder->SetInsertPoint(endBB);
}

void Codegen::emitDoWhile(DoWhileStmt* s, Scope& scope) {
    setLoc(s->loc);
    llvm::Function* F = builder->GetInsertBlock()->getParent();

    auto* bodyBB = llvm::BasicBlock::Create(ctx, "dowhile.body", F);
    auto* condBB = llvm::BasicBlock::Create(ctx, "dowhile.cond");
    auto* endBB  = llvm::BasicBlock::Create(ctx,  "dowhile.end");

    builder->CreateBr(bodyBB);

    // Empilha contexto de laço: continue -> condBB; break -> endBB
    loopStack.push_back({condBB, condBB, endBB});

    // BODY
    builder->SetInsertPoint(bodyBB);
    emitBlock(s->body.get(), scope);
    if (!builder->GetInsertBlock()->getTerminator()) builder->CreateBr(condBB);

    // COND
    F->insert(F->end(), condBB);
    builder->SetInsertPoint(condBB);
    llvm::Value* cond = toBool(emitExpr(s->cond.get(), scope));
    builder->CreateCondBr(cond, bodyBB, endBB);

    // END
    loopStack.pop_back();
    F->insert(F->end(), endBB);
    builder->SetInsertPoint(endBB);
}

void Codegen::emitFor(ForStmt* s, Scope& parent) {
    llvm::Function* F = builder->GetInsertBlock()->getParent();
    Scope local(&parent);
    if (s->init) emitStmt(s->init.get(), local);

    auto* condBB = llvm::BasicBlock::Create(ctx, "for.cond", F);
    auto* bodyBB = llvm::BasicBlock::Create(ctx, "for.body");
    auto* stepBB = llvm::BasicBlock::Create(ctx, "for.step");
    auto* endBB  = llvm::BasicBlock::Create(ctx, "for.end");

    builder->CreateBr(condBB);

    loopStack.push_back({condBB, stepBB, endBB});

    builder->SetInsertPoint(condBB);
    llvm::Value* cond = nullptr;
    if (s->cond) cond = toBool(emitExpr(s->cond.get(), local));
    else         cond = llvm::ConstantInt::getTrue(ctx);
    builder->CreateCondBr(cond, bodyBB, endBB);

    F->insert(F->end(), bodyBB);
    builder->SetInsertPoint(bodyBB);
    emitBlock(s->body.get(), local);
    if (!builder->GetInsertBlock()->getTerminator()) builder->CreateBr(stepBB);

    F->insert(F->end(), stepBB);
    builder->SetInsertPoint(stepBB);
    if (s->step) {
        emitStmt(s->step.get(), local);
    }
    builder->CreateBr(condBB);

    loopStack.pop_back();

    F->insert(F->end(), endBB);
    builder->SetInsertPoint(endBB);
}

void Codegen::emitSwitch(SwitchStmt* s, Scope& scope) {
    setLoc(s->loc);
    llvm::Function* F = builder->GetInsertBlock()->getParent();

    // END block
    auto* endBB = llvm::BasicBlock::Create(ctx, "switch.end", F);

    // Scrutinee to i32
    llvm::Value* scr = emitExpr(s->scrutinee.get(), scope);
    if (!scr->getType()->isIntegerTy(32)) scr = builder->CreateZExtOrTrunc(scr, llvm::Type::getInt32Ty(ctx), "switch.scr");

    // Prepare case and default blocks
    std::vector<std::pair<int, llvm::BasicBlock*>> caseBBs;
    caseBBs.reserve(s->cases.size());
    for (auto& c : s->cases) {
        auto* bb = llvm::BasicBlock::Create(ctx, std::string("switch.case.") + std::to_string(c.value), F);
        caseBBs.emplace_back(c.value, bb);
    }
    llvm::BasicBlock* defaultBB = s->deflt ? llvm::BasicBlock::Create(ctx, "switch.default", F) : endBB;

    // Create LLVM 'switch' dispatch
    auto* swi = builder->CreateSwitch(scr, defaultBB, (unsigned)caseBBs.size());
    for (auto& p : caseBBs) {
        swi->addCase(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), p.first), p.second);
    }

    // push destino de 'break' para todo o switch
    breakTargets.push_back(endBB);

    // Emit case bodies
    for (size_t i = 0; i < caseBBs.size(); ++i) {
        auto [val, caseBB] = caseBBs[i]; (void)val;
        builder->SetInsertPoint(caseBB);
        // calcula o destino de fallthrough: proximo case se houver; senao default, senao end
        llvm::BasicBlock* nextArm = (i + 1 < caseBBs.size()) ? caseBBs[i+1].second
                                    : (defaultBB != endBB ? defaultBB : endBB);
        fallthroughTargets.push_back(nextArm);
        emitBlock(s->cases[i].body.get(), scope);
        fallthroughTargets.pop_back();
        // Sem fallthrough implícito: se não terminou, salta para end
        if (!builder->GetInsertBlock()->getTerminator()) builder->CreateBr(endBB);
    }

    // Default body
    if (defaultBB != endBB) {
        builder->SetInsertPoint(defaultBB);
        fallthroughTargets.push_back(endBB);
        emitBlock(s->deflt.get(), scope);
        fallthroughTargets.pop_back();
        if (!builder->GetInsertBlock()->getTerminator()) builder->CreateBr(endBB);
    }

    breakTargets.pop_back();

    // Ensure insertion point is valid
    if (!endBB->getTerminator()) {
        builder->SetInsertPoint(endBB);
    } else {
        auto* contBB = llvm::BasicBlock::Create(ctx, "switch.cont", F);
        builder->SetInsertPoint(contBB);
    }
}

} // namespace mycc
