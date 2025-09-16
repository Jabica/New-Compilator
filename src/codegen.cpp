#include "codegen.hpp"
#include <cassert>
#include <string>
#include <llvm/IR/Intrinsics.h>

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

    // R2-11: placeholders para copy/fill (interceptamos no codegen de Call)
    if (!mod->getFunction("copy")) {
        auto *FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(ctx),
            { llvm::PointerType::get(ctx, 0), llvm::PointerType::get(ctx, 0), llvm::Type::getInt32Ty(ctx) },
            false);
        llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "copy", mod.get());
    }
    if (!mod->getFunction("fill")) {
        auto *FT = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(ctx),
            { llvm::PointerType::get(ctx, 0), llvm::Type::getInt32Ty(ctx), llvm::Type::getInt32Ty(ctx) },
            false);
        llvm::Function::Create(FT, llvm::Function::ExternalLinkage, "fill", mod.get());
    }
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
        // Açúcar: se faltou 1 índice (under-indexing 1D), tratar como copy/fill de slice
        if (idxs.size() + 1 == it->second.size()) {
            // constrói destino como slice contíguo 1D (linha)
            llvm::Value* offPrefix = linearizeOffset(it->second, idxs);
            llvm::Value* baseI32Ptr = nullptr;
            if (S->isGlobal && S->isArray) {
                auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                baseI32Ptr = builder->CreateInBoundsGEP(
                    llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(),
                    S->ptr,
                    std::array<llvm::Value*,2>{zero, offPrefix}, name + ".g.slice.base");
            } else {
                baseI32Ptr = builder->CreateInBoundsGEP(S->elemTy, S->ptr, offPrefix, name + ".slice.base");
            }
            Slice1D dst;
            dst.baseI8   = builder->CreateBitCast(baseI32Ptr, llvm::PointerType::get(ctx, 0));
            dst.lenElems = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), it->second.back());
            dst.strideB  = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 4);
            dst.elemBytes= 4;

            bool rhsIsSlice = (dynamic_cast<Index*>(ai->value.get()) != nullptr);
            bool rhsIsCol   = false;
            if (auto call = dynamic_cast<Call*>(ai->value.get())) rhsIsCol = (call->callee == "m_col" || call->callee == "slice");
            if (rhsIsSlice || rhsIsCol) {
                Slice1D src = getSlice1D(ai->value.get(), scope);
                if (!src.isValid()) { diag.error(0,0, "atribuicao de slice: rhs nao e view 1D"); return; }
                emitCopySliceSmart(dst, src);
                return;
            } else {
                llvm::Value* v = emitExpr(ai->value.get(), scope);
                emitFillSliceSmart(dst, v);
                return;
            }
        }
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
        // R2-11/12: intercepta built-ins copy/fill para emitir memcpy/memset (ou laços estridados)
        if (c->callee == "copy") {
            if (c->args.size() != 2) {
                diag.error(0,0, "copy espera 2 argumentos");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }
            Slice1D dst = getSlice1D(c->args[0].get(), scope);
            Slice1D src = getSlice1D(c->args[1].get(), scope);
            if (!dst.isValid() || !src.isValid()) {
                diag.error(0,0, "copy: views invalidos/nao-1D");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }
            emitCopySliceSmart(dst, src);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        if (c->callee == "fill") {
            if (c->args.size() != 2) {
                diag.error(0,0, "fill espera 2 argumentos");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }
            Slice1D dst = getSlice1D(c->args[0].get(), scope);
            if (!dst.isValid()) {
                diag.error(0,0, "fill: view invalido/nao-1D");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }
            llvm::Value* val = emitExpr(c->args[1].get(), scope);
            emitFillSliceSmart(dst, val);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        // R2-15: intrínsecas 2D geradas em IR
        if (c->callee == "copy2d") {
            if (c->args.size() != 8) {
                diag.error(0,0, "copy2d: aridade invalida (esperado 8)");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }
            auto* dstRef = dynamic_cast<VarRef*>(c->args[0].get());
            auto* srcRef = dynamic_cast<VarRef*>(c->args[3].get());
            if (!dstRef || !srcRef) {
                diag.error(0,0, "copy2d: primeiro e quarto argumentos devem ser variaveis de array");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }
            VarSlot* dstS = scope.lookup(dstRef->name);
            VarSlot* srcS = scope.lookup(srcRef->name);
            if (!dstS || !srcS) {
                diag.error(0,0, "copy2d: variavel nao declarada (dst ou src)");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }

            auto toI32 = [&](Expr* ex)->llvm::Value*{
                llvm::Value* v = emitExpr(ex, scope);
                if (!v->getType()->isIntegerTy(32)) v = builder->CreateZExtOrTrunc(v, llvm::Type::getInt32Ty(ctx));
                return v;
            };
            llvm::Value* dstStride = toI32(c->args[1].get());
            llvm::Value* dstIdx    = toI32(c->args[2].get());
            llvm::Value* srcStride = toI32(c->args[4].get());
            llvm::Value* srcIdx    = toI32(c->args[5].get());
            llvm::Value* cols      = toI32(c->args[6].get());
            llvm::Value* rows      = toI32(c->args[7].get());

            auto gepElem = [&](VarSlot* S, llvm::Value* off, const std::string& nm){
                if (S->isGlobal && S->isArray) {
                    auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                    return (llvm::Value*)builder->CreateInBoundsGEP(
                        llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(),
                        S->ptr, std::array<llvm::Value*,2>{zero, off}, nm);
                } else {
                    return (llvm::Value*)builder->CreateInBoundsGEP(S->elemTy, S->ptr, off, nm);
                }
            };

            llvm::Function* F = builder->GetInsertBlock()->getParent();
            auto* i32 = llvm::Type::getInt32Ty(ctx);
            auto* i64 = llvm::Type::getInt64Ty(ctx);
            auto* i8p = llvm::PointerType::get(ctx, 0);

            // R2-17: contiguous fast-path (single memcpy) when allowed
            bool tryContig = (fast2DMode == Fast2DMode::Always) || (fast2DMode == Fast2DMode::Auto);
            if (tryContig) {
                // cond1: cols == dstStride == srcStride
                llvm::Value* cols_eq_dst = builder->CreateICmpEQ(cols, dstStride);
                llvm::Value* cols_eq_src = builder->CreateICmpEQ(cols, srcStride);
                llvm::Value* cond1 = builder->CreateAnd(cols_eq_dst, cols_eq_src);
                // cond2: dstIdx % dstStride == 0 && srcIdx % srcStride == 0
                llvm::Value* dstRem = builder->CreateSRem(dstIdx, dstStride);
                llvm::Value* srcRem = builder->CreateSRem(srcIdx, srcStride);
                llvm::Value* dstAligned = builder->CreateICmpEQ(dstRem, llvm::ConstantInt::get(i32, 0));
                llvm::Value* srcAligned = builder->CreateICmpEQ(srcRem, llvm::ConstantInt::get(i32, 0));
                llvm::Value* cond2 = builder->CreateAnd(dstAligned, srcAligned);
                llvm::Value* contiguous = builder->CreateAnd(cond1, cond2);

                // Se a condição é constante em tempo de compilação, evite gerar o caminho alternativo
                if (auto C = llvm::dyn_cast<llvm::ConstantInt>(contiguous)) {
                    if (C->isOne()) {
                        // Somente caminho contíguo
                        llvm::Value* dstBasePtr = gepElem(dstS, dstIdx, "copy2d.dst.base");
                        llvm::Value* srcBasePtr = gepElem(srcS, srcIdx, "copy2d.src.base");
                        llvm::Value* dstI8 = builder->CreateBitCast(dstBasePtr, i8p);
                        llvm::Value* srcI8 = builder->CreateBitCast(srcBasePtr, i8p);
                        llvm::Value* elems = builder->CreateMul(rows, cols);
                        llvm::Value* bytes32 = builder->CreateMul(elems, llvm::ConstantInt::get(i32, 4));
                        llvm::Value* bytes64 = builder->CreateZExt(bytes32, i64);
                        auto align4 = llvm::MaybeAlign(4);
                        builder->CreateMemCpy(dstI8, align4, srcI8, align4, bytes64);
                        return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                    } else {
                        // Somente fallback por linha (gera apenas esse caminho)
                        auto* rCond = llvm::BasicBlock::Create(ctx, "copy2d.r.cond", F);
                        auto* rBody = llvm::BasicBlock::Create(ctx, "copy2d.r.body");
                        auto* rInc  = llvm::BasicBlock::Create(ctx, "copy2d.r.inc");
                        auto* rEnd  = llvm::BasicBlock::Create(ctx, "copy2d.after");

                        // r = 0
                        llvm::AllocaInst* rVar = createEntryAlloca(F, i32, "r");
                        builder->CreateStore(llvm::ConstantInt::get(i32, 0), rVar);
                        builder->CreateBr(rCond);

                        F->insert(F->end(), rCond);
                        builder->SetInsertPoint(rCond);
                        llvm::Value* rVal = builder->CreateLoad(i32, rVar, "r.val");
                        llvm::Value* rCmp = builder->CreateICmpSLT(rVal, rows);
                        builder->CreateCondBr(rCmp, rBody, rEnd);

                        // r body: memcpy por linha
                        F->insert(F->end(), rBody);
                        builder->SetInsertPoint(rBody);
                        llvm::Value* rMulDst = builder->CreateMul(rVal, dstStride);
                        llvm::Value* dstLineOff = builder->CreateAdd(dstIdx, rMulDst);
                        llvm::Value* rMulSrc = builder->CreateMul(rVal, srcStride);
                        llvm::Value* srcLineOff = builder->CreateAdd(srcIdx, rMulSrc);
                        llvm::Value* dstLinePtr = gepElem(dstS, dstLineOff, "copy2d.dst.line.ptr");
                        llvm::Value* srcLinePtr = gepElem(srcS, srcLineOff, "copy2d.src.line.ptr");
                        llvm::Value* dstI8L = builder->CreateBitCast(dstLinePtr, i8p);
                        llvm::Value* srcI8L = builder->CreateBitCast(srcLinePtr, i8p);
                        llvm::Value* bytes32L = builder->CreateMul(cols, llvm::ConstantInt::get(i32, 4));
                        llvm::Value* bytes64L = builder->CreateZExt(bytes32L, i64);
                        builder->CreateMemCpy(dstI8L, llvm::MaybeAlign(4), srcI8L, llvm::MaybeAlign(4), bytes64L);
                        builder->CreateBr(rInc);

                        F->insert(F->end(), rInc);
                        builder->SetInsertPoint(rInc);
                        llvm::Value* rNext = builder->CreateAdd(rVal, llvm::ConstantInt::get(i32, 1));
                        builder->CreateStore(rNext, rVar);
                        builder->CreateBr(rCond);

                        F->insert(F->end(), rEnd);
                        builder->SetInsertPoint(rEnd);
                        return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                    }
                }

                auto* bbContig = llvm::BasicBlock::Create(ctx, "copy2d.contig", F);
                auto* bbRow    = llvm::BasicBlock::Create(ctx, "copy2d.row");
                auto* bbEnd    = llvm::BasicBlock::Create(ctx, "copy2d.end");
                builder->CreateCondBr(contiguous, bbContig, bbRow);

                // CONTIG: single memcpy
                builder->SetInsertPoint(bbContig);
                llvm::Value* dstBasePtr = gepElem(dstS, dstIdx, "copy2d.dst.base");
                llvm::Value* srcBasePtr = gepElem(srcS, srcIdx, "copy2d.src.base");
                llvm::Value* dstI8 = builder->CreateBitCast(dstBasePtr, i8p);
                llvm::Value* srcI8 = builder->CreateBitCast(srcBasePtr, i8p);
                llvm::Value* elems = builder->CreateMul(rows, cols);
                llvm::Value* bytes32 = builder->CreateMul(elems, llvm::ConstantInt::get(i32, 4));
                llvm::Value* bytes64 = builder->CreateZExt(bytes32, i64);
                auto align4 = llvm::MaybeAlign(4);
                builder->CreateMemCpy(dstI8, align4, srcI8, align4, bytes64);
                builder->CreateBr(bbEnd);

                // fallback: row-wise memcpy inside r loop
                F->insert(F->end(), bbRow);
                builder->SetInsertPoint(bbRow);
                // r loop
                auto* rCond = llvm::BasicBlock::Create(ctx, "copy2d.r.cond", F);
                auto* rBody = llvm::BasicBlock::Create(ctx, "copy2d.r.body");
                auto* rInc  = llvm::BasicBlock::Create(ctx, "copy2d.r.inc");
                // r = 0
                llvm::AllocaInst* rVar = createEntryAlloca(F, i32, "r");
                builder->CreateStore(llvm::ConstantInt::get(i32, 0), rVar);
                builder->CreateBr(rCond);

                F->insert(F->end(), rCond);
                builder->SetInsertPoint(rCond);
                llvm::Value* rVal = builder->CreateLoad(i32, rVar, "r.val");
                llvm::Value* rCmp = builder->CreateICmpSLT(rVal, rows);
                builder->CreateCondBr(rCmp, rBody, bbEnd);

                // r body: memcpy por linha
                F->insert(F->end(), rBody);
                builder->SetInsertPoint(rBody);
                llvm::Value* rMulDst = builder->CreateMul(rVal, dstStride);
                llvm::Value* dstLineOff = builder->CreateAdd(dstIdx, rMulDst);
                llvm::Value* rMulSrc = builder->CreateMul(rVal, srcStride);
                llvm::Value* srcLineOff = builder->CreateAdd(srcIdx, rMulSrc);
                llvm::Value* dstLinePtr = gepElem(dstS, dstLineOff, "copy2d.dst.line.ptr");
                llvm::Value* srcLinePtr = gepElem(srcS, srcLineOff, "copy2d.src.line.ptr");
                llvm::Value* dstI8L = builder->CreateBitCast(dstLinePtr, i8p);
                llvm::Value* srcI8L = builder->CreateBitCast(srcLinePtr, i8p);
                llvm::Value* bytes32L = builder->CreateMul(cols, llvm::ConstantInt::get(i32, 4));
                llvm::Value* bytes64L = builder->CreateZExt(bytes32L, i64);
                builder->CreateMemCpy(dstI8L, llvm::MaybeAlign(4), srcI8L, llvm::MaybeAlign(4), bytes64L);
                builder->CreateBr(rInc);

                F->insert(F->end(), rInc);
                builder->SetInsertPoint(rInc);
                llvm::Value* rNext = builder->CreateAdd(rVal, llvm::ConstantInt::get(i32, 1));
                builder->CreateStore(rNext, rVar);
                builder->CreateBr(rCond);

                F->insert(F->end(), bbEnd);
                builder->SetInsertPoint(bbEnd);
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }

            // r loop
            auto* rCond = llvm::BasicBlock::Create(ctx, "copy2d.r.cond", F);
            auto* rBody = llvm::BasicBlock::Create(ctx, "copy2d.r.body");
            auto* rInc  = llvm::BasicBlock::Create(ctx, "copy2d.r.inc");
            auto* rEnd  = llvm::BasicBlock::Create(ctx, "copy2d.after");

            // r = 0
            llvm::AllocaInst* rVar = createEntryAlloca(F, i32, "r");
            builder->CreateStore(llvm::ConstantInt::get(i32, 0), rVar);
            builder->CreateBr(rCond);

            F->insert(F->end(), rCond);
            builder->SetInsertPoint(rCond);
            llvm::Value* rVal = builder->CreateLoad(i32, rVar, "r.val");
            llvm::Value* rCmp = builder->CreateICmpSLT(rVal, rows);
            builder->CreateCondBr(rCmp, rBody, rEnd);

            // r body: memcpy por linha (cols inteiros => bytes = cols*4)
            F->insert(F->end(), rBody);
            builder->SetInsertPoint(rBody);
            // offsets de linha
            llvm::Value* rMulDst = builder->CreateMul(rVal, dstStride);
            llvm::Value* dstLineOff = builder->CreateAdd(dstIdx, rMulDst);
            llvm::Value* rMulSrc = builder->CreateMul(rVal, srcStride);
            llvm::Value* srcLineOff = builder->CreateAdd(srcIdx, rMulSrc);

            // GEP para primeiro elemento da linha
            llvm::Value* dstLinePtr = gepElem(dstS, dstLineOff, "copy2d.dst.line.ptr");
            llvm::Value* srcLinePtr = gepElem(srcS, srcLineOff, "copy2d.src.line.ptr");
            // Bitcast para ptr genérico (opaque)
            auto* anyPtr = llvm::PointerType::get(ctx, 0);
            llvm::Value* dstI8 = builder->CreateBitCast(dstLinePtr, anyPtr);
            llvm::Value* srcI8 = builder->CreateBitCast(srcLinePtr, anyPtr);
            // bytes = cols * 4
            llvm::Value* bytes32 = builder->CreateMul(cols, llvm::ConstantInt::get(i32, 4));
            //auto* i64 = llvm::Type::getInt64Ty(ctx);
            llvm::Value* bytes64 = builder->CreateZExt(bytes32, i64);
            auto align4 = llvm::MaybeAlign(4);
            builder->CreateMemCpy(dstI8, align4, srcI8, align4, bytes64);
            builder->CreateBr(rInc);

            // r.inc
            F->insert(F->end(), rInc);
            builder->SetInsertPoint(rInc);
            llvm::Value* rNext = builder->CreateAdd(rVal, llvm::ConstantInt::get(i32, 1));
            builder->CreateStore(rNext, rVar);
            builder->CreateBr(rCond);

            // end
            F->insert(F->end(), rEnd);
            builder->SetInsertPoint(rEnd);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
        if (c->callee == "fill2d") {
            if (c->args.size() != 6) {
                diag.error(0,0, "fill2d: aridade invalida (esperado 6)");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }
            auto* dstRef = dynamic_cast<VarRef*>(c->args[0].get());
            if (!dstRef) {
                diag.error(0,0, "fill2d: primeiro argumento deve ser variavel de array");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }
            VarSlot* dstS = scope.lookup(dstRef->name);
            if (!dstS) {
                diag.error(0,0, "fill2d: variavel nao declarada (dst)");
                return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            }
            auto toI32 = [&](Expr* ex)->llvm::Value*{
                llvm::Value* v = emitExpr(ex, scope);
                if (!v->getType()->isIntegerTy(32)) v = builder->CreateZExtOrTrunc(v, llvm::Type::getInt32Ty(ctx));
                return v;
            };
            llvm::Value* dstStride = toI32(c->args[1].get());
            llvm::Value* dstIdx    = toI32(c->args[2].get());
            llvm::Value* value     = toI32(c->args[3].get());
            llvm::Value* cols      = toI32(c->args[4].get());
            llvm::Value* rows      = toI32(c->args[5].get());

            auto gepElem = [&](VarSlot* S, llvm::Value* off, const std::string& nm){
                if (S->isGlobal && S->isArray) {
                    auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                    return (llvm::Value*)builder->CreateInBoundsGEP(
                        llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(),
                        S->ptr, std::array<llvm::Value*,2>{zero, off}, nm);
                } else {
                    return (llvm::Value*)builder->CreateInBoundsGEP(S->elemTy, S->ptr, off, nm);
                }
            };

            llvm::Function* F = builder->GetInsertBlock()->getParent();
            auto* i32 = llvm::Type::getInt32Ty(ctx);

            auto* rCond = llvm::BasicBlock::Create(ctx, "fill2d.r.cond", F);
            auto* rBody = llvm::BasicBlock::Create(ctx, "fill2d.r.body");
            auto* rInc  = llvm::BasicBlock::Create(ctx, "fill2d.r.inc");
            auto* rEnd  = llvm::BasicBlock::Create(ctx, "fill2d.after");

            llvm::AllocaInst* rVar = createEntryAlloca(F, i32, "r");
            builder->CreateStore(llvm::ConstantInt::get(i32, 0), rVar);
            builder->CreateBr(rCond);

            F->insert(F->end(), rCond);
            builder->SetInsertPoint(rCond);
            llvm::Value* rVal = builder->CreateLoad(i32, rVar, "r.val");
            llvm::Value* rCmp = builder->CreateICmpSLT(rVal, rows);
            builder->CreateCondBr(rCmp, rBody, rEnd);

            F->insert(F->end(), rBody);
            builder->SetInsertPoint(rBody);
            bool canUseMemsetZero = false;
            if (auto CI = llvm::dyn_cast<llvm::ConstantInt>(value)) { canUseMemsetZero = CI->isZero(); }
            if (canUseMemsetZero) {
                // memset por linha
                llvm::Value* rMul = builder->CreateMul(rVal, dstStride);
                llvm::Value* dstLineOff = builder->CreateAdd(dstIdx, rMul);
                llvm::Value* dstLinePtr = gepElem(dstS, dstLineOff, "fill2d.dst.line.ptr");
                auto* anyPtr = llvm::PointerType::get(ctx, 0);
                llvm::Value* dstI8 = builder->CreateBitCast(dstLinePtr, anyPtr);
                llvm::Value* bytes32 = builder->CreateMul(cols, llvm::ConstantInt::get(i32, 4));
                auto* i64 = llvm::Type::getInt64Ty(ctx);
                llvm::Value* bytes64 = builder->CreateZExt(bytes32, i64);
                auto align4 = llvm::MaybeAlign(4);
                builder->CreateMemSet(dstI8, llvm::ConstantInt::get(llvm::Type::getInt8Ty(ctx), 0), bytes64, align4);
                builder->CreateBr(rInc);
            } else {
                // R2-18: vetorização leve 4xi32 quando habilitado/seguro
                auto* i64 = llvm::Type::getInt64Ty(ctx);
                auto* v4i32 = llvm::FixedVectorType::get(i32, 4);
                llvm::MaybeAlign A4(4);

                // Ponteiro base da linha (i32*) = &dst[dstIdx + r*dstStride]
                llvm::Value* rMul = builder->CreateMul(rVal, dstStride);
                llvm::Value* dstLineOff = builder->CreateAdd(dstIdx, rMul);
                llvm::Value* rowBasePtr = gepElem(dstS, dstLineOff, "fill2d.dst.line.ptr");

                bool tryVec = (vec2DMode == Vec2DMode::Always) || (vec2DMode == Vec2DMode::Auto);
                if (tryVec) {
                    // nVec = cols / 4, tail = cols % 4
                    llvm::Value* four = llvm::ConstantInt::get(i32, 4);
                    llvm::Value* nVec  = builder->CreateSDiv(cols, four, "nVec");
                    llvm::Value* nTail = builder->CreateSRem(cols, four, "tail");
                    // tailBase = rowBasePtr + 4*nVec (vale mesmo se nVec==0)
                    llvm::Value* advElems = builder->CreateMul(nVec, four);
                    llvm::Value* tailBase = builder->CreateInBoundsGEP(i32, rowBasePtr, advElems, "tail.base");

                    llvm::Value* hasVec = builder->CreateICmpSGT(nVec, llvm::ConstantInt::get(i32, 0));

                    auto* bbVec = llvm::BasicBlock::Create(ctx, "fill2d.vec", F);
                    auto* bbSca = llvm::BasicBlock::Create(ctx, "fill2d.sca");
                    builder->CreateCondBr(hasVec, bbVec, bbSca);

                    // --- Loop vetorizado ---
                    builder->SetInsertPoint(bbVec);
                    // Splat de value para <4 x i32>
                    llvm::Value* VV = llvm::UndefValue::get(v4i32);
                    VV = builder->CreateInsertElement(VV, value, (uint64_t)0);
                    llvm::Value* mask0 = llvm::ConstantVector::getSplat(
                        llvm::ElementCount::getFixed(4), llvm::ConstantInt::get(i32, 0));
                    VV = builder->CreateShuffleVector(VV, llvm::UndefValue::get(v4i32), mask0);

                    // i = 0..nVec-1
                    auto* preBB = builder->GetInsertBlock();
                    auto* loopBB = llvm::BasicBlock::Create(ctx, "fill2d.vec.loop", F);
                    auto* exitBB = llvm::BasicBlock::Create(ctx, "fill2d.vec.exit");
                    builder->CreateBr(loopBB);

                    builder->SetInsertPoint(loopBB);
                    llvm::PHINode* phiI = builder->CreatePHI(i32, 2, "i");
                    phiI->addIncoming(llvm::ConstantInt::get(i32, 0), preBB);
                    // store VV em &rowBasePtr[ i*4 ]
                    llvm::Value* iMul4 = builder->CreateMul(phiI, four);
                    llvm::Value* vecDstPtr = builder->CreateInBoundsGEP(i32, rowBasePtr, iMul4);
                    auto* st = builder->CreateStore(VV, vecDstPtr);
                    // alinha conservadoramente em 4
                    if (auto* stInst = llvm::dyn_cast<llvm::StoreInst>(st)) {
                        stInst->setAlignment(llvm::Align(4));
                    }
                    // i++ e cond
                    llvm::Value* i1 = builder->CreateAdd(phiI, llvm::ConstantInt::get(i32, 1));
                    llvm::Value* cond = builder->CreateICmpSLT(i1, nVec);
                    phiI->addIncoming(i1, builder->GetInsertBlock());
                    builder->CreateCondBr(cond, loopBB, exitBB);

                    F->insert(F->end(), exitBB);
                    builder->SetInsertPoint(exitBB);
                    // Vai para escalar/tail
                    F->insert(F->end(), bbSca);
                    builder->CreateBr(bbSca);

                    // --- Tail escalar (0..tail-1) ---
                    builder->SetInsertPoint(bbSca);
                    llvm::Value* hasTail = builder->CreateICmpSGT(nTail, llvm::ConstantInt::get(i32, 0));
                    auto* bbTail = llvm::BasicBlock::Create(ctx, "fill2d.tail", F);
                    auto* bbDone = llvm::BasicBlock::Create(ctx, "fill2d.after.line", F);
                    builder->CreateCondBr(hasTail, bbTail, bbDone);

                    builder->SetInsertPoint(bbTail);
                    llvm::PHINode* j = builder->CreatePHI(i32, 2, "j");
                    j->addIncoming(llvm::ConstantInt::get(i32, 0), bbSca);
                    llvm::Value* tPtr = builder->CreateInBoundsGEP(i32, tailBase, j);
                    builder->CreateStore(value, tPtr);
                    llvm::Value* j1 = builder->CreateAdd(j, llvm::ConstantInt::get(i32, 1));
                    llvm::Value* jt = builder->CreateICmpSLT(j1, nTail);
                    j->addIncoming(j1, builder->GetInsertBlock());
                    builder->CreateCondBr(jt, bbTail, bbDone);

                    builder->SetInsertPoint(bbDone);
                    builder->CreateBr(rInc);
                } else {
                    // fallback: laço escalar por coluna
                    auto* cCond = llvm::BasicBlock::Create(ctx, "fill2d.c.cond", F);
                    auto* cBody = llvm::BasicBlock::Create(ctx, "fill2d.c.body");
                    auto* cInc  = llvm::BasicBlock::Create(ctx, "fill2d.c.inc");
                    llvm::AllocaInst* cVar = createEntryAlloca(F, i32, "c");
                    builder->CreateStore(llvm::ConstantInt::get(i32, 0), cVar);
                    builder->CreateBr(cCond);

                    builder->SetInsertPoint(cCond);
                    llvm::Value* cVal = builder->CreateLoad(i32, cVar, "c.val");
                    llvm::Value* cCmp = builder->CreateICmpSLT(cVal, cols);
                    builder->CreateCondBr(cCmp, cBody, rInc);

                    F->insert(F->end(), cBody);
                    builder->SetInsertPoint(cBody);
                    llvm::Value* off  = builder->CreateAdd(dstLineOff, cVal);
                    llvm::Value* dstPtr = gepElem(dstS, off, "fill2d.dst.ptr");
                    builder->CreateStore(value, dstPtr);

                    builder->CreateBr(cInc);
                    F->insert(F->end(), cInc);
                    builder->SetInsertPoint(cInc);
                    llvm::Value* cNext = builder->CreateAdd(cVal, llvm::ConstantInt::get(i32, 1));
                    builder->CreateStore(cNext, cVar);
                    builder->CreateBr(cCond);
                }
            }

            F->insert(F->end(), rInc);
            builder->SetInsertPoint(rInc);
            llvm::Value* rNext = builder->CreateAdd(rVal, llvm::ConstantInt::get(i32, 1));
            builder->CreateStore(rNext, rVar);
            builder->CreateBr(rCond);

            F->insert(F->end(), rEnd);
            builder->SetInsertPoint(rEnd);
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }

        // R2-13: slice/transpose são tokens reconhecidos em getSlice1D/Index; aqui retornamos dummy
        if (c->callee == "slice" || c->callee == "transpose") {
            if (c->callee == "slice" && c->args.size() != 4) diag.error(0,0, "slice: use slice(view,start,len,step)");
            if (c->callee == "transpose" && c->args.size() != 1) diag.error(0,0, "transpose: aridade invalida");
            return ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        }
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
// R2-11: obtém view contiguo 1D (i8* base + tamanho em bytes) a partir de um Index/VarRef
namespace mycc {
Codegen::ViewInfo Codegen::getContiguous1DView(Expr* e, Scope& scope) {
    ViewInfo vi;

    // Caso: cadeia de Index ou VarRef
    std::string name;
    std::vector<llvm::Value*> idxs;
    if (auto ix = dynamic_cast<Index*>(e)) {
        auto pair = flattenIndexChain(e, scope);
        name = pair.first;
        idxs = std::move(pair.second);
    } else if (auto vr = dynamic_cast<VarRef*>(e)) {
        name = vr->name;
    } else {
        return vi; // nao reconhecido
    }

    if (name.empty()) return vi;
    VarSlot* S = scope.lookup(name);
    if (!S) return vi;
    auto it = arrayDimsByName.find(name);
    if (it == arrayDimsByName.end()) return vi;
    const std::vector<int>& dims = it->second;
    size_t rank = dims.size();

    // view contiguo 1D quando: (a) rank==1 e idxs.size()==0 (vetor inteiro), ou (b) idxs.size()==rank-1
    if (!((rank == 1 && idxs.size() == 0) || (rank >= 1 && idxs.size() + 1 == rank))) {
        return vi;
    }

    llvm::Value* off = nullptr;
    if (!idxs.empty()) off = linearizeOffset(dims, idxs);
    else off = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);

    llvm::Value* baseI32Ptr = nullptr;
    if (S->isGlobal && S->isArray) {
        auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
        baseI32Ptr = builder->CreateInBoundsGEP(
            llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(), S->ptr,
            std::array<llvm::Value*,2>{zero, off}, name + ".g.slice.base");
    } else {
        baseI32Ptr = builder->CreateInBoundsGEP(S->elemTy, S->ptr, off, name + ".slice.base");
    }
    llvm::Value* baseI8 = builder->CreateBitCast(baseI32Ptr, llvm::PointerType::get(ctx, 0));
    vi.basePtrI8 = baseI8;
    int lenElems = dims.back();
    vi.lenBytes = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), lenElems * 4);
    vi.elemBytes = 4;
    return vi;
}
} // namespace mycc

// R2-12: slices 1D com stride (linhas/colunas via helper m_col)
namespace mycc {
Codegen::Slice1D Codegen::getSlice1D(Expr* e, Scope& scope) {
    Slice1D s;
    // transpose(m)[j]
    if (auto ix = dynamic_cast<Index*>(e)) {
        if (auto callTr = dynamic_cast<Call*>(ix->base.get())) {
            if (callTr->callee == "transpose" && callTr->args.size() == 1) {
                if (auto vr = dynamic_cast<VarRef*>(callTr->args[0].get())) {
                    VarSlot* S = scope.lookup(vr->name);
                    if (!S) return s;
                    auto it = arrayDimsByName.find(vr->name);
                    if (it == arrayDimsByName.end() || it->second.size() != 2) return s;
                    int rows = it->second[0];
                    int cols = it->second[1];
                    llvm::Value* jV = emitExpr(ix->idx.get(), scope);
                    if (!jV->getType()->isIntegerTy(32)) jV = builder->CreateZExtOrTrunc(jV, llvm::Type::getInt32Ty(ctx));
                    llvm::Value* baseI32Ptr = nullptr;
                    if (S->isGlobal && S->isArray) {
                        auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                        baseI32Ptr = builder->CreateInBoundsGEP(
                            llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(), S->ptr,
                            std::array<llvm::Value*,2>{zero, jV}, vr->name + ".g.tcol.base");
                    } else {
                        baseI32Ptr = builder->CreateInBoundsGEP(S->elemTy, S->ptr, jV, vr->name + ".tcol.base");
                    }
                    s.baseI8   = builder->CreateBitCast(baseI32Ptr, llvm::PointerType::get(ctx, 0));
                    s.lenElems = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), rows);
                    s.strideB  = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), cols * 4);
                    s.elemBytes= 4;
                    return s;
                }
            }
        }
    }
    // slice(view,start,len,step)
    if (auto callS = dynamic_cast<Call*>(e)) {
        if (callS->callee == "slice" && callS->args.size() == 4) {
            Slice1D base = getSlice1D(callS->args[0].get(), scope);
            if (!base.isValid()) return s;
            auto toI32 = [&](Expr* ex){ llvm::Value* v=emitExpr(ex,scope); if(!v->getType()->isIntegerTy(32)) v=builder->CreateZExtOrTrunc(v, llvm::Type::getInt32Ty(ctx)); return v; };
            llvm::Value* start = toI32(callS->args[1].get());
            llvm::Value* len   = toI32(callS->args[2].get());
            llvm::Value* step  = toI32(callS->args[3].get());
            // validações triviais
            if (auto cst = llvm::dyn_cast<llvm::ConstantInt>(step)) {
                if (cst->getSExtValue() <= 0) { diag.error(0,0, "slice: step deve ser > 0"); return s; }
            }
            if (auto cstL = llvm::dyn_cast<llvm::ConstantInt>(len)) {
                if (cstL->getSExtValue() < 0) { diag.error(0,0, "slice: len deve ser >= 0"); return s; }
            }
            // offset inicial em bytes = start * base.strideB
            llvm::Value* startBytes = builder->CreateMul(start, base.strideB);
            s.baseI8   = builder->CreateInBoundsGEP(llvm::Type::getInt8Ty(ctx), base.baseI8, startBytes);
            s.lenElems = len;
            s.strideB  = builder->CreateMul(step, base.strideB);
            s.elemBytes= base.elemBytes;
            return s;
        }
    }
    if (auto call = dynamic_cast<Call*>(e)) {
        if (call->callee == "m_col" && call->args.size() == 2) {
            if (auto vr = dynamic_cast<VarRef*>(call->args[0].get())) {
                VarSlot* S = scope.lookup(vr->name);
                if (!S) return s;
                auto it = arrayDimsByName.find(vr->name);
                if (it == arrayDimsByName.end() || it->second.size() != 2) return s;
                int rows = it->second[0];
                int cols = it->second[1];
                llvm::Value* j = emitExpr(call->args[1].get(), scope);
                if (!j->getType()->isIntegerTy(32)) j = builder->CreateZExtOrTrunc(j, llvm::Type::getInt32Ty(ctx));
                llvm::Value* off = j;
                llvm::Value* baseI32Ptr = nullptr;
                if (S->isGlobal && S->isArray) {
                    auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
                    baseI32Ptr = builder->CreateInBoundsGEP(
                        llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(), S->ptr,
                        std::array<llvm::Value*,2>{zero, off}, vr->name + ".g.col.base");
                } else {
                    baseI32Ptr = builder->CreateInBoundsGEP(S->elemTy, S->ptr, off, vr->name + ".col.base");
                }
                s.baseI8   = builder->CreateBitCast(baseI32Ptr, llvm::PointerType::get(ctx, 0));
                s.lenElems = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), rows);
                s.strideB  = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), cols * 4);
                s.elemBytes= 4;
                return s;
            }
        }
    }
    std::string name;
    std::vector<llvm::Value*> idxs;
    if (auto ix = dynamic_cast<Index*>(e)) {
        auto pair = flattenIndexChain(e, scope);
        name = pair.first; idxs = std::move(pair.second);
    } else if (auto vr = dynamic_cast<VarRef*>(e)) {
        name = vr->name;
    } else {
        return s;
    }
    if (name.empty()) return s;
    VarSlot* S = scope.lookup(name); if (!S) return s;
    auto it = arrayDimsByName.find(name); if (it == arrayDimsByName.end()) return s;
    size_t rank = it->second.size(); if (rank == 0) return s;
    if (rank == 1 && idxs.empty()) {
        llvm::Value* baseI32Ptr = nullptr;
        if (S->isGlobal && S->isArray) {
            auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            baseI32Ptr = builder->CreateInBoundsGEP(
                llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(), S->ptr,
                std::array<llvm::Value*,2>{zero, zero}, name + ".g.base");
        } else {
            baseI32Ptr = S->ptr;
        }
        s.baseI8   = builder->CreateBitCast(baseI32Ptr, llvm::PointerType::get(ctx, 0));
        s.lenElems = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), it->second[0]);
        s.strideB  = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 4);
        s.elemBytes= 4; return s;
    }
    if (idxs.size() + 1 == rank) {
        llvm::Value* off = linearizeOffset(it->second, idxs);
        llvm::Value* baseI32Ptr = nullptr;
        if (S->isGlobal && S->isArray) {
            auto* zero = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            baseI32Ptr = builder->CreateInBoundsGEP(
                llvm::cast<llvm::GlobalVariable>(S->ptr)->getValueType(), S->ptr,
                std::array<llvm::Value*,2>{zero, off}, name + ".g.row.base");
        } else {
            baseI32Ptr = builder->CreateInBoundsGEP(S->elemTy, S->ptr, off, name + ".row.base");
        }
        s.baseI8   = builder->CreateBitCast(baseI32Ptr, llvm::PointerType::get(ctx, 0));
        s.lenElems = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), it->second.back());
        s.strideB  = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 4);
        s.elemBytes= 4; return s;
    }
    return s;
}

void Codegen::emitCopySlice(const Slice1D& dst, const Slice1D& src) {
    emitCopySliceSmart(dst, src);
}

void Codegen::emitFillSlice(const Slice1D& dst, llvm::Value* v) {
    emitFillSliceSmart(dst, v);
}

// R2-14: Smart copy (fast-path memcpy/memset + unrollx4)
void Codegen::emitCopySliceSmart(const Slice1D& D, const Slice1D& S) {
    assert(D.isValid() && S.isValid());
    auto* i32 = llvm::Type::getInt32Ty(ctx);
    auto* i64 = llvm::Type::getInt64Ty(ctx);
    // runtime contiguity: stride == elemBytes
    llvm::Value* isContigDst = builder->CreateICmpEQ(D.strideB, llvm::ConstantInt::get(i32, (int)D.elemBytes));
    llvm::Value* isContigSrc = builder->CreateICmpEQ(S.strideB, llvm::ConstantInt::get(i32, (int)S.elemBytes));
    llvm::Value* isContig    = builder->CreateAnd(isContigDst, isContigSrc);

    llvm::Function* F = builder->GetInsertBlock()->getParent();
    auto* fastBB = llvm::BasicBlock::Create(ctx, "slice.fast", F);
    auto* slowBB = llvm::BasicBlock::Create(ctx, "slice.slow");
    auto* endBB  = llvm::BasicBlock::Create(ctx, "slice.end");
    builder->CreateCondBr(isContig, fastBB, slowBB);

    // FAST
    builder->SetInsertPoint(fastBB);
    llvm::Value* len64  = builder->CreateZExt(D.lenElems, i64);
    llvm::Value* eBytes = llvm::ConstantInt::get(i64, (int)D.elemBytes);
    llvm::Value* nBytes = builder->CreateMul(len64, eBytes);
    auto align = llvm::MaybeAlign(D.elemBytes);
    builder->CreateMemCpy(D.baseI8, align, S.baseI8, align, nBytes);
    builder->CreateBr(endBB);

    // SLOW: unrollx4 + tail
    F->insert(F->end(), slowBB);
    builder->SetInsertPoint(slowBB);
    auto* idx = builder->CreateAlloca(i32, nullptr, "idx");
    builder->CreateStore(llvm::ConstantInt::get(i32, 0), idx);

    auto offB = [&](llvm::Value* ii, llvm::Value* stride) {
        auto* off32 = builder->CreateMul(ii, stride);
        return builder->CreateZExt(off32, i64);
    };
    auto loadElem = [&](llvm::Value* baseI8, llvm::Value* off) {
        auto* p = builder->CreateInBoundsGEP(llvm::Type::getInt8Ty(ctx), baseI8, off);
        auto* p32 = builder->CreateBitCast(p, llvm::PointerType::get(llvm::Type::getInt32Ty(ctx), 0));
        return builder->CreateLoad(llvm::Type::getInt32Ty(ctx), p32);
    };
    auto storeElem = [&](llvm::Value* baseI8, llvm::Value* off, llvm::Value* v) {
        auto* p = builder->CreateInBoundsGEP(llvm::Type::getInt8Ty(ctx), baseI8, off);
        auto* p32 = builder->CreateBitCast(p, llvm::PointerType::get(llvm::Type::getInt32Ty(ctx), 0));
        builder->CreateStore(v, p32);
    };

    auto* unrollCond = llvm::BasicBlock::Create(ctx, "slice.copy.unroll.cond", F);
    auto* unrollBody = llvm::BasicBlock::Create(ctx, "slice.copy.unroll.body");
    auto* tailCond   = llvm::BasicBlock::Create(ctx, "slice.copy.tail.cond");
    auto* tailBody   = llvm::BasicBlock::Create(ctx, "slice.copy.tail.body");
    auto* slowEnd    = llvm::BasicBlock::Create(ctx, "slice.slow.end");

    builder->CreateBr(unrollCond);
    builder->SetInsertPoint(unrollCond);
    auto* i0 = builder->CreateLoad(i32, idx);
    auto* iPlus4 = builder->CreateAdd(i0, llvm::ConstantInt::get(i32, 4));
    auto* canUnroll = builder->CreateICmpSLE(iPlus4, D.lenElems);
    builder->CreateCondBr(canUnroll, unrollBody, tailCond);

    // Unrolled body
    F->insert(F->end(), unrollBody);
    builder->SetInsertPoint(unrollBody);
    auto* j0 = i0;
    auto* j1 = builder->CreateAdd(i0, llvm::ConstantInt::get(i32, 1));
    auto* j2 = builder->CreateAdd(i0, llvm::ConstantInt::get(i32, 2));
    auto* j3 = builder->CreateAdd(i0, llvm::ConstantInt::get(i32, 3));
    auto* so0 = offB(j0, S.strideB); auto* do0 = offB(j0, D.strideB);
    auto* so1 = offB(j1, S.strideB); auto* do1 = offB(j1, D.strideB);
    auto* so2 = offB(j2, S.strideB); auto* do2 = offB(j2, D.strideB);
    auto* so3 = offB(j3, S.strideB); auto* do3 = offB(j3, D.strideB);
    auto* v0 = loadElem(S.baseI8, so0);
    auto* v1 = loadElem(S.baseI8, so1);
    auto* v2 = loadElem(S.baseI8, so2);
    auto* v3 = loadElem(S.baseI8, so3);
    storeElem(D.baseI8, do0, v0);
    storeElem(D.baseI8, do1, v1);
    storeElem(D.baseI8, do2, v2);
    storeElem(D.baseI8, do3, v3);
    builder->CreateStore(iPlus4, idx);
    builder->CreateBr(unrollCond);

    // Tail loop
    F->insert(F->end(), tailCond);
    builder->SetInsertPoint(tailCond);
    auto* it = builder->CreateLoad(i32, idx);
    auto* contTail = builder->CreateICmpSLT(it, D.lenElems);
    builder->CreateCondBr(contTail, tailBody, slowEnd);

    F->insert(F->end(), tailBody);
    builder->SetInsertPoint(tailBody);
    auto* offS = offB(it, S.strideB);
    auto* offD = offB(it, D.strideB);
    auto* vv   = loadElem(S.baseI8, offS);
    storeElem(D.baseI8, offD, vv);
    auto* itNext = builder->CreateAdd(it, llvm::ConstantInt::get(i32, 1));
    builder->CreateStore(itNext, idx);
    builder->CreateBr(tailCond);

    F->insert(F->end(), slowEnd);
    builder->SetInsertPoint(slowEnd);
    builder->CreateBr(endBB);

    F->insert(F->end(), endBB);
    builder->SetInsertPoint(endBB);
}

void Codegen::emitFillSliceSmart(const Slice1D& D, llvm::Value* scalar32) {
    assert(D.isValid());
    auto* i32 = llvm::Type::getInt32Ty(ctx);
    auto* i64 = llvm::Type::getInt64Ty(ctx);
    llvm::Value* isContig = builder->CreateICmpEQ(D.strideB, llvm::ConstantInt::get(i32, (int)D.elemBytes));
    llvm::Function* F = builder->GetInsertBlock()->getParent();
    auto* fastBB = llvm::BasicBlock::Create(ctx, "slice.fast", F);
    auto* slowBB = llvm::BasicBlock::Create(ctx, "slice.slow");
    auto* endBB  = llvm::BasicBlock::Create(ctx, "slice.end");
    builder->CreateCondBr(isContig, fastBB, slowBB);

    // FAST memset
    builder->SetInsertPoint(fastBB);
    if (!scalar32->getType()->isIntegerTy(32)) scalar32 = builder->CreateZExtOrTrunc(scalar32, i32);
    auto* v8 = builder->CreateTrunc(scalar32, llvm::Type::getInt8Ty(ctx));
    llvm::Value* len64  = builder->CreateZExt(D.lenElems, i64);
    llvm::Value* eBytes = llvm::ConstantInt::get(i64, (int)D.elemBytes);
    llvm::Value* nBytes = builder->CreateMul(len64, eBytes);
    auto align = llvm::MaybeAlign(D.elemBytes);
    builder->CreateMemSet(D.baseI8, v8, nBytes, align);
    builder->CreateBr(endBB);

    // SLOW: unrollx4 + tail
    F->insert(F->end(), slowBB);
    builder->SetInsertPoint(slowBB);
    auto* idx = builder->CreateAlloca(i32, nullptr, "idx");
    builder->CreateStore(llvm::ConstantInt::get(i32, 0), idx);
    if (!scalar32->getType()->isIntegerTy(32)) scalar32 = builder->CreateZExtOrTrunc(scalar32, i32);

    auto offB = [&](llvm::Value* ii, llvm::Value* stride) {
        auto* off32 = builder->CreateMul(ii, stride);
        return builder->CreateZExt(off32, i64);
    };
    auto storeElem = [&](llvm::Value* baseI8, llvm::Value* off, llvm::Value* v) {
        auto* p = builder->CreateInBoundsGEP(llvm::Type::getInt8Ty(ctx), baseI8, off);
        auto* p32 = builder->CreateBitCast(p, llvm::PointerType::get(llvm::Type::getInt32Ty(ctx), 0));
        builder->CreateStore(v, p32);
    };

    auto* unrollCond = llvm::BasicBlock::Create(ctx, "slice.fill.unroll.cond", F);
    auto* unrollBody = llvm::BasicBlock::Create(ctx, "slice.fill.unroll.body");
    auto* tailCond   = llvm::BasicBlock::Create(ctx, "slice.fill.tail.cond");
    auto* tailBody   = llvm::BasicBlock::Create(ctx, "slice.fill.tail.body");
    auto* slowEnd    = llvm::BasicBlock::Create(ctx, "slice.slow.end");

    builder->CreateBr(unrollCond);
    builder->SetInsertPoint(unrollCond);
    auto* i0 = builder->CreateLoad(i32, idx);
    auto* iPlus4 = builder->CreateAdd(i0, llvm::ConstantInt::get(i32, 4));
    auto* canUnroll = builder->CreateICmpSLE(iPlus4, D.lenElems);
    builder->CreateCondBr(canUnroll, unrollBody, tailCond);

    F->insert(F->end(), unrollBody);
    builder->SetInsertPoint(unrollBody);
    auto* j0 = i0;
    auto* j1 = builder->CreateAdd(i0, llvm::ConstantInt::get(i32, 1));
    auto* j2 = builder->CreateAdd(i0, llvm::ConstantInt::get(i32, 2));
    auto* j3 = builder->CreateAdd(i0, llvm::ConstantInt::get(i32, 3));
    auto* do0 = offB(j0, D.strideB);
    auto* do1 = offB(j1, D.strideB);
    auto* do2 = offB(j2, D.strideB);
    auto* do3 = offB(j3, D.strideB);
    storeElem(D.baseI8, do0, scalar32);
    storeElem(D.baseI8, do1, scalar32);
    storeElem(D.baseI8, do2, scalar32);
    storeElem(D.baseI8, do3, scalar32);
    builder->CreateStore(iPlus4, idx);
    builder->CreateBr(unrollCond);

    F->insert(F->end(), tailCond);
    builder->SetInsertPoint(tailCond);
    auto* it = builder->CreateLoad(i32, idx);
    auto* contTail = builder->CreateICmpSLT(it, D.lenElems);
    builder->CreateCondBr(contTail, tailBody, slowEnd);

    F->insert(F->end(), tailBody);
    builder->SetInsertPoint(tailBody);
    auto* offD = offB(it, D.strideB);
    storeElem(D.baseI8, offD, scalar32);
    auto* itNext = builder->CreateAdd(it, llvm::ConstantInt::get(i32, 1));
    builder->CreateStore(itNext, idx);
    builder->CreateBr(tailCond);

    F->insert(F->end(), slowEnd);
    builder->SetInsertPoint(slowEnd);
    builder->CreateBr(endBB);

    F->insert(F->end(), endBB);
    builder->SetInsertPoint(endBB);
}
} // namespace mycc
