//
// Created by will on 10/27/24.
//
module;
#include "llvm/Support/SMLoc.h"
module AST;
import Basic;

namespace funLang {

FunctionDecl::FunctionDecl(u_ptr<TypeUse> retType, Symbol *Name,
                           u_ptr<CompoundStmt> Statements,
                           const llvm::SMLoc TypeLoc, const llvm::SMLoc RLoc,
                           u_ptr<ParamDecl> Params, DeclContext *ParentCtx)
    : Decl(DK_FN, Name, TypeLoc, RLoc), DeclContext(ParentCtx),
      Body(std::move(Statements)), ReturnType(std::move(retType)),
      Params(std::move(Params)) {}

auto FunctionDecl::Create(u_ptr<TypeUse> retType, Symbol *Name,
                          u_ptr<CompoundStmt> Statements,
                          u_ptr<ParamDecl> Params, DeclContext *ParentCtx) {
  return u_ptr<FunctionDecl>(
      new FunctionDecl(std::move(retType), Name, std::move(Statements),
                       retType->getTypeNameLoc(), Statements->getEndLoc(),
                       std::move(Params), ParentCtx));
}

}// namespace funLang
