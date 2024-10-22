//
// Created by will on 10/27/24.
//
module;
#include <llvm/Support/SMLoc.h>
#include <memory>
module funLangAST;
import Basic;
import :Expr;

namespace funLang {
  ParamDecl::ParamDecl(IDTableEntry *Name,
                       u_ptr<Expr> Init,
                       DeclContext *Container,
                       u_ptr<ParamDecl> Next,
                       const llvm::SMLoc Left,
                       const llvm::SMLoc Right,
                       u_ptr<TypeUse> T) : Decl(DK_PARAM, Name, Left, Right, std::move(Next)), Init(std::move(Init)),
                                           Container(Container), T(std::move(T)) {
  };

  FunctionDecl::FunctionDecl(u_ptr<TypeUse> retType,
                             IDTableEntry *Name,
                             u_ptr<CompoundStmt> Statements,
                             const llvm::SMLoc TypeLoc,
                             const llvm::SMLoc RLoc,
                             u_ptr<ParamDecl> Params,
                             DeclContext *ParentCtx) : Decl(DK_FN, Name, TypeLoc, RLoc), DeclContext(ParentCtx),
                                                       ReturnType(std::move(retType)),
                                                       Statements(std::move(Statements)), Params(std::move(Params)) {
  }

  FunctionDecl::~FunctionDecl() = default;
}
