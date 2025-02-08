//
// Created by will on 10/27/24.
//
module;
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>
#include <memory>

module AST;
import Basic;

namespace funLang {
ifStmt::ifStmt(const llvm::SMLoc IfLoc, u_ptr<Stmt> cond,
               u_ptr<CompoundStmt> block1, u_ptr<elifStmt> elif,
               u_ptr<CompoundStmt> block2)
    : Stmt(SK_IF, IfLoc), Condition(std::move(cond)), Block(std::move(block1)),
      elif (std::move(elif)), ElseBlock(std::move(block2)) {}

elifStmt::elifStmt(const llvm::SMLoc Loc, u_ptr<Stmt> cond,
                   u_ptr<CompoundStmt> block, u_ptr<elifStmt> elif)
    : Stmt(SK_ELIF, Loc), Cond(std::move(cond)), NextElif(std::move(elif)),
      Block(std::move(block)) {}
elifStmt::~elifStmt() = default;

DeclStmt::DeclStmt(const llvm::SMLoc NameStart, const llvm::SMLoc SemiLoc,
                   VarDecl *NamedDecl, std::unique_ptr<Stmt> Expression)
    : Stmt(SK_DECL, NameStart, SemiLoc), NamedDecl(NamedDecl),
      Init(std::move(Expression)) {}

llvm::StringRef DeclStmt::getName() const { return NamedDecl->getName(); }
}// namespace funLang
