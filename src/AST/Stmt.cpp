//
// Created by will on 10/27/24.
//
module;
#include <llvm/Support/SMLoc.h>
#include <memory>
#include <llvm/ADT/StringRef.h>

module AST;
import Basic;

namespace funLang {
  ifStmt::ifStmt(const llvm::SMLoc IfLoc,
                 u_ptr<Stmt> cond,
                 u_ptr<CompoundStmt> block1,
                 u_ptr<elifStmt> elif,
                 u_ptr<CompoundStmt> block2) : Stmt(SK_IF, IfLoc),
                                               Condition(std::move(cond)), Block(std::move(block1)),
                                               elif(std::move(elif)), ElseBlock(std::move(block2)) {
  }
  elifStmt::elifStmt(const llvm::SMLoc Loc,
                     u_ptr<Stmt> cond,
                     u_ptr<CompoundStmt> block,
                     u_ptr<elifStmt> elif) : Stmt(SK_ELIF, Loc), Cond(std::move(cond)), NextElif(std::move(elif)),
                                             Block(std::move(block)) {
  }
  elifStmt::~elifStmt() = default;

  forStmt::forStmt(const llvm::SMLoc EndLoc,
                   const llvm::SMLoc loc,
                   u_ptr<Stmt> Init,
                   u_ptr<Stmt> Cond,
                   u_ptr<Stmt> Inc,
                   u_ptr<CompoundStmt> Compound) : loopStmt(SK_FOR, std::move(Compound), loc, EndLoc),
                                                   Init(std::move(Init)), Cond(std::move(Cond)),
                                                   Inc(std::move(Inc)) {
  }

  whileStmt::whileStmt(u_ptr<Stmt> Condition,
                       u_ptr<CompoundStmt> Compound,
                       const llvm::SMLoc whileLoc,
                       const llvm::SMLoc endLoc) : loopStmt(SK_WHILE, std::move(Compound), whileLoc, endLoc),
                                                   Condition(std::move(Condition)) {
  }

  DeclStmt::DeclStmt(const llvm::SMLoc NameStart,
                     const llvm::SMLoc SemiLoc,
                     VarDecl *NamedDecl,
                     std::unique_ptr<Stmt> Expression) : Stmt(SK_DECL, NameStart, SemiLoc), NamedDecl(NamedDecl),
                                                         Init(std::move(Expression)) {
  }

  llvm::StringRef DeclStmt::getName() const { return NamedDecl->getName(); }
} // namespace funLang
