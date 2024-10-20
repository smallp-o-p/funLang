//
// Created by will on 7/24/24.
//
#include "AST/Stmt.hpp"
#include "AST/Decl.hpp"
#include "AST/Type.hpp"
using namespace funLang;
llvm::StringRef funLang::DeclStmt::getName() { return NamedDecl->getName(); }

DeclStmt::DeclStmt(llvm::SMLoc NameStart, llvm::SMLoc SemiLoc, VarDecl *NamedDecl, std::unique_ptr<Expr> Expression)
	: NamedDecl(NamedDecl), Init(std::move(Expression)),
	  Stmt(SK_VARDECL, NameStart, SemiLoc) {}

Stmt::Stmt(funLang::Stmt::StmtKind k, llvm::SMLoc loc) : kind(k) {}

elifStmt::elifStmt(llvm::SMLoc Loc,
				   std::unique_ptr<Expr> cond,
				   std::unique_ptr<CompoundStmt> block,
				   std::unique_ptr<elifStmt> elif)
	: Stmt(SK_ELIF, Loc), cond(std::move(cond)), Block(std::move(block)),
	  NextElif(std::move(elif)) {}

ifStmt::ifStmt(llvm::SMLoc IfLoc,
			   std::unique_ptr<Expr> cond,
			   llvm::SMLoc EndOfCondExpr,
			   std::unique_ptr<CompoundStmt> block1,
			   std::unique_ptr<elifStmt> elif,
			   std::unique_ptr<CompoundStmt> block2)
	: Condition(std::move(cond)), Block(std::move(block1)),
	  elif(std::move(elif)), ElseBlock(std::move(block2)), Stmt(SK_IF, IfLoc) {}

forStmt::forStmt(llvm::SMLoc EndLoc,
				 llvm::SMLoc loc,
				 std::unique_ptr<Stmt> Init,
				 std::unique_ptr<Expr> Cond,
				 std::unique_ptr<Expr> Inc,
				 std::unique_ptr<CompoundStmt> Compound)
	: Stmt(SK_FOR, loc, EndLoc), Init(std::move(Init)), Cond(std::move(Cond)),
	  Inc(std::move(Inc)), Compound(std::move(Compound)) {}

whileStmt::whileStmt(std::unique_ptr<Expr> condition,
					 std::unique_ptr<CompoundStmt> compound,
					 llvm::SMLoc whileLoc,
					 llvm::SMLoc endLoc)
	: compound(std::move(compound)), condition(std::move(condition)),
	  Stmt(SK_WHILE, whileLoc, endLoc) {}

ReturnStmt::ReturnStmt(std::unique_ptr<Expr> exprNode)
	: ReturnExpr(std::move(exprNode)), Stmt(SK_RETURN) {}

bool Expr::isAssignable() {
  return llvm::isa<NameUsage>(this);
}

bool Expr::isArithAssignable() {
  if (auto *N = llvm::dyn_cast<NameUsage>(this)) {
	if (auto *D = llvm::dyn_cast<VarDecl>(N->getDecl())) {
	  return D->getTypePtr()->isIntType() || D->getTypePtr()->isFloatType();
	} else if (auto *VD = llvm::dyn_cast<ParamDecl>(N->getDecl())) {
	  return VD->getTypePtr()->isIntType() || VD->getTypePtr()->isFloatType();
	}
	return false;
  }
  return false;
}

bool Expr::isCompatibleWith(Expr *RHS) {
  if (getType()->isIntType() || getType()->isIntLiteral()) {
	return RHS->getType()->isIntType() || RHS->getType()->isIntLiteral();
  }
  if (getType()->isFloatType() || getType()->isFloatLiteral()) {
	return RHS->getType()->isFloatType() || RHS->getType()->isFloatLiteral();
  }
  return false;
}
bool Expr::isIncrementable() {
  return llvm::isa<NameUsage>(this) && resultType->isIntType();
}
bool Expr::isComplementable() {
  return resultType->isIntType() || resultType->isFloatType();
}
