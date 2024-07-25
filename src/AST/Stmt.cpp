//
// Created by will on 7/24/24.
//
#include "AST/Stmt.hpp"
#include "AST/Decl.hpp"
using namespace funLang;
llvm::StringRef funLang::DeclStmt::getName() { return NamedDecl->getName(); }

DeclStmt::DeclStmt(VarDecl *NamedDecl, std::unique_ptr<Expr> Expression, llvm::SMLoc Left, llvm::SMLoc Right)
	: NamedDecl(NamedDecl), RHS(std::move(Expression)),
	  Stmt(SK_VARDECL, Left, Right) {}

Stmt::Stmt(funLang::Stmt::StmtKind k, llvm::SMLoc loc) : kind(k) {}

elifStmt::elifStmt(llvm::SMLoc Loc,
				   std::unique_ptr<Expr> cond,
				   std::unique_ptr<CompoundStmt> block,
				   std::unique_ptr<elifStmt> elif)
	: Stmt(SK_ELIF, Loc), cond(std::move(cond)), Block(std::move(block)),
	  NextElif(std::move(elif)) {}

ifStmt::ifStmt(llvm::SMLoc IfLoc,
			   std::unique_ptr<Expr> cond,
			   std::unique_ptr<CompoundStmt> block1,
			   std::unique_ptr<elifStmt> elif,
			   std::unique_ptr<CompoundStmt> block2)
	: Condition(std::move(cond)), Block(std::move(block1)),
	  elif(std::move(elif)), ElseBlock(std::move(block2)), Stmt(SK_IF, IfLoc) {}

forStmt::forStmt(std::unique_ptr<Stmt> var,
				 std::unique_ptr<Expr> range,
				 std::unique_ptr<Expr> iter,
				 std::unique_ptr<CompoundStmt> compound,
				 llvm::SMLoc loc)
	: Stmt(SK_FOR, loc), var(std::move(var)), range(std::move(range)),
	  iterator(std::move(iter)), compound(std::move(compound)) {}

whileStmt::whileStmt(std::unique_ptr<Expr> condition,
					 std::unique_ptr<CompoundStmt> compound,
					 llvm::SMLoc whileLoc,
					 llvm::SMLoc endLoc)
	: compound(std::move(compound)), condition(std::move(condition)),
	  Stmt(SK_WHILE, whileLoc, endLoc) {}

matchArm::matchArm(std::unique_ptr<Expr> lhs, std::unique_ptr<CompoundStmt> rhs, llvm::SMLoc begin)
	: lhs(std::move(lhs)), rhs(std::move(rhs)), Stmt(SK_MATCHARM, begin) {}

ReturnStmt::ReturnStmt(std::unique_ptr<Expr> exprNode)
	: ReturnExpr(std::move(exprNode)), Stmt(SK_RETURN) {}
