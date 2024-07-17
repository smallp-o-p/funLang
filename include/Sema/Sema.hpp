#pragma once
#include "AST/AST.hpp"
#include "Basic/Basic.hpp"
#include "Lex/Lex.hpp"
#include "llvm/ADT/StringMap.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
namespace funLang {
class SemaAnalyzer;
class Scope;

class Scope {
  llvm::StringMap<Decl *> symTable;
  std::shared_ptr<Scope> parentScope;

public:
  Scope() : parentScope(nullptr) { symTable = llvm::StringMap<Decl *>(); }
  explicit Scope(std::shared_ptr<Scope> parent)
	  : parentScope(std::move(parent)), symTable(llvm::StringMap<Decl *>()) {}
  Scope(std::shared_ptr<Scope> parent, FunctionNode *enclosingFn)
	  : parentScope(std::move(parent)), symTable(llvm::StringMap<Decl *>()) {}
  bool insert(Decl *decl) {
	return symTable.insert({decl->getName(), decl}).second;
  }
  std::shared_ptr<Scope> &getParent() { return parentScope; }
  Decl *find(llvm::StringRef varName) {
	auto found = symTable.find(varName);
	if (found == symTable.end()) {
	  return nullptr;
	}
	return found->second;
  }
};

class SemaAnalyzer {
private:
  std::shared_ptr<Scope> currentScope;
  std::shared_ptr<DiagEngine> diags;
  std::unique_ptr<TypeUse> currentFnRetType;
  std::unique_ptr<Scope> baseTypeTable;
  void init();

protected:
  bool TypesAreEqual(TypeDecl *LHS, TypeDecl *RHS);
  bool TypesAreEqualOrCompatible(TypeDecl *LHS, TypeDecl *RHS);
  Decl *lookup(llvm::StringRef var);
  Decl *lookupCurrentScope(llvm::StringRef varName);
  TypeDecl *lookupType(llvm::StringRef Type);

public:
  explicit SemaAnalyzer(std::shared_ptr<DiagEngine> diag)
	  : currentScope(std::make_shared<Scope>()), diags(std::move(diag)),
		baseTypeTable(std::make_unique<Scope>()) {
	init();
  }

  void enterScope();
  void exitScope();

  std::unique_ptr<VarDeclStmt>
  actOnVarDeclStmt(std::unique_ptr<VarDecl> NameDecl,
				   std::unique_ptr<Expr> ExprInput, llvm::SMLoc RightLoc);
  void enterFunction(std::unique_ptr<TypeUse> retType, ArgsList &args);
  std::unique_ptr<TypeUse> exitFunction();

  std::unique_ptr<NameUsage> actOnNameUsage(Token &Identifier);

  std::unique_ptr<FunctionNode>
  actOnFnDecl(std::unique_ptr<TypeUse> Type, Token ID,
			  std::unique_ptr<ArgsList> Args,
			  std::unique_ptr<CompoundStmt> Compound);
  std::unique_ptr<Expr> actOnFnCall(Token &ID,
									std::unique_ptr<CallArgList> PassedArgs);
  std::unique_ptr<ReturnStmt> actOnReturnStmt(llvm::SMLoc ReturnLoc,
											  std::unique_ptr<Expr> ReturnExpr);
  std::unique_ptr<Expr> actOnUnaryOp(Basic::Op::Unary Op,
									 std::unique_ptr<Expr> ExprInput);
  std::unique_ptr<Expr> actOnDereference(std::unique_ptr<Expr> ExprInput, size_t DerefCount);
  std::unique_ptr<Expr> actOnBinaryOp(std::unique_ptr<Expr> LHS,
									  Basic::Op::Binary Op,
									  std::unique_ptr<Expr> RHS);
  std::unique_ptr<IntegerLiteral> actOnIntegerLiteral(Token &Literal);
  std::unique_ptr<FloatingLiteral> actOnFloatingLiteral(Token &Literal);
  std::unique_ptr<BooleanLiteral> actOnBooleanLiteral(Token &literal);
  std::unique_ptr<StringLiteral> actOnStrLiteral(Token &Literal);
  std::unique_ptr<TypeUse> actOnTypeUse(Token &TypeName, size_t IndirectionCount);
  std::unique_ptr<forStmt> actOnForStmt(std::unique_ptr<Stmt> Init,
										std::unique_ptr<Expr> Cond,
										std::unique_ptr<Expr> Inc,
										std::unique_ptr<CompoundStmt> Body,
										llvm::SMLoc Left, llvm::SMLoc Right);
  std::unique_ptr<whileStmt>
  actOnWhileStmt(std::unique_ptr<Expr> Condition,
				 std::unique_ptr<CompoundStmt> Compound, llvm::SMLoc Left,
				 llvm::SMLoc Right);

  std::unique_ptr<loopStmt> actOnLoopStmt(std::unique_ptr<CompoundStmt> Compound, llvm::SMLoc LoopLoc);

  std::unique_ptr<Expr> actOnIndexOperation(std::unique_ptr<Expr> Accessed,
											std::unique_ptr<Expr> AccessExpr,
											llvm::SMLoc Left,
											llvm::SMLoc Right);
  std::unique_ptr<Expr> actOnMemberExpr(std::unique_ptr<Expr> Accessed,
										std::unique_ptr<Expr> Accessee,
										llvm::SMLoc Left,
										llvm::SMLoc Right);
  bool actOnTopLevelDecl(Decl *TopLDecl);
  std::unique_ptr<TypeDecl>
  actOnStructDecl(Token &TypeName, std::unique_ptr<TypeProperties> Properties,
				  llvm::SMLoc RBraceLoc);
  std::unique_ptr<ifStmt> actOnIfStmt(llvm::SMLoc IfLoc, std::unique_ptr<Expr> IfCondition,
									  std::unique_ptr<CompoundStmt> FirstBlock,
									  std::unique_ptr<elifStmt> ElifChain,
									  std::unique_ptr<CompoundStmt> ElseBlock);
  std::unique_ptr<VarDecl> actOnNameDecl(std::unique_ptr<TypeUse> Type,
										 Token &Name);
};
} // namespace funLang
