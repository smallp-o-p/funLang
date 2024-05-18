#pragma once
#include "AST.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include "llvm/ADT/StringMap.h"
namespace funLang {
class SemaAnalyzer;
class Scope;

class Scope {
  llvm::StringMap<Decl *> symTable;
  std::shared_ptr<Scope> parentScope;
  FunctionNode *currentFunction;
public:
  Scope() : parentScope(nullptr), symTable(llvm::StringMap<Decl *>()), currentFunction(nullptr) {}
  explicit Scope(std::shared_ptr<Scope> parent)
	  : parentScope(std::move(parent)), symTable(llvm::StringMap<Decl *>()), currentFunction(nullptr) {}
  Scope(std::shared_ptr<Scope> parent, FunctionNode *enclosingFn)
	  : parentScope(std::move(parent)), symTable(llvm::StringMap<Decl *>()), currentFunction(enclosingFn) {}
  bool insert(Decl *decl) { return symTable.insert({decl->getName(), decl}).second; }
  std::shared_ptr<Scope> &getParent() { return parentScope; }
  Decl *find(llvm::StringRef varName) {
	auto found = symTable.find(varName);
	if (found==symTable.end()) {
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
public:
  explicit SemaAnalyzer(std::shared_ptr<DiagEngine> diag)
	  : currentScope(std::make_shared<Scope>()), diags(std::move(diag)) {}

  void init() {
	auto boolType = new TypeDecl("bool");
	auto i32Type = new TypeDecl("i32");
	auto i64Type = new TypeDecl("i64");
	auto f32Type = new TypeDecl("f32");
	auto f64Type = new TypeDecl("f64");
	auto stringType = new TypeDecl("string");
	auto voidType = new TypeDecl("void");

	currentScope->insert(boolType);
	currentScope->insert(i32Type);
	currentScope->insert(i64Type);
	currentScope->insert(f32Type);
	currentScope->insert(f64Type);
	currentScope->insert(stringType);
	currentScope->insert(voidType);
  }
  void enterScope();
  void enterFunction(std::unique_ptr<TypeUse> retType);;
  std::unique_ptr<TypeUse> exitFunction();
  void exitScope();
  void actOnVarDeclStmt(VarDeclStmt &declStmt);
  bool actOnNameUsage(Token &identifier);
  bool actOnFnDecl(FunctionNode &fn);
  bool actOnReturnStmt(Expr &retExpr);
  bool actOnUnaryOp(UnaryOp &unary);
  bool actOnBinaryOp(BinaryOp &bin);
  bool actOnFnCall(FunctionCall &fnCall);
  bool actOnTopLevelDecl(Decl &decl);
  Decl *lookup(llvm::StringRef var) {
	std::shared_ptr<Scope> s = currentScope;
	while (s) {
	  Decl *found = s->find(var);
	  if (found) {
		return found;
	  } else {
		s = s->getParent();
	  }
	}
	return nullptr;
  }
};
}