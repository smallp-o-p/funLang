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
public:
  Scope() : parentScope(nullptr) {
	symTable = llvm::StringMap<Decl *>();
  }
  explicit Scope(std::shared_ptr<Scope> parent)
	  : parentScope(std::move(parent)), symTable(llvm::StringMap<Decl *>()) {}
  Scope(std::shared_ptr<Scope> parent, FunctionNode *enclosingFn)
	  : parentScope(std::move(parent)), symTable(llvm::StringMap<Decl *>()) {}
  bool insert(Decl *decl) { return symTable.insert({decl->getName(), decl}).second; }
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
public:
  explicit SemaAnalyzer(std::shared_ptr<DiagEngine> diag)
	  : currentScope(std::make_shared<Scope>()), diags(std::move(diag)), baseTypeTable(std::make_unique<Scope>()) {
	init();
  }

  void enterScope();
  void enterFunction(std::unique_ptr<TypeUse> retType, ArgsList &args);
  std::unique_ptr<TypeUse> exitFunction();
  void exitScope();
  void actOnVarDeclStmt(VarDeclStmt &declStmt);
  bool actOnNameUsage(Token &identifier);
  bool actOnFnDecl(FunctionNode &fn);
  bool actOnReturnStmt(Expr &retExpr);
  bool actOnUnaryOp(UnaryOp &unary);
  bool actOnBinaryOp(BinaryOp &bin);
  void actOnFnArgsList(ArgsList &args);
  bool actOnFnCall(FunctionCall &fnCall);
  bool actOnTopLevelDecl(Decl &TopLDecl);
  bool actOnStructVarDecl(VarDeclStmt &DeclStmt);
  bool actOnStructDecl(TypeDecl &StructDecl);

  TypeDecl *getBaseType(llvm::StringRef type) {
	Decl *found = baseTypeTable->find(type);
	assert(found && "Tried to lookup base type that does not exist");
	return llvm::dyn_cast<TypeDecl>(found);
  }

  std::vector<TypeDecl *> getManyTypes(std::initializer_list<llvm::StringRef> TypesToGet) {
	std::vector<TypeDecl *> List;
	for (llvm::StringRef TypeName : TypesToGet) {
	  auto *Found = llvm::dyn_cast<TypeDecl>(baseTypeTable->find(TypeName));
	  assert(Found && "Did not find a base type in getManyTypes()");
	  List.push_back(Found);
	}
	return List;
  }

  Decl *lookup(llvm::StringRef var);

  Decl *lookupOneScope(llvm::StringRef varName) {
	return currentScope->find(varName);
  }
};
}