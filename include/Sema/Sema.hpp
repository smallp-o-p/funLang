#pragma once
#include "AST/AST.hpp"
#include "Basic/Basic.hpp"
#include "llvm/ADT/StringMap.h"
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

public:
  explicit SemaAnalyzer(std::shared_ptr<DiagEngine> diag)
      : currentScope(std::make_shared<Scope>()), diags(std::move(diag)),
        baseTypeTable(std::make_unique<Scope>()) {
    init();
  }

  void enterScope();
  void enterFunction(std::unique_ptr<TypeUse> retType, ArgsList &args);
  std::unique_ptr<TypeUse> exitFunction();
  void exitScope();
  bool actOnVarDeclStmt(VarDeclStmt &declStmt);
  bool actOnNameUsage(Token &identifier);
  bool actOnFnDecl(FunctionNode &Fn);
  bool actOnReturnStmt(ReturnStmt &RetExpr);
  bool actOnUnaryOp(UnaryOp &unary);
  bool actOnBinaryOp(BinaryOp &Binary);
  bool actOnAddSubOp(BinaryOp &AddOrSubtract);
  bool actOnMultDivOp(BinaryOp &MultiplyOrDivide);
  bool actOnComparisonOp(BinaryOp &CmpOp);
  void actOnFnArgsList(ArgsList &args);
  bool actOnFnCall(FunctionCall &fnCall);
  bool actOnTopLevelDecl(Decl &TopLDecl);
  bool actOnStructVarDecl(VarDeclStmt &DeclStmt);
  bool actOnStructDecl(TypeDecl &StructDecl);
  Decl *lookup(llvm::StringRef var);
  Decl *lookupOneScope(llvm::StringRef varName);
  TypeDecl *lookupType(llvm::StringRef Type);
  TypeDecl *lookupBaseType(Basic::Data::Type Type);
  TypeDecl *isEqualToBaseType(std::initializer_list<Basic::Data::Type> Types,
                              TypeDecl *Type);
};
} // namespace funLang
