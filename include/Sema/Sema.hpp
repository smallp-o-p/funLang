#pragma once
#include "AST/AST.hpp"
#include "Basic/Basic.hpp"
#include "Lex/Lex.hpp"
#include "llvm/ADT/StringMap.h"
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

public:
  explicit SemaAnalyzer(std::shared_ptr<DiagEngine> diag)
      : currentScope(std::make_shared<Scope>()), diags(std::move(diag)),
        baseTypeTable(std::make_unique<Scope>()) {
    init();
  }

  void enterScope();

  std::unique_ptr<VarDeclStmt>
  actOnVarDeclStmt(std::unique_ptr<TypeUse> Type, Token IDTok,
                   std::unique_ptr<Expr> ExprInput);
  void enterFunction(std::unique_ptr<TypeUse> retType, ArgsList &args);
  std::unique_ptr<TypeUse> exitFunction();
  void exitScope();
  void actOnVarDeclStmt(VarDeclStmt &declStmt);

  std::unique_ptr<NameUsage> actOnNameUsage(Token &Identifier);
  bool actOnFnDecl(FunctionNode &Fn);
  std::unique_ptr<ReturnStmt> actOnReturnStmt(llvm::SMLoc ReturnLoc,
                                              std::unique_ptr<Expr> ReturnExpr);
  std::unique_ptr<UnaryOp> actOnUnaryOp(Basic::Op::Unary Op,
                                        std::unique_ptr<Expr> ExprInput);
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
};
} // namespace funLang
