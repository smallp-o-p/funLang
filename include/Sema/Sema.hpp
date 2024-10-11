#pragma once
#include "AST/Decl.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"
#include "llvm/ADT/StringMap.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/SMLoc.h>
#include <memory>
#include <utility>
import Lex;
import Basic.Diag;

namespace funLang {

class CallArgList;

class Scope {
  friend class SemaAnalyzer;
  enum ScopeKind {
    FunctionScope,
    LoopScope,
    BlockScope,
    StructScope,
    GlobalScope,
  };

  std::unique_ptr<Scope> Parent;
  DeclContext *ScopeContext;
  llvm::DenseMap<IDTableEntry *, Decl *> TheLookupTable;
  ScopeKind Kind;

public:
  Scope();
  explicit Scope(std::unique_ptr<Scope> Parent, ScopeKind K);
  Scope(std::unique_ptr<Scope> Parent, ScopeKind K, DeclContext *Ctx)
      : Parent(std::move(Parent)), Kind(K), ScopeContext(Ctx) {}
  bool insertInContext(std::unique_ptr<Decl> Dec);
  std::unique_ptr<Scope> moveParentScope();
  Scope *getParent();
  DeclContext *getContext();
  Decl *lookup(IDTableEntry *Name);

  bool isFunctionScope() { return Kind == FunctionScope; }
  bool isStructScope() { return Kind == StructScope; }
  bool isLoopScope() { return Kind == LoopScope; }
  bool isBlockScope() { return Kind == BlockScope; }

  Scope *getClosestLoopScope() {
    Scope *S = this;
    while (!S->isLoopScope()) {
      S = S->getParent();
      if (!S) {
        return nullptr;
      }
    }
    return S;
  }

  bool insertInLexicalScope(IDTableEntry *ID, Decl *Dec) {
    return TheLookupTable.insert({ID, Dec}).second;
  }
  bool isGlobalScope();
};

class SemaAnalyzer {
  std::shared_ptr<DiagEngine> Diags;
  std::unique_ptr<Scope> DeclScope;
  Type *CurrentFunctionReturnType{};
  llvm::SmallVector<BuiltInType *> BuiltinTypes;
  llvm::DenseMap<IDTableEntry *, Type *> Types;
  llvm::FoldingSet<PointerType *> PointerTypes;

  void init();

protected:
  Decl *lookup(IDTableEntry *var);
  Type *getType(IDTableEntry *Ty);
  Decl *lookupOne(IDTableEntry *Var);
  BuiltInType *getBool() { return BuiltinTypes[Basic::Data::bool_]; };
  BuiltInType *getVoid() { return BuiltinTypes[Basic::Data::void_]; }

  Type *getResultTyForBinOp(Expr *LHS, Expr *RHS);

  bool insert(std::unique_ptr<Decl> ToInsert);// inserts into lexical scope AND DeclContext !!
  Type *getBuiltInType(BuiltInType::BTKind Ty);

public:
  explicit SemaAnalyzer(std::shared_ptr<DiagEngine> diag);

  void enterScope(Scope::ScopeKind K = Scope::ScopeKind::FunctionScope);
  void exitScope();
  std::unique_ptr<FunctionDecl> enterFunctionScope(std::unique_ptr<TypeUse> FunctionReturnType,
                                                   u_ptr<llvm::SmallVector<std::unique_ptr<ParamDecl>>> Params,
                                                   Token &FunctionName,
                                                   llvm::SMLoc RParenLoc);

  void enterLoopScope();
  void exitFunctionScope();

  std::unique_ptr<DeclStmt>
  actOnVarDeclStmt(std::unique_ptr<VarDecl> NameDecl,
                   std::unique_ptr<Expr> ExprInput, llvm::SMLoc RightLoc);

  std::unique_ptr<RecordDecl> enterStructScope(Token &StructDetails);
  std::unique_ptr<Expr> actOnNameUsage(Token &Identifier);
  void
  actOnFunctionDecl(FunctionDecl *Function, std::unique_ptr<CompoundStmt> CompoundToAttach);
  std::unique_ptr<Expr> actOnFunctionCall(Token &ID,
                                          llvm::SMLoc RParenLoc,
                                          u_ptr<llvm::SmallVector<u_ptr<Expr>>> PassedArgs);
  std::unique_ptr<ReturnStmt> actOnReturnStmt(llvm::SMLoc ReturnLoc,
                                              llvm::SMLoc SemicolonLoc,
                                              std::unique_ptr<Expr> ReturnExpr);
  std::unique_ptr<Expr> actOnUnaryOp(Basic::Op::Unary Op,
                                     std::unique_ptr<Expr> ExprInput,
                                     llvm::SMLoc OpLoc);
  std::unique_ptr<Expr> actOnDereference(std::unique_ptr<Expr> ExprInput, size_t DerefCount);
  std::unique_ptr<Expr> actOnBinaryOp(std::unique_ptr<Expr> LHS,
                                      Basic::Op::Binary Op,
                                      std::unique_ptr<Expr> RHS);
  std::unique_ptr<IntegerLiteral> actOnIntegerLiteral(Token &Literal);
  std::unique_ptr<FloatingLiteral> actOnFloatingLiteral(Token &Literal);
  std::unique_ptr<BooleanLiteral> actOnBooleanLiteral(Token &literal);
  std::unique_ptr<StrLiteral> actOnStrLiteral(Token &Literal);
  std::unique_ptr<TypeUse> actOnTypeUse(Token &TypeName, size_t IndirectionCount);
  std::unique_ptr<Stmt> actOnBreakStmt(Token &BreakLoc);
  std::unique_ptr<Stmt> actOnNextStmt(Token &NextLoc);

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
  std::unique_ptr<Expr> actOnMemberExpr(std::unique_ptr<Expr> Accessed, std::unique_ptr<Expr> Accessor);
  bool actOnTopLevelDecl(std::unique_ptr<Decl> TopLDecl);
  std::unique_ptr<ifStmt> actOnIfStmt(llvm::SMLoc IfLoc,
                                      llvm::SMLoc EndOfExprLoc,
                                      std::unique_ptr<Expr> IfCondition,
                                      std::unique_ptr<CompoundStmt> FirstBlock,
                                      std::unique_ptr<elifStmt> ElifChain,
                                      std::unique_ptr<CompoundStmt> ElseBlock);
  std::unique_ptr<VarDecl> actOnNameDecl(std::unique_ptr<TypeUse> Type,
                                         Token &Name);
  void actOnStructMemberDecl(std::unique_ptr<Decl> Var);

  void actOnParamDecl(llvm::SmallVector<std::unique_ptr<ParamDecl>> &CurrentParamList,
                      std::unique_ptr<ParamDecl> Param,
                      llvm::DenseMap<IDTableEntry *, ParamDecl *> &CheckAgainst);
};
}// namespace funLang
