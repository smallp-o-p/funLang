//
// Created by will on 12/17/24.
//
module;

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/SMLoc.h"
#include <memory>
#include <utility>
export module Sema;
export import :Scope;
import Basic;
import Diag;
import AST;
import Lex;

namespace funLang {
using namespace Basic;
export class SemaAnalyzer {
  DiagEngine &Diags;
  u_ptr<Scope> DeclScope{};
  llvm::SmallVector<BuiltInType *> BuiltinTypes{};
  llvm::DenseMap<IDTableEntry *, Type *> Types{};
  llvm::FoldingSet<PointerType *> PointerTypes{};
  void init();

protected:
  explicit SemaAnalyzer(DiagEngine &D) : Diags(D) { init(); }
  static Type *checkBinOperands(Expr &LHS, Expr &RHS);
  static Type *checkTyOperands(Type *LHS, Type *RHS);
  static bool TypesAreCompatible(Type *LHS, Type *RHS);
  Decl *lookup(const IDTableEntry *VariableName) const {
    auto CurrentScopeLookup = DeclScope.get();
    auto Found = CurrentScopeLookup->lookup(VariableName);
    while (!CurrentScopeLookup->lookup(VariableName)) {
      CurrentScopeLookup = CurrentScopeLookup->getParent();
      Found = CurrentScopeLookup->lookup(VariableName);
    }
    return Found;
  }
  Type *getType(IDTableEntry *Ty);
  Decl *lookupOne(IDTableEntry *Var);
  BuiltInType *getBool() { return BuiltinTypes[Data::bool_]; };
  BuiltInType *getVoid() { return BuiltinTypes[Data::void_]; }
  BuiltInType *getBuiltinType(const BuiltInType::BTKind T) {
    return BuiltinTypes[T];
  }
  Type *getResultTyForBinOp(Expr *LHS, Expr *RHS);
  Type *getClosestFunctionReturnType() {
    return DeclScope->getFunctionReturnType();
  }
  bool
  insert(u_ptr<Decl> ToInsert);// inserts into lexical scope AND DeclContext !!

  void handleSemaError();

public:
  void initCompilation(DeclContext *GlobalScope) {
    DeclScope = std::make_unique<Scope>(GlobalScope);
  }
  explicit SemaAnalyzer(std::shared_ptr<DiagEngine> diag);
  void pushScope(Scope::ScopeKind K) {
    DeclScope = std::make_unique<Scope>(std::move(DeclScope), K);
  }
  void popScope() {
    assert(!DeclScope->isGlobalScope() && "Attempted to exit global scope!\n");
    DeclScope = DeclScope->moveParentScope();
  }
  u_ptr<FunctionDecl>
  enterFunctionScope(u_ptr<TypeUse> FunctionReturnType,
                     u_ptr<llvm::SmallVector<u_ptr<ParamDecl>>> Params,
                     Token &FunctionName, llvm::SMLoc RParenLoc);

  void enterLoopScope();
  void exitFunctionScope();

  u_ptr<DeclStmt> actOnVarDeclStmt(u_ptr<VarDecl> NameDecl,
                                   u_ptr<Expr> ExprInput, llvm::SMLoc RightLoc);

  u_ptr<RecordDecl> enterStructScope(Token &StructDetails);
  u_ptr<Expr> actOnNameUsage(Token &Identifier);
  void actOnFunctionDecl(FunctionDecl *Function,
                         u_ptr<CompoundStmt> CompoundToAttach);
  u_ptr<Expr> actOnFunctionCall(const Token &ID, llvm::SMLoc RParenLoc,
                                llvm::SmallVector<u_ptr<Expr>> PassedArgs);
  u_ptr<Stmt> actOnReturnStmt(llvm::SMLoc ReturnLoc, llvm::SMLoc SemicolonLoc,
                              u_ptr<Expr> ReturnExpr);
  u_ptr<Expr> actOnUnaryOp(Op::Unary Op, u_ptr<Expr> ExprInput,
                           llvm::SMLoc OpLoc);
  u_ptr<Expr> actOnDereference(u_ptr<Expr> ExprInput, size_t DerefCount);
  u_ptr<Expr> actOnBinaryOp(u_ptr<Expr> LHS, Op::Binary Op, u_ptr<Expr> RHS);
  u_ptr<IntegerLiteral> actOnIntegerLiteral(Token &Literal);
  u_ptr<FloatingLiteral> actOnFloatingLiteral(Token &Literal);
  u_ptr<BooleanLiteral> actOnBooleanLiteral(Token &literal);
  u_ptr<StrLiteral> actOnStrLiteral(Token &Literal);
  u_ptr<TypeUse> actOnTypeUse(const Token &TypeName);
  u_ptr<Stmt> actOnBreakStmt(Token &BreakLoc);
  u_ptr<Stmt> actOnNextStmt(Token &NextLoc);

  u_ptr<forStmt> actOnForStmt(u_ptr<Stmt> Init, u_ptr<Expr> Cond,
                              u_ptr<Expr> Inc, u_ptr<CompoundStmt> Body,
                              llvm::SMLoc Left, llvm::SMLoc Right);
  u_ptr<whileStmt> actOnWhileStmt(u_ptr<Expr> Condition,
                                  u_ptr<CompoundStmt> Compound,
                                  llvm::SMLoc Left, llvm::SMLoc Right) const;

  u_ptr<loopStmt> actOnLoopStmt(u_ptr<CompoundStmt> Compound,
                                llvm::SMLoc LoopLoc);

  u_ptr<Expr> actOnIndexOperation(u_ptr<Expr> Accessed, u_ptr<Expr> AccessExpr,
                                  llvm::SMLoc Left, llvm::SMLoc Right);
  u_ptr<Expr> actOnMemberExpr(u_ptr<Expr> Accessed, u_ptr<Expr> Accessor);
  bool actOnTopLevelDecl(u_ptr<Decl> TopLDecl);
  u_ptr<ifStmt> actOnIfStmt(llvm::SMLoc IfLoc, llvm::SMLoc EndOfExprLoc,
                            u_ptr<Expr> IfCondition,
                            u_ptr<CompoundStmt> FirstBlock,
                            u_ptr<elifStmt> ElifChain,
                            u_ptr<CompoundStmt> ElseBlock);
  u_ptr<VarDecl> actOnNameDecl(u_ptr<TypeUse> Type, Token &Name);
  void actOnStructMemberDecl(u_ptr<Decl> Var);

  void
  actOnParamDecl(llvm::SmallVector<u_ptr<ParamDecl>> &CurrentParamList,
                 u_ptr<ParamDecl> Param,
                 llvm::DenseMap<IDTableEntry *, ParamDecl *> &CheckAgainst);
};
}// namespace funLang
