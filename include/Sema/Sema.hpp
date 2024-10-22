#pragma once
#include "llvm/ADT/StringMap.h"
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/SMLoc.h>
#include "llvm/ADT/DenseMap.h"
#include <memory>
#include <utility>
#include <llvm/ADT/FoldingSet.h>
import Lex;
import Basic;
import funLangAST;

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

    u_ptr<Scope> Parent{};
    DeclContext *ScopeContext{};
    llvm::DenseMap<IDTableEntry *, Decl *> LookupTable{};
    ScopeKind Kind;

  public:
    Scope();
    explicit Scope(u_ptr<Scope> Parent, ScopeKind K);
    Scope(u_ptr<Scope> Parent, const ScopeKind K, DeclContext *Ctx)
      : Parent(std::move(Parent)), ScopeContext(Ctx), Kind(K) {
    }
    bool insertInContext(u_ptr<Decl> Dec);
    u_ptr<Scope> moveParentScope();
    Scope *getParent();
    DeclContext *getContext();
    Decl *lookup(const IDTableEntry *Name);

    [[nodiscard]] bool isFunctionScope() const { return Kind == FunctionScope; }
    [[nodiscard]] bool isStructScope() const { return Kind == StructScope; }
    [[nodiscard]] bool isLoopScope() const { return Kind == LoopScope; }
    [[nodiscard]] bool isBlockScope() const { return Kind == BlockScope; }

    Scope *getClosestLoopScope() {
      auto S = this;
      while (!S->isLoopScope()) {
        S = S->getParent();
        if (!S) {
          return nullptr;
        }
      }
      return S;
    }

    bool insertInLexicalScope(IDTableEntry *ID, Decl *Dec) {
      return LookupTable.insert({ID, Dec}).second;
    }
    bool isGlobalScope();
  };

  class SemaAnalyzer {
    std::shared_ptr<DiagEngine> Diags;
    u_ptr<Scope> DeclScope;
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
    bool insert(u_ptr<Decl> ToInsert); // inserts into lexical scope AND DeclContext !!

  public:
    explicit SemaAnalyzer(std::shared_ptr<DiagEngine> diag);

    void enterScope(Scope::ScopeKind K = Scope::ScopeKind::FunctionScope);
    void exitScope();
    u_ptr<FunctionDecl> enterFunctionScope(u_ptr<TypeUse> FunctionReturnType,
                                           u_ptr<llvm::SmallVector<u_ptr<ParamDecl> > > Params,
                                           Token &FunctionName,
                                           llvm::SMLoc RParenLoc);

    void enterLoopScope();
    void exitFunctionScope();

    u_ptr<DeclStmt>
    actOnVarDeclStmt(u_ptr<VarDecl> NameDecl,
                     u_ptr<Expr> ExprInput,
                     llvm::SMLoc RightLoc);

    u_ptr<RecordDecl> enterStructScope(Token &StructDetails);
    u_ptr<Expr> actOnNameUsage(Token &Identifier);
    void
    actOnFunctionDecl(FunctionDecl *Function, u_ptr<CompoundStmt> CompoundToAttach);
    u_ptr<Expr> actOnFunctionCall(Token &ID,
                                  llvm::SMLoc RParenLoc,
                                  u_ptr<llvm::SmallVector<u_ptr<Expr> > > PassedArgs);
    u_ptr<ReturnStmt> actOnReturnStmt(llvm::SMLoc ReturnLoc,
                                      llvm::SMLoc SemicolonLoc,
                                      u_ptr<Expr> ReturnExpr);
    u_ptr<Expr> actOnUnaryOp(Basic::Op::Unary Op,
                             u_ptr<Expr> ExprInput,
                             llvm::SMLoc OpLoc);
    u_ptr<Expr> actOnDereference(u_ptr<Expr> ExprInput, size_t DerefCount);
    u_ptr<Expr> actOnBinaryOp(u_ptr<Expr> LHS,
                              Basic::Op::Binary Op,
                              u_ptr<Expr> RHS);
    u_ptr<IntegerLiteral> actOnIntegerLiteral(Token &Literal);
    u_ptr<FloatingLiteral> actOnFloatingLiteral(Token &Literal);
    u_ptr<BooleanLiteral> actOnBooleanLiteral(Token &literal);
    u_ptr<StrLiteral> actOnStrLiteral(Token &Literal);
    u_ptr<TypeUse> actOnTypeUse(Token &TypeName, size_t IndirectionCount);
    u_ptr<Stmt> actOnBreakStmt(Token &BreakLoc);
    u_ptr<Stmt> actOnNextStmt(Token &NextLoc);

    u_ptr<forStmt> actOnForStmt(u_ptr<Stmt> Init,
                                u_ptr<Expr> Cond,
                                u_ptr<Expr> Inc,
                                u_ptr<CompoundStmt> Body,
                                llvm::SMLoc Left,
                                llvm::SMLoc Right);
    u_ptr<whileStmt>
    actOnWhileStmt(u_ptr<Expr> Condition,
                   u_ptr<CompoundStmt> Compound,
                   llvm::SMLoc Left,
                   llvm::SMLoc Right);

    u_ptr<loopStmt> actOnLoopStmt(u_ptr<CompoundStmt> Compound, llvm::SMLoc LoopLoc);

    u_ptr<Expr> actOnIndexOperation(u_ptr<Expr> Accessed,
                                    u_ptr<Expr> AccessExpr,
                                    llvm::SMLoc Left,
                                    llvm::SMLoc Right);
    u_ptr<Expr> actOnMemberExpr(u_ptr<Expr> Accessed, u_ptr<Expr> Accessor);
    bool actOnTopLevelDecl(u_ptr<Decl> TopLDecl);
    u_ptr<ifStmt> actOnIfStmt(llvm::SMLoc IfLoc,
                              llvm::SMLoc EndOfExprLoc,
                              u_ptr<Expr> IfCondition,
                              u_ptr<CompoundStmt> FirstBlock,
                              u_ptr<elifStmt> ElifChain,
                              u_ptr<CompoundStmt> ElseBlock);
    u_ptr<VarDecl> actOnNameDecl(u_ptr<TypeUse> Type,
                                 Token &Name);
    void actOnStructMemberDecl(u_ptr<Decl> Var);

    void actOnParamDecl(llvm::SmallVector<u_ptr<ParamDecl> > &CurrentParamList,
                        u_ptr<ParamDecl> Param,
                        llvm::DenseMap<IDTableEntry *, ParamDecl *> &CheckAgainst);
  };
} // namespace funLang
