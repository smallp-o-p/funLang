#pragma once
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
import Basic;
import Basic.IdentifierTable;
import Basic.Result;

namespace funLang {
  class Stmt;
  class CompoundStmt;
  class forStmt;
  class whileStmt;
  class loopStmt;
  class DeclStmt;
  class BreakStmt;
  class NextStmt;
  class Expr;
  class BinaryOp;
  class UnaryOp;
  class NameUsage;
  class IntegerLiteral;
  class BooleanLiteral;
  class FloatingLiteral;
  class FunctionCall;
  class MatchExpr;
  class MatchArm;
  class ErrorStmt;

  class Decl;
  class VarDecl;
  class Type;

  class Stmt {
    friend class Expr;

  public:
    enum StmtKind {
      SK_VARDECL,
      SK_RETURN,
      SK_COMPOUND,
      SK_IF,
      SK_ELIF,
      SK_FOR,
      SK_WHILE,
      SK_LOOP,
      SK_BREAK,
      SK_NEXT,
      SK_ERROR,
      SK_EXPR,
      SK_UNARYEXPR,
      SK_BINARYEXPR,
      SK_FNCALLEXPR,
      SK_INTEXPR,
      SK_FLOATEXPR,
      SK_BOOLEXPR,
      SK_STRINGEXPR,
      SK_MEMBEREXPR,
      SK_INDEXEXPR,
      SK_NAMEEXPR,
      SK_MATCHEXPR,
      SK_ERROREXPR
    };

  protected:
    StmtKind kind;
    llvm::SMLoc StartLoc;
    llvm::SMLoc EndLoc;

  public:
    explicit Stmt(StmtKind k) : kind(k) {}
    Stmt(StmtKind k, llvm::SMLoc loc);
    Stmt(StmtKind K, llvm::SMLoc Left, llvm::SMLoc Right)
        : kind(K), StartLoc(Left), EndLoc(Right) {}
    StmtKind getKind() const { return kind; }

    llvm::SMLoc getStartLoc() { return StartLoc; }
    llvm::SMLoc getEndLoc() { return EndLoc; }
  };

  class ErrorStmt : public Stmt {
  private:
    u_ptr<Stmt> BadStmt;

  public:
    explicit ErrorStmt(u_ptr<Stmt> Bad)
        : BadStmt(std::move(Bad)), Stmt(SK_ERROR, Bad->getStartLoc(), Bad->getEndLoc()) {}
  };

  class CompoundStmt : public Stmt {
  private:
    llvm::SmallVector<u_ptr<Stmt>> Statements;

  public:
    explicit CompoundStmt(llvm::SmallVector<std::unique_ptr<Stmt>> Simples)
        : Statements(std::move(Simples)), Stmt(SK_COMPOUND) {}

    static bool classof(const Stmt *S) { return S->getKind() == SK_COMPOUND; }
    llvm::SmallVector<std::unique_ptr<Stmt>> &getStatements() { return Statements; }
  };

  class elifStmt : public Stmt {
  private:
    std::unique_ptr<Expr> cond;
    std::unique_ptr<CompoundStmt> Block;
    std::unique_ptr<elifStmt> NextElif;

  public:
    elifStmt(llvm::SMLoc Loc, std::unique_ptr<Expr> cond,
             std::unique_ptr<CompoundStmt> block, std::unique_ptr<elifStmt> elif);
    void setNextInChain(std::unique_ptr<elifStmt> Elif) {
      NextElif = std::move(Elif);
    }
  };

  class ifStmt : public Stmt {
  private:
    std::unique_ptr<Expr> Condition;
    std::unique_ptr<CompoundStmt> Block;
    std::unique_ptr<elifStmt> elif;
    std::unique_ptr<CompoundStmt> ElseBlock;

  public:
    ifStmt(llvm::SMLoc IfLoc,
           std::unique_ptr<Expr> cond,
           llvm::SMLoc EndOfCondExpr,
           std::unique_ptr<CompoundStmt> block1,
           std::unique_ptr<elifStmt> elif,
           std::unique_ptr<CompoundStmt> block2);
  };

  class forStmt : public Stmt {
  private:
    std::unique_ptr<Stmt> Init;
    std::unique_ptr<Expr> Cond;
    std::unique_ptr<Expr> Inc;
    std::unique_ptr<CompoundStmt> Compound;

  public:
    forStmt(llvm::SMLoc EndLoc,
            llvm::SMLoc loc,
            std::unique_ptr<Stmt> Init,
            std::unique_ptr<Expr> Cond,
            std::unique_ptr<Expr> Inc,
            std::unique_ptr<CompoundStmt> Compound);

    static bool classof(const Stmt *S) { return S->getKind() == SK_FOR; }
  };

  class whileStmt : public Stmt {
  private:
    std::unique_ptr<Expr> condition;
    std::unique_ptr<CompoundStmt> compound;

  public:
    whileStmt(std::unique_ptr<Expr> condition,
              std::unique_ptr<CompoundStmt> compound, llvm::SMLoc whileLoc,
              llvm::SMLoc endLoc);

    static bool classof(const Stmt *S) { return S->getKind() == SK_WHILE; }
  };
  class loopStmt : public Stmt {
  private:
    std::unique_ptr<CompoundStmt> compound;

  public:
    loopStmt(std::unique_ptr<CompoundStmt> compound, llvm::SMLoc loc)
        : compound(std::move(compound)), Stmt(SK_LOOP, loc) {}
  };

  class DeclStmt : public Stmt {
  private:
    VarDecl *NamedDecl;
    std::unique_ptr<Expr> Init;

  public:
    DeclStmt(llvm::SMLoc NameStart,
             llvm::SMLoc SemiLoc,
             VarDecl *NamedDecl,
             std::unique_ptr<Expr> Expression);

    llvm::StringRef getName();
    VarDecl *getDecl() { return NamedDecl; }
    Expr *getInit() { return Init.get(); };
    static bool classof(const Stmt *S) {
      return S->getKind() == StmtKind::SK_VARDECL;
    }
  };

  class ReturnStmt : public Stmt {
  private:
    std::unique_ptr<Expr> ReturnExpr;

  public:
    explicit ReturnStmt(std::unique_ptr<Expr> exprNode);

    Expr *getExprInput() { return ReturnExpr.get(); }
    static bool classof(Stmt *S) { return S->getKind() == SK_RETURN; }
  };

  class BreakStmt : public Stmt {
  public:
    BreakStmt(llvm::SMLoc BreakLoc, llvm::SMLoc EndOfBreakLoc) : Stmt(SK_BREAK, BreakLoc, EndOfBreakLoc) {}
  };

  class NextStmt : public Stmt {
  public:
    NextStmt(llvm::SMLoc BreakLoc, llvm::SMLoc EndOfBreakLoc) : Stmt(SK_NEXT, BreakLoc, EndOfBreakLoc) {}
  };

  class Expr : public Stmt {
  protected:
    Type *resultType;// nullptr as a poison value to indicate a bad type to suppress error messages

  public:
    ~Expr() = default;
    explicit Expr(StmtKind Kind, llvm::SMLoc StartLoc, llvm::SMLoc EndLoc)
        : Stmt(Kind, StartLoc, EndLoc), resultType(nullptr) {};

    Expr(StmtKind Kind, Type *resultType, llvm::SMLoc StartLoc, llvm::SMLoc EndLoc)
        : Stmt(Kind, StartLoc, EndLoc), resultType(resultType) {}

    void setType(Type *ToSet) { resultType = ToSet; };
    Type *getType() { return resultType; }
    bool isAssignable();
    bool isArithAssignable();
    bool isComplementable();
    bool isIncrementable();

    bool isCompatibleWith(Expr *RHS);
    static bool classof(const Stmt *S) { return S->getKind() >= SK_EXPR && S->getKind() <= SK_ERROREXPR; }
  };

  class ErrorExpr : public Expr {
  protected:
    std::unique_ptr<Expr> Expression;

  public:
    explicit ErrorExpr(std::unique_ptr<Expr> ExprInput)
        : Expression(std::move(ExprInput)), Expr(SK_ERROREXPR, ExprInput->getStartLoc(), ExprInput->getEndLoc()) {}
    ErrorExpr() : Expression(nullptr), Expr(SK_ERROREXPR, llvm::SMLoc(), llvm::SMLoc()) {}
    static bool classof(const Expr *E) { return E->getKind() == SK_ERROREXPR; }
  };

  class NameUsage : public Expr {
  protected:
    IDTableEntry *TableEntry;
    Decl *UsedName;

  public:
    NameUsage(IDTableEntry *Entry,
              Decl *UsedName,
              llvm::SMLoc SLoc,
              llvm::SMLoc ELoc,
              Type *Type)
        : TableEntry(Entry), UsedName(UsedName), Expr(SK_NAMEEXPR, Type, SLoc, ELoc) {}

    llvm::StringRef getUsedName() { return TableEntry->first(); };
    IDTableEntry *getEntry() { return TableEntry; }

    Decl *getDecl() { return UsedName; }

    static bool classof(const Expr *E) { return E->getKind() == SK_NAMEEXPR; }
  };

  class IntegerLiteral : public Expr {
  private:
    llvm::APInt Value;

  public:
    explicit IntegerLiteral(llvm::APInt value, llvm::SMLoc SLoc, llvm::SMLoc ELoc)
        : Value(std::move(value)), Expr(SK_INTEXPR, SLoc, ELoc) {}
    static bool classof(const Expr *E) { return E->getKind() == SK_INTEXPR; }
  };

  class FloatingLiteral : public Expr {
  private:
    llvm::APFloat Value;

  public:
    explicit FloatingLiteral(llvm::APFloat Val, llvm::SMLoc SLoc, llvm::SMLoc RLoc)
        : Value(std::move(Val)), Expr(SK_FLOATEXPR, SLoc, RLoc) {}

    static bool classof(const Expr *E) { return E->getKind() == SK_FLOATEXPR; }
  };

  class BooleanLiteral : public Expr {
  private:
    bool TrueOrFalse;

  public:
    explicit BooleanLiteral(bool Value, llvm::SMLoc SLoc, llvm::SMLoc RLoc)
        : Expr(SK_BOOLEXPR, SLoc, RLoc), TrueOrFalse(Value) {}

    static bool classof(Expr *E) { return E->getKind() == SK_BOOLEXPR; }
    bool getValue() { return TrueOrFalse; }
  };

  class StrLiteral : public Expr {
  private:
    llvm::StringRef Str;
    uint32_t Len;

  public:
    explicit StrLiteral(uint32_t len, llvm::StringRef str,
                        llvm::SMLoc LeftQuoteLoc, llvm::SMLoc RightQuoteLoc, Type *StringType)
        : Str(str), Len(len), Expr(SK_STRINGEXPR, StringType, LeftQuoteLoc, RightQuoteLoc) {}

    static bool classof(Expr *E) { return E->getKind() == SK_STRINGEXPR; }
  };

  class MemberAccess : public Expr {
  private:
    Decl *MemberDecl;
    std::unique_ptr<Expr> Accessed;

  public:
    MemberAccess(std::unique_ptr<Expr> Accessed, Decl *MemberDecl, Type *ResultTy, llvm::SMLoc SLoc, llvm::SMLoc ELoc)
        : Accessed(std::move(Accessed)), MemberDecl(MemberDecl), Expr(SK_MEMBEREXPR, ResultTy, SLoc, ELoc) {}
    static bool classof(const Expr *E) { return E->getKind() == SK_MEMBEREXPR; }
  };

  class ArrayIndex : public Expr {
  private:
    std::unique_ptr<Expr> Accessed;
    std::unique_ptr<Expr> AccessExpr;

  public:
    ArrayIndex(std::unique_ptr<Expr> Accessed,
               std::unique_ptr<Expr> AccessExpr,
               Type *ResultTy,
               llvm::SMLoc LSquare,
               llvm::SMLoc RSquare)
        : Accessed(std::move(Accessed)), AccessExpr(std::move(AccessExpr)),
          Expr(SK_INDEXEXPR, ResultTy, LSquare, RSquare) {}

    static bool classof(const Expr *E) { return E->getKind() == SK_INDEXEXPR; }
  };

  class FunctionCall : public Expr {
  private:
    IDTableEntry *TableEntry;
    u_ptr<llvm::SmallVector<u_ptr<Expr>>> PassedParameters;

  public:
    FunctionCall(IDTableEntry *name, u_ptr<llvm::SmallVector<u_ptr<Expr>>> arguments,
                 llvm::SMLoc NameLoc, llvm::SMLoc RParen, Type *Type = nullptr)
        : TableEntry(name), PassedParameters(std::move(arguments)),
          Expr(SK_FNCALLEXPR, Type, NameLoc, RParen) {}

    llvm::StringRef getName() const { return TableEntry->first(); }
    auto getPassedArgs() const { return PassedParameters.get(); }
    static bool classof(const Expr *E) { return E->getKind() == SK_FNCALLEXPR; }
  };

  class UnaryOp : public Expr {
  private:
    Basic::Op::Unary Operator;
    std::unique_ptr<Expr> Input;

  public:
    UnaryOp(std::unique_ptr<Expr> Input, Basic::Op::Unary OpCode,
            Type *ResultType, llvm::SMLoc OpLoc, llvm::SMLoc RLoc)
        : Input(std::move(Input)), Operator(OpCode),
          Expr(SK_UNARYEXPR, ResultType, OpLoc, RLoc) {}
    Basic::Op::Unary getOp() { return Operator; }
    Expr &getExprInput() { return *Input; }
    static bool classof(Expr *E) { return E->getKind() == SK_UNARYEXPR; }
  };

  class BinaryOp : public Expr {
  private:
    Basic::Op::Binary Operator;
    std::unique_ptr<Expr> LHS;
    std::unique_ptr<Expr> RHS;

  public:
    BinaryOp(std::unique_ptr<Expr> left, std::unique_ptr<Expr> right,
             Basic::Op::Binary opcode, Type *Result)
        : LHS(std::move(left)), RHS(std::move(right)), Operator(opcode),
          Expr(SK_BINARYEXPR, Result, left->getStartLoc(), right->getEndLoc()) {}
    Expr &getLhs() { return *LHS; }
    Expr &getRhs() { return *RHS; }
    Basic::Op::Binary getOp() { return Operator; }
    Type *getLHSType() { return LHS->getType(); }
    Type *getRHSType() { return RHS->getType(); }
    static bool classof(Expr *E) { return E->getKind() == SK_BINARYEXPR; }
  };

  class MatchArm {
    std::unique_ptr<Expr> Val;

  public:
    MatchArm(std::unique_ptr<Expr> Val) : Val(std::move(Val)) {}
  };

  class MatchExpr : public Expr {
    llvm::SmallVector<std::unique_ptr<MatchArm>> Arms;
    std::unique_ptr<Expr> MatchingOn;

  public:
    MatchExpr(llvm::SmallVector<std::unique_ptr<MatchArm>> Arms,
              std::unique_ptr<Expr> MatchingOn,
              llvm::SMLoc MatchLoc,
              llvm::SMLoc RBraceLoc) : Expr(SK_MATCHEXPR, MatchLoc, RBraceLoc), Arms(std::move(Arms)),
                                       MatchingOn(std::move(MatchingOn)) {}
  };

}// namespace funLang
