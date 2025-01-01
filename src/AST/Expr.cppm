//
// Created by will on 10/27/24.
//
module;
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
#include <utility>

export module AST:Expr;
import Basic;
import :Stmt;
import :Type;

namespace funLang {
class Decl;
export {
  class ErrorExpr;
  class Expr : public Stmt {
  public:
    enum ExprKind { Location, Value };

  protected:
    Type *
        resultType{};// nullptr as a poison value to indicate a bad type to suppress error messages
    ExprKind ExpressionType;

    Expr(const StmtKind Kind, const llvm::SMLoc StartLoc,
         const llvm::SMLoc EndLoc, const ExprKind ExprK = Value)
        : Stmt(Kind, StartLoc, EndLoc), ExpressionType(ExprK) {}

    Expr(const StmtKind Kind, Type *resultType, const llvm::SMLoc StartLoc,
         const llvm::SMLoc EndLoc, const ExprKind ExprValue)
        : Stmt(Kind, StartLoc, EndLoc), resultType(resultType),
          ExpressionType(ExprValue) {}

  public:
    using ExprPtr = u_ptr<Expr>;
    ~Expr() = default;
    void setType(Type *ToSet) { resultType = ToSet; }
    [[nodiscard]] Type *getType() const { return resultType; }
    bool isValueExpr() { return ExpressionType == Value; }
    bool isLocationExpr() { return ExpressionType == Location; }
    bool isComplementable() { return resultType->isIntType(); }
    bool isIncrementable() {
      return isLocationExpr() and (resultType->isI32() or resultType->isI64());
    }
    bool isCompatibleWith(Expr *RHS);
    [[nodiscard]] bool isError() const { return llvm::isa<ErrorExpr>(this); }
    static bool classof(const Stmt *S) {
      return S->getKind() >= SK_EXPR && S->getKind() <= SK_ERROREXPR;
    }
  };

  class ErrorExpr : public Expr {
    using ErrExprPtr = std::unique_ptr<ErrorExpr>;
    u_ptr<Expr> Expression{};
    explicit ErrorExpr(u_ptr<Expr> ExprInput)
        : Expr(SK_ERROREXPR, ExprInput->getStartLoc(), ExprInput->getEndLoc()),
          Expression(std::move(ExprInput)) {}

  public:
    static ErrExprPtr Create(ExprPtr E) {
      return std::unique_ptr<ErrorExpr>(new ErrorExpr(std::move(E)));
    }
    static bool classof(const Expr *E) { return E->getKind() == SK_ERROREXPR; }
  };

  class NameUsage : public Expr {
  protected:
    IDTableEntry *TableEntry{};
    Decl *UsedName{};

  public:
    NameUsage(IDTableEntry *Entry, Decl *UsedName, const llvm::SMLoc SLoc,
              const llvm::SMLoc ELoc, Type *Type)
        : Expr(SK_NAMEEXPR, Type, SLoc, ELoc, Location), TableEntry(Entry),
          UsedName(UsedName) {}

    [[nodiscard]] llvm::StringRef getUsedName() const {
      return TableEntry->first();
    };
    [[nodiscard]] IDTableEntry *getEntry() const { return TableEntry; }
    [[nodiscard]] Decl *getDecl() const { return UsedName; }

    static bool classof(const Expr *E) { return E->getKind() == SK_NAMEEXPR; }
  };

  class IntegerLiteral : public Expr {
    llvm::APInt Value{};

  public:
    explicit IntegerLiteral(llvm::APInt value, llvm::SMLoc SLoc,
                            llvm::SMLoc ELoc)
        : Expr(SK_INTEXPR, SLoc, ELoc), Value(std::move(value)) {}
    static bool classof(const Expr *E) { return E->getKind() == SK_INTEXPR; }
  };

  class FloatingLiteral : public Expr {
    llvm::APFloat Value;

  public:
    explicit FloatingLiteral(llvm::APFloat Val, llvm::SMLoc SLoc,
                             llvm::SMLoc RLoc)
        : Expr(SK_FLOATEXPR, SLoc, RLoc), Value(std::move(Val)) {}

    static bool classof(const Expr *E) { return E->getKind() == SK_FLOATEXPR; }
  };

  class BooleanLiteral : public Expr {
    bool TrueOrFalse;

  public:
    explicit BooleanLiteral(bool Value, llvm::SMLoc SLoc, llvm::SMLoc RLoc)
        : Expr(SK_BOOLEXPR, SLoc, RLoc), TrueOrFalse(Value) {}

    static bool classof(const Expr *E) { return E->getKind() == SK_BOOLEXPR; }
    [[nodiscard]] bool getValue() const { return TrueOrFalse; }
  };

  class StrLiteral : public Expr {
    llvm::StringRef Str;
    uint32_t Len;

  public:
    explicit StrLiteral(uint32_t len, llvm::StringRef str,
                        llvm::SMLoc LeftQuoteLoc, llvm::SMLoc RightQuoteLoc,
                        Type *StringType)
        : Expr(SK_STRINGEXPR, StringType, LeftQuoteLoc, RightQuoteLoc, Value),
          Str(str), Len(len) {}

    static bool classof(const Expr *E) { return E->getKind() == SK_STRINGEXPR; }
  };

  class MemberAccess : public Expr {
    Decl *MemberDecl{};
    u_ptr<Expr> Accessed;

  public:
    MemberAccess(u_ptr<Expr> Accessed, Decl *MemberDecl, Type *ResultTy,
                 llvm::SMLoc SLoc, llvm::SMLoc ELoc)
        : Expr(SK_MEMBEREXPR, ResultTy, SLoc, ELoc, Location),
          MemberDecl(MemberDecl), Accessed(std::move(Accessed)) {}
    static bool classof(const Expr *E) { return E->getKind() == SK_MEMBEREXPR; }
  };

  class ArrayIndex : public Expr {
    u_ptr<Expr> Accessed;
    u_ptr<Expr> AccessExpr;

  public:
    ArrayIndex(u_ptr<Expr> Accessed, u_ptr<Expr> AccessExpr, Type *ResultTy,
               llvm::SMLoc LSquare, llvm::SMLoc RSquare)
        : Expr(SK_INDEXEXPR, ResultTy, LSquare, RSquare, Location),
          Accessed(std::move(Accessed)), AccessExpr(std::move(AccessExpr)) {}

    static bool classof(const Expr *E) { return E->getKind() == SK_INDEXEXPR; }
  };

  class FunctionCall : public Expr {
    IDTableEntry *TableEntry{};
    llvm::SmallVector<u_ptr<Expr>> PassedParameters{};

    FunctionCall(IDTableEntry *Name, llvm::SmallVector<u_ptr<Expr>> arguments,
                 const llvm::SMLoc NameLoc, const llvm::SMLoc RParen,
                 Type *Type = nullptr)
        : Expr(SK_FNCALLEXPR, Type, NameLoc, RParen, Value), TableEntry(Name),
          PassedParameters(std::move(arguments)) {}

    FunctionCall(IDTableEntry *Name, const llvm::SMLoc NameLoc,
                 const llvm::SMLoc RParen, Type *T = nullptr)
        : Expr(SK_FNCALLEXPR, T, NameLoc, RParen, Value) {}

  public:
    static u_ptr<FunctionCall> Create(IDTableEntry *Name,
                                      llvm::SmallVector<u_ptr<Expr>> arguments,
                                      const llvm::SMLoc NameLoc,
                                      const llvm::SMLoc RParen,
                                      Type *Type = nullptr) {
      return std::unique_ptr<FunctionCall>(
          new FunctionCall(Name, std::move(arguments), NameLoc, RParen, Type));
    }

    static u_ptr<FunctionCall> CreateNoArgs(IDTableEntry *Name,
                                            const llvm::SMLoc NameLoc,
                                            const llvm::SMLoc RParen,
                                            Type *Type = nullptr) {
      return std::unique_ptr<FunctionCall>(
          new FunctionCall(Name, NameLoc, RParen, Type));
    }

    [[nodiscard]] llvm::StringRef getName() const {
      return TableEntry->first();
    }

    [[nodiscard]] auto getPassedArgs() const
        -> const llvm::SmallVector<std::unique_ptr<Expr>> * {
      return &PassedParameters;
    }

    [[nodiscard]] auto getNumArgs() const -> size_t {
      return PassedParameters.size();
    }

    static bool classof(const Expr *E) { return E->getKind() == SK_FNCALLEXPR; }
  };

  class UnaryOp : public Expr {
    Basic::Op::Unary Operator;
    u_ptr<Expr> Input;

    UnaryOp(u_ptr<Expr> Input, Basic::Op::Unary OpCode, Type *ResultType,
            llvm::SMLoc OpLoc, llvm::SMLoc RLoc, ExprKind Kind)
        : Expr(SK_UNARYEXPR, ResultType, OpLoc, RLoc, Kind), Operator(OpCode),
          Input(std::move(Input)) {}

  public:
    static u_ptr<UnaryOp> Create(u_ptr<Expr> Input, Basic::Op::Unary OpCode,
                                 Type *ResultType, llvm::SMLoc OpLoc,
                                 llvm::SMLoc RLoc, ExprKind Kind) {
      return std::unique_ptr<UnaryOp>(
          new UnaryOp(std::move(Input), OpCode, ResultType, OpLoc, RLoc, Kind));
    }

    [[nodiscard]] Basic::Op::Unary getOp() const { return Operator; }
    [[nodiscard]] Expr &getExprInput() const { return *Input; }
    static bool classof(const Expr *E) { return E->getKind() == SK_UNARYEXPR; }
  };

  class BinaryOp : public Expr {
    using BinOpPtr = u_ptr<BinaryOp>;
    Basic::Op::Binary Operator;
    u_ptr<Expr> LHS;
    u_ptr<Expr> RHS;

    BinaryOp(u_ptr<Expr> left, u_ptr<Expr> right, Basic::Op::Binary opcode,
             Type *Result)
        : Expr(SK_BINARYEXPR, Result, left->getStartLoc(), right->getEndLoc(),
               Value),
          Operator(opcode), LHS(std::move(left)), RHS(std::move(right)) {}

  public:
    static BinOpPtr Create(ExprPtr LHS, ExprPtr RHS, Basic::Op::Binary OpCode,
                           Type *ResultTy) {
      return std::unique_ptr<BinaryOp>(
          new BinaryOp(std::move(LHS), std::move(RHS), OpCode, ResultTy));
    }
    Expr &getLhs() const { return *LHS; }
    Expr &getRhs() const { return *RHS; }
    Basic::Op::Binary getOp() const { return Operator; }
    Type *getLHSType() const { return LHS->getType(); }
    Type *getRHSType() const { return RHS->getType(); }
    static bool classof(const Expr *E) { return E->getKind() == SK_BINARYEXPR; }
  };

  class MatchArm {
    u_ptr<Expr> Val{};
    u_ptr<MatchArm> Next{};

  public:
    explicit MatchArm(u_ptr<Expr> Val, u_ptr<MatchArm> Next)
        : Val(std::move(Val)), Next(std::move(Next)) {}
  };

  class MatchExpr : public Expr {
    u_ptr<MatchArm> Arms{};
    u_ptr<Expr> MatchingOn{};

  public:
    MatchExpr(u_ptr<MatchArm> Arms, u_ptr<Expr> MatchingOn,
              const llvm::SMLoc MatchLoc, const llvm::SMLoc RBraceLoc)
        : Expr(SK_MATCHEXPR, MatchLoc, RBraceLoc, Value), Arms(std::move(Arms)),
          MatchingOn(std::move(MatchingOn)) {}
  };
}
}// namespace funLang
