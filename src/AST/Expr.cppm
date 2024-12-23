//
// Created by will on 10/27/24.
//
module;
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Support/SMLoc.h"
export module AST:Expr;
import Basic;
import :Stmt;
import :Type;

namespace funLang {
  class Decl;

  export
  {
    class Expr : public Stmt {
    protected:
      Type *resultType{}; // nullptr as a poison value to indicate a bad type to suppress error messages
    public:
      ~Expr() = default;
      explicit Expr(const StmtKind Kind, const llvm::SMLoc StartLoc, const llvm::SMLoc EndLoc)
        : Stmt(Kind, StartLoc, EndLoc) {
      }

      Expr(const StmtKind Kind, Type *resultType, const llvm::SMLoc StartLoc, const llvm::SMLoc EndLoc)
        : Stmt(Kind, StartLoc, EndLoc), resultType(resultType) {
      }

      void setType(Type *ToSet) { resultType = ToSet; }
      [[nodiscard]] Type *getType() const { return resultType; }
      bool isAssignable();
      bool isArithAssignable();
      bool isComplementable();
      bool isIncrementable();
      bool isCompatibleWith(Expr *RHS);
      static bool classof(const Stmt *S) { return S->getKind() >= SK_EXPR && S->getKind() <= SK_ERROREXPR; }
    };

    class ErrorExpr : public Expr {
    protected:
      u_ptr<Expr> Expression{};

    public:
      explicit ErrorExpr(u_ptr<Expr> ExprInput)
        : Expr(SK_ERROREXPR, ExprInput->getStartLoc(), ExprInput->getEndLoc()), Expression(std::move(ExprInput)) {
      }
      ErrorExpr() : Expr(SK_ERROREXPR, llvm::SMLoc(), llvm::SMLoc()), Expression(nullptr) {
      }
      static bool classof(const Expr *E) { return E->getKind() == SK_ERROREXPR; }
    };

    class NameUsage : public Expr {
    protected:
      IDTableEntry *TableEntry{};
      Decl *UsedName{};

    public:
      NameUsage(IDTableEntry *Entry,
                Decl *UsedName,
                const llvm::SMLoc SLoc,
                const llvm::SMLoc ELoc,
                Type *Type)
        : Expr(SK_NAMEEXPR, Type, SLoc, ELoc), TableEntry(Entry), UsedName(UsedName) {
      }

      [[nodiscard]] llvm::StringRef getUsedName() const { return TableEntry->first(); };
      [[nodiscard]] IDTableEntry *getEntry() const { return TableEntry; }
      [[nodiscard]] Decl *getDecl() const { return UsedName; }

      static bool classof(const Expr *E) { return E->getKind() == SK_NAMEEXPR; }
    };

    class IntegerLiteral : public Expr {
      llvm::APInt Value{};

    public:
      explicit IntegerLiteral(llvm::APInt value, llvm::SMLoc SLoc, llvm::SMLoc ELoc)
        : Expr(SK_INTEXPR, SLoc, ELoc), Value(std::move(value)) {
      }
      static bool classof(const Expr *E) { return E->getKind() == SK_INTEXPR; }
    };

    class FloatingLiteral : public Expr {
      llvm::APFloat Value;

    public:
      explicit FloatingLiteral(llvm::APFloat Val, llvm::SMLoc SLoc, llvm::SMLoc RLoc)
        : Expr(SK_FLOATEXPR, SLoc, RLoc), Value(std::move(Val)) {
      }

      static bool classof(const Expr *E) { return E->getKind() == SK_FLOATEXPR; }
    };

    class BooleanLiteral : public Expr {
      bool TrueOrFalse;

    public:
      explicit BooleanLiteral(bool Value, llvm::SMLoc SLoc, llvm::SMLoc RLoc)
        : Expr(SK_BOOLEXPR, SLoc, RLoc), TrueOrFalse(Value) {
      }

      static bool classof(const Expr *E) { return E->getKind() == SK_BOOLEXPR; }
      [[nodiscard]] bool getValue() const { return TrueOrFalse; }
    };

    class StrLiteral : public Expr {
      llvm::StringRef Str;
      uint32_t Len;

    public:
      explicit StrLiteral(uint32_t len,
                          llvm::StringRef str,
                          llvm::SMLoc LeftQuoteLoc,
                          llvm::SMLoc RightQuoteLoc,
                          Type *StringType)
        : Expr(SK_STRINGEXPR, StringType, LeftQuoteLoc, RightQuoteLoc), Str(str), Len(len) {
      }

      static bool classof(const Expr *E) { return E->getKind() == SK_STRINGEXPR; }
    };

    class MemberAccess : public Expr {
      Decl *MemberDecl{};
      u_ptr<Expr> Accessed;

    public:
      MemberAccess(u_ptr<Expr> Accessed, Decl *MemberDecl, Type *ResultTy, llvm::SMLoc SLoc, llvm::SMLoc ELoc)
        : Expr(SK_MEMBEREXPR, ResultTy, SLoc, ELoc), MemberDecl(MemberDecl), Accessed(std::move(Accessed)) {
      }
      static bool classof(const Expr *E) { return E->getKind() == SK_MEMBEREXPR; }
    };

    class ArrayIndex : public Expr {
      u_ptr<Expr> Accessed;
      u_ptr<Expr> AccessExpr;

    public:
      ArrayIndex(u_ptr<Expr> Accessed,
                 u_ptr<Expr> AccessExpr,
                 Type *ResultTy,
                 llvm::SMLoc LSquare,
                 llvm::SMLoc RSquare)
        : Expr(SK_INDEXEXPR, ResultTy, LSquare, RSquare), Accessed(std::move(Accessed)),
          AccessExpr(std::move(AccessExpr)) {
      }

      static bool classof(const Expr *E) { return E->getKind() == SK_INDEXEXPR; }
    };

    class FunctionCall : public Expr {
      IDTableEntry *TableEntry{};
      u_ptr<llvm::SmallVector<u_ptr<Expr> > > PassedParameters{};

    public:
      FunctionCall(IDTableEntry *name,
                   u_ptr<llvm::SmallVector<u_ptr<Expr> > > arguments,
                   llvm::SMLoc NameLoc,
                   llvm::SMLoc RParen,
                   Type *Type = nullptr)
        : Expr(SK_FNCALLEXPR, Type, NameLoc, RParen), TableEntry(name),
          PassedParameters(std::move(arguments)) {
      }

      [[nodiscard]] llvm::StringRef getName() const { return TableEntry->first(); }
      auto getPassedArgs() const { return PassedParameters.get(); }
      static bool classof(const Expr *E) { return E->getKind() == SK_FNCALLEXPR; }
    };

    class UnaryOp : public Expr {
      Basic::Op::Unary Operator;
      u_ptr<Expr> Input;

    public:
      UnaryOp(u_ptr<Expr> Input,
              Basic::Op::Unary OpCode,
              Type *ResultType,
              llvm::SMLoc OpLoc,
              llvm::SMLoc RLoc)
        : Expr(SK_UNARYEXPR, ResultType, OpLoc, RLoc), Operator(OpCode),
          Input(std::move(Input)) {
      }
      Basic::Op::Unary getOp() const { return Operator; }
      Expr &getExprInput() const { return *Input; }
      static bool classof(const Expr *E) { return E->getKind() == SK_UNARYEXPR; }
    };

    class BinaryOp : public Expr {
      Basic::Op::Binary Operator;
      u_ptr<Expr> LHS;
      u_ptr<Expr> RHS;

    public:
      BinaryOp(u_ptr<Expr> left,
               u_ptr<Expr> right,
               Basic::Op::Binary opcode,
               Type *Result)
        : Expr(SK_BINARYEXPR, Result, left->getStartLoc(), right->getEndLoc()), Operator(opcode), LHS(std::move(left)),
          RHS(std::move(right)) {
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

    public:
      explicit MatchArm(u_ptr<Expr> Val) : Val(std::move(Val)) {
      }
    };

    class MatchExpr : public Expr {
      llvm::SmallVector<u_ptr<MatchArm> > Arms{};
      u_ptr<Expr> MatchingOn{};

    public:
      MatchExpr(llvm::SmallVector<u_ptr<MatchArm> > Arms,
                u_ptr<Expr> MatchingOn,
                const llvm::SMLoc MatchLoc,
                const llvm::SMLoc RBraceLoc) : Expr(SK_MATCHEXPR, MatchLoc, RBraceLoc), Arms(std::move(Arms)),
                                               MatchingOn(std::move(MatchingOn)) {
      }
    };
  }
} // namespace funLang
