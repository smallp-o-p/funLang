//
// Created by will on 10/27/24.
//
module;
#include <cassert>
#include <llvm/Support/SMLoc.h>
#include <memory>
export module AST:Stmt;
import Basic;

namespace funLang {
class VarDecl;

export {
  class elifStmt;
  class Expr;
  class Stmt {
  public:
    enum StmtKind {
      SK_EMPTY,
      SK_VARDECL,
      SK_RETURN,
      SK_COMPOUND,
      SK_IF,
      SK_ELIF,
      SK_LOOP,
      SK_FOR,
      SK_WHILE,
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
      SK_ERROREXPR,
      SK_DECL
    };

  protected:
    const StmtKind Kind;
    const llvm::SMLoc StartLoc{}, EndLoc{};

  public:
    explicit Stmt(const StmtKind K) : Kind(K) {}
    Stmt(const StmtKind K, const llvm::SMLoc Loc)
        : Kind(K), StartLoc(Loc), EndLoc(Loc) {}
    Stmt(const StmtKind K, const llvm::SMLoc Left, const llvm::SMLoc Right)
        : Kind(K), StartLoc(Left), EndLoc(Right) {}

    [[nodiscard]] StmtKind getKind() const { return Kind; }
    [[nodiscard]] llvm::SMLoc getStartLoc() const { return StartLoc; }
    [[nodiscard]] llvm::SMLoc getEndLoc() const { return EndLoc; }
  };

  class CompoundStmt : public Stmt {
    friend class StmtResult;
    llvm::SmallVector<u_ptr<Stmt>> Statements{};

  public:
    explicit CompoundStmt(llvm::SmallVector<u_ptr<Stmt>> Simples)
        : Stmt(SK_COMPOUND), Statements(std::move(Simples)) {}

    static bool classof(const Stmt *S) { return S->getKind() == SK_COMPOUND; }
    llvm::SmallVector<u_ptr<Stmt>> &getStatements() { return Statements; }
  };

  class ErrorStmt : public Stmt {
    u_ptr<Stmt> BadStmt{};
    explicit ErrorStmt(u_ptr<Stmt> Bad)
        : Stmt(SK_ERROR, Bad->getStartLoc(), Bad->getEndLoc()),
          BadStmt(std::move(Bad)) {}

  public:
    static u_ptr<ErrorStmt> Create(u_ptr<Stmt> Bad) {
      return std::unique_ptr<ErrorStmt>(new ErrorStmt(std::move(Bad)));
    }
    static bool classof(const Stmt *S) { return S->getKind() == SK_ERROR; }
  };

  class ifStmt : public Stmt {
    u_ptr<Expr> Condition{};
    u_ptr<CompoundStmt> Block{};
    u_ptr<elifStmt> elif{};
    u_ptr<CompoundStmt> ElseBlock{};
    ifStmt(llvm::SMLoc IfLoc, u_ptr<Expr> cond, u_ptr<CompoundStmt> block1,
           u_ptr<elifStmt> elif, u_ptr<CompoundStmt> block2);

  public:
    static auto Create(llvm::SMLoc IfLoc, u_ptr<Expr> cond,
                       u_ptr<CompoundStmt> block1, u_ptr<elifStmt> elif,
                       u_ptr<CompoundStmt> block2) {
      return u_ptr<ifStmt>(new ifStmt(IfLoc, std::move(cond), std::move(block1),
                                      std::move(elif), std::move(block2)));
    }
  };

  class elifStmt : public Stmt {
    u_ptr<Stmt> Cond{};
    u_ptr<elifStmt> NextElif{};
    u_ptr<CompoundStmt> Block{};

  public:
    elifStmt(llvm::SMLoc Loc, u_ptr<Stmt> cond, u_ptr<CompoundStmt> block,
             u_ptr<elifStmt> elif);
    ~elifStmt();
    void setNextInChain(u_ptr<elifStmt> Elif) { NextElif = std::move(Elif); }
  };

  class loopStmt : public Stmt {
    friend class ActionRes;
    u_ptr<CompoundStmt> Compound{};

    loopStmt(u_ptr<CompoundStmt> Compound, const llvm::SMLoc Left,
             const llvm::SMLoc Right)
        : Stmt(SK_LOOP, Left, Right), Compound(std::move(Compound)) {}

  protected:
    loopStmt(const StmtKind LoopKind, u_ptr<CompoundStmt> Compound,
             const llvm::SMLoc Left, const llvm::SMLoc Right)
        : Stmt(LoopKind, Left, Right), Compound(std::move(Compound)) {}

  public:
    static auto Create(u_ptr<CompoundStmt> Compound, const llvm::SMLoc Left,
                       const llvm::SMLoc Right) {
      return std::unique_ptr<loopStmt>(
          new loopStmt(std::move(Compound), Left, Right));
    }

    static bool classof(const Stmt *S) { return S->getKind() == SK_LOOP; }
  };

  class forStmt : public loopStmt {
    using ForPtr = u_ptr<forStmt>;
    u_ptr<Stmt> Init{};
    u_ptr<Stmt> Cond{};
    u_ptr<Stmt> Inc{};

    forStmt(const llvm::SMLoc SLoc, const llvm::SMLoc EndLoc, u_ptr<Stmt> Init,
            u_ptr<Stmt> Cond, u_ptr<Stmt> Inc, u_ptr<CompoundStmt> Compound)
        : loopStmt(SK_FOR, std::move(Compound), SLoc, EndLoc),
          Init(std::move(Init)), Cond(std::move(Cond)), Inc(std::move(Inc)) {}

  public:
    static auto Create(llvm::SMLoc L, llvm::SMLoc R, u_ptr<Stmt> Init,
                       u_ptr<Stmt> Cond, u_ptr<Stmt> Inc,
                       u_ptr<CompoundStmt> Compound) {
      return ForPtr(new forStmt(L, R, std::move(Init), std::move(Cond),
                                std::move(Inc), std::move(Compound)));
    }

    static bool classof(const Stmt *S) { return S->getKind() == SK_FOR; }
  };

  class whileStmt : public loopStmt {
    u_ptr<Expr> Condition{};

    whileStmt(llvm::SMLoc whileLoc, llvm::SMLoc endLoc, u_ptr<Expr> Condition,
              u_ptr<CompoundStmt> Compound)
        : loopStmt(SK_WHILE, std::move(Compound), whileLoc, endLoc),
          Condition(std::move(Condition)) {};

  public:
    static auto Create(const llvm::SMLoc While, const llvm::SMLoc End,
                       u_ptr<Expr> Condition, u_ptr<CompoundStmt> Compound) {
      return std::unique_ptr<whileStmt>(
          new whileStmt(While, End, std::move(Condition), std::move(Compound)));
    }
    static bool classof(const Stmt *S) { return S->getKind() == SK_WHILE; }
  };

  class DeclStmt : public Stmt {
    VarDecl *NamedDecl;
    u_ptr<Stmt> Init;

  public:
    DeclStmt(VarDecl *NamedDecl, u_ptr<Stmt> Expression);

    [[nodiscard]] llvm::StringRef getName() const;
    [[nodiscard]] VarDecl *getDecl() const { return NamedDecl; }
    [[nodiscard]] Stmt *getInit() const { return Init.get(); }
    [[nodiscard]] bool isUnitialized() const { return Init.get() == nullptr; }
    static bool classof(const Stmt *S) { return S->getKind() == SK_VARDECL; }
  };

  class ReturnStmt : public Stmt {
    using ReturnStmtPtr = std::unique_ptr<ReturnStmt>;
    u_ptr<Expr> ReturnExpr{};
    explicit ReturnStmt(u_ptr<Expr> exprNode, llvm::SMLoc RetStart,
                        llvm::SMLoc RetEnd);

  public:
    static ReturnStmtPtr Create(u_ptr<Expr> E, SourceLoc RetStart,
                                SourceLoc End);

    static ReturnStmtPtr Naked(const SourceLoc RetStart,
                               const SourceLoc RetEnd) {
      return ReturnStmtPtr(new ReturnStmt(nullptr, RetStart, RetEnd));
    }

    [[nodiscard]] Expr *getExpr() const { return ReturnExpr.get(); }
    [[nodiscard]] bool isNaked() const { return ReturnExpr.get() == nullptr; }
    static bool classof(const Stmt *S) { return S->getKind() == SK_RETURN; }
  };

  class BreakStmt : public Stmt {
    BreakStmt(const llvm::SMLoc BreakLoc, const llvm::SMLoc EndOfBreakLoc)
        : Stmt(SK_BREAK, BreakLoc, EndOfBreakLoc) {}

  public:
    static u_ptr<BreakStmt> Create(const llvm::SMLoc BreakLoc,
                                   const llvm::SMLoc EndOfBreakLoc) {
      return u_ptr<BreakStmt>(new BreakStmt(BreakLoc, EndOfBreakLoc));
    }
  };

  class NextStmt : public Stmt {
    NextStmt(const llvm::SMLoc BreakLoc, const llvm::SMLoc EndOfBreakLoc)
        : Stmt(SK_NEXT, BreakLoc, EndOfBreakLoc) {}

  public:
    static u_ptr<NextStmt> Create(const llvm::SMLoc NextLoc,
                                  const llvm::SMLoc NextLocEnd) {
      return u_ptr<NextStmt>(new NextStmt(NextLoc, NextLocEnd));
    }
  };

  class EmptyStmt : public Stmt {
    explicit EmptyStmt(SourceLoc SemicolonLoc) : Stmt(SK_EMPTY, SemicolonLoc) {}

  public:
    static auto Create(SourceLoc SemicolonLoc) {
      return u_ptr<EmptyStmt>(new EmptyStmt(SemicolonLoc));
    }
  };
}

}// namespace funLang
