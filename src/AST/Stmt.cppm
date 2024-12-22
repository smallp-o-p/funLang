//
// Created by will on 10/27/24.
//
module;
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"
export module AST:Stmt;
import Basic;

namespace funLang {
  class VarDecl;

  export
  {
    class elifStmt;
    class Stmt {
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
        SK_ERROREXPR,
        SK_DECL
      };

    protected:
      const StmtKind Kind;
      const llvm::SMLoc StartLoc{}, EndLoc{};

    public:
      explicit Stmt(const StmtKind K) : Kind(K) {
      }
      Stmt(const StmtKind K, const llvm::SMLoc Loc): Kind(K), StartLoc(Loc), EndLoc(Loc) {
      }
      Stmt(const StmtKind K, const llvm::SMLoc Left, const llvm::SMLoc Right)
        : Kind(K), StartLoc(Left), EndLoc(Right) {
      }

      [[nodiscard]] StmtKind getKind() const { return Kind; }
      [[nodiscard]] llvm::SMLoc getStartLoc() const { return StartLoc; }
      [[nodiscard]] llvm::SMLoc getEndLoc() const { return EndLoc; }
    };

    class CompoundStmt : public Stmt {
      llvm::SmallVector<u_ptr<Stmt> > Statements{};

    public:
      explicit CompoundStmt(llvm::SmallVector<u_ptr<Stmt> > Simples)
        : Stmt(SK_COMPOUND), Statements(std::move(Simples)) {
      }

      static bool classof(const Stmt *S) { return S->getKind() == SK_COMPOUND; }
      llvm::SmallVector<u_ptr<Stmt> > &getStatements() { return Statements; }
    };

    class ErrorStmt : public Stmt {
      u_ptr<Stmt> BadStmt{};

    public:
      explicit ErrorStmt(u_ptr<Stmt> Bad)
        : Stmt(SK_ERROR, Bad->getStartLoc(), Bad->getEndLoc()), BadStmt(std::move(Bad)) {
      }
      static bool classof(const Stmt *S) { return S->getKind() == SK_ERROR; }
    };

    class ifStmt : public Stmt {
      u_ptr<Stmt> Condition{};
      u_ptr<CompoundStmt> Block{};
      u_ptr<elifStmt> elif{};
      u_ptr<CompoundStmt> ElseBlock{};

    public:
      ifStmt(llvm::SMLoc IfLoc,
             u_ptr<Stmt> cond,
             u_ptr<CompoundStmt> block1,
             u_ptr<elifStmt> elif,
             u_ptr<CompoundStmt> block2);
    };

    class elifStmt : public Stmt {
      u_ptr<Stmt> Cond{};
      u_ptr<elifStmt> NextElif{};
      u_ptr<CompoundStmt> Block{};

    public:
      elifStmt(llvm::SMLoc Loc,
               u_ptr<Stmt> cond,
               u_ptr<CompoundStmt> block,
               u_ptr<elifStmt> elif);
      ~elifStmt();
      void setNextInChain(u_ptr<elifStmt> Elif) { NextElif = std::move(Elif); }
    };

    class loopStmt : public Stmt {
      u_ptr<CompoundStmt> Compound{};

    public:
      loopStmt(const StmtKind LoopType, u_ptr<CompoundStmt> compound, const llvm::SMLoc Left, const llvm::SMLoc Right)
        : Stmt(LoopType, Left, Right), Compound(std::move(compound)) {
      }
      loopStmt(u_ptr<CompoundStmt> Compound, const llvm::SMLoc Loc) : Stmt(SK_LOOP, Loc),
                                                                      Compound(std::move(Compound)) {
      }
      static bool classof(const Stmt *S) { return S->getKind() == SK_FOR; }
    };

    class forStmt : public loopStmt {
      u_ptr<Stmt> Init{};
      u_ptr<Stmt> Cond{};
      u_ptr<Stmt> Inc{};

    public:
      forStmt(llvm::SMLoc EndLoc,
              llvm::SMLoc loc,
              u_ptr<Stmt> Init,
              u_ptr<Stmt> Cond,
              u_ptr<Stmt> Inc,
              u_ptr<CompoundStmt> Compound);

      static bool classof(const Stmt *S) { return S->getKind() == SK_FOR; }
    };

    class whileStmt : public loopStmt {
      u_ptr<Stmt> Condition{};

    public:
      whileStmt(u_ptr<Stmt> Condition,
                u_ptr<CompoundStmt> Compound,
                llvm::SMLoc whileLoc,
                llvm::SMLoc endLoc);

      static bool classof(const Stmt *S) { return S->getKind() == SK_WHILE; }
    };

    class DeclStmt : public Stmt {
      VarDecl *NamedDecl;
      u_ptr<Stmt> Init;

    public:
      DeclStmt(llvm::SMLoc NameStart,
               llvm::SMLoc SemiLoc,
               VarDecl *NamedDecl,
               std::unique_ptr<Stmt> Expression);

      [[nodiscard]] llvm::StringRef getName() const;
      [[nodiscard]] VarDecl *getDecl() const { return NamedDecl; }
      [[nodiscard]] Stmt *getInit() const { return Init.get(); }
      [[nodiscard]] bool isUnitialized() const { return Init == nullptr; }
      static bool classof(const Stmt *S) { return S->getKind() == SK_VARDECL; }
    };

    class ReturnStmt : public Stmt {
      std::unique_ptr<Stmt> ReturnExpr;

    public:
      explicit ReturnStmt(std::unique_ptr<Stmt> exprNode);

      [[nodiscard]] Stmt *getExpr() const { return ReturnExpr.get(); }
      [[nodiscard]] bool isNaked() const { return ReturnExpr == nullptr; }
      static bool classof(const Stmt *S) { return S->getKind() == SK_RETURN; }
    };

    class BreakStmt : public Stmt {
    public:
      BreakStmt(const llvm::SMLoc BreakLoc, const llvm::SMLoc EndOfBreakLoc) : Stmt(SK_BREAK, BreakLoc, EndOfBreakLoc) {
      }
    };

    class NextStmt : public Stmt {
    public:
      NextStmt(const llvm::SMLoc BreakLoc, const llvm::SMLoc EndOfBreakLoc) : Stmt(SK_NEXT, BreakLoc, EndOfBreakLoc) {
      }
    };
  }
} // namespace funLang
