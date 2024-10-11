//
// Created by will on 10/6/24.
//
module;
#include "AST/Decl.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"
#include "Sema/Sema.hpp"
#include <memory>
#include <utility>
export module Parse;
import Basic.Result;
import Lex;

namespace funLang {
export {
  class ParseTester;
  class Parser {
    friend class ParseTester;
    Lexer &Lexer_;
    DiagEngine &Diags;
    SemaAnalyzer &Semantics;
    mutable bool Err;

    Parser(Lexer &LexerObj, DiagEngine &Diags, SemaAnalyzer &Sema) : Lexer_(LexerObj), Diags(Diags),
                                                                     Semantics(Sema), Err(false) {}
    Token peek() const { return Lexer_.peek(); }
    Token previous() const { return Lexer_.previous(); }
    Token advance() const { return Lexer_.advance(); }
    void reportExpect(const tok::Tag Expected, Token Received) const {
      Diags.emitDiagMsg(Received.getLoc(), Diag::err_expected,
                        getTokenName(Expected), Received.getLexeme());
    }
    bool expect(const tok::Tag T) const {
      if (!Lexer_.advance().is(T)) {
        reportExpect(T, previous());
        Err = true;
        return false;
      }
      return true;
    }
    bool nextTokIs(const tok::Tag T) const {
      return peek().is(T);
    }
    template<typename... Ts>
    bool nextIsOneOf(tok::Tag T1, Ts... Tss) {
      return peek().isOneOf(T1, Tss...);
    }
    Token lookahead(const uint_fast8_t HowMuch) const { return Lexer_.lookahead(HowMuch); }
    template<typename... Ts>
    void skipUntil(const tok::Tag T, Ts... Tss) const {
      while (!advance().isOneOf(T, Tss...)) {}
    }

    using StmtResult = ActionRes<Stmt>;
    using ExprResult = ActionRes<Expr>;
    using CompileResult = ActionRes<CompilationUnit>;
    using DeclResult = ActionRes<Decl>;
    using ParamVector = ActionRes<llvm::SmallVector<ParamDecl>>;
    using ExprVector = ActionRes<llvm::SmallVector<Expr>>;

    CompileResult program() {
      llvm::DenseMap<IDTableEntry *, std::unique_ptr<Decl>> GlobalDecls;
      while (true) {
        DeclResult Toplevel;
        if (nextTokIs(tok::kw_struct)) {
          Toplevel = typeDecl();
        } else if (nextTokIs(tok::identifier) || peek().isBaseType()) {
          Toplevel = function();
        }
        if (!Toplevel) {
          return CompileResult::InvalidRes();
        }

        Semantics.actOnTopLevelDecl(Toplevel.move());
        if (Lexer_.atEnd()) {
          break;
        }
      }
      return CompileResult(std::make_unique<CompilationUnit>(std::move(GlobalDecls)));
    }

    DeclResult typeDecl() {
      if (!expect(tok::identifier)) {
        return DeclResult::InvalidRes();
      }
      Token StructName = previous();

      if (!expect(tok::l_brace)) {
        return DeclResult::InvalidRes();
      }
      auto NewType = Semantics.enterStructScope(StructName);
      while (true) {
        if (nextTokIs(tok::r_brace)) {
          break;
        }
        DeclResult Field = nameDecl();
        if (!Field) {
          skipUntil(tok::semi, tok::r_brace);
          return DeclResult::InvalidRes();
        }
        Semantics.actOnStructMemberDecl(Field.move());
        if (!expect(tok::semi)) {
          skipUntil(tok::semi, tok::r_brace);
          return DeclResult::InvalidRes();
        }
      }
      return DeclResult(std::move(NewType));
    }

    DeclResult function();
    DeclResult nameDecl();
    DeclResult paramDecl();
    ParamVector parameters();
    StmtResult compoundStmt();
    StmtResult stmt();
    StmtResult declStmt();
    StmtResult forStmt();
    StmtResult whileStmt();
    StmtResult loopStmt();
    StmtResult returnStmt();
    StmtResult ifStmt();
    ExprResult match();
    ExprResult expr();
    ExprResult assign();
    ExprResult eqExpr();
    ExprResult cmpExpr();
    ExprResult addExpr();
    ExprResult multdivExpr();
    ExprResult unaryExpr();
    ExprResult fnCallExpr();
    ExprResult postfixExpr();
    ExprResult arrayIndexExpr();
    ExprResult derefExpr();
    ExprVector callArgs();
    ActionRes<TypeUse> type();
  };
}
}// namespace funLang
