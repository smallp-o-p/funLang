//
// Created by will on 10/6/24.
//
module;
#include <llvm/ADT/StringMapEntry.h>
#include <memory>
#include <utility>
export module Parse;
import Basic;
import Diag;
import Lex;
import Sema;
import llvm;
import std_modules;

namespace funLang {
export {
  class Parser {
    Lexer &Lexer_;
    DiagEngine &Diags;
    SemaAnalyzer &Semantics;
    mutable bool Err;

    Parser(Lexer &LexerObj, DiagEngine &Diags, SemaAnalyzer &Sema)
        : Lexer_(LexerObj), Diags(Diags), Semantics(Sema), Err(false) {}
    Token peek() const { return Lexer_.peek(); }
    Token previous() const { return Lexer_.previous(); }
    Token advance() const { return Lexer_.advance(); }
    void reportExpect(const tok::Tag Expected, const Token &Received) const {
      Diags.emitDiagMsg(Received.getLoc(), Diag::err_expected,
                        getTokenName(Expected), Received.getLexeme());
    }
    template<typename... Ts>
    void reportExpectOneOf(const Token &Received, Ts... ExpectOneOf) {
      Diags.emitDiagMsg(Received.getLoc(), Diag::err_expect_one_of,
                        Received.getLexeme(), ExpectOneOf.getLexeme()...);
    }
    bool expect(const tok::Tag T) const {
      if (!Lexer_.advance().is(T)) {
        reportExpect(T, previous());
        Err = true;
        return false;
      }
      return true;
    }
    std::optional<Token> expectIdentifier() {
      if (const auto T = Lexer_.advance(); T.isIdentifier()) {
        return std::make_optional(T);
      }
      return std::nullopt;
    }
    bool nextTokIs(const tok::Tag T) const { return peek().is(T); }
    template<typename... Ts> bool nextIsOneOf(tok::Tag T1, Ts... Tss) {
      return peek().isOneOf(T1, Tss...);
    }
    Token lookahead(const uint_fast8_t HowMuch) const {
      return Lexer_.lookahead(HowMuch);
    }
    template<typename... Ts> void skipUntil(const tok::Tag T, Ts... Tss) const {
      while (!advance().isOneOf(T, Tss...)) {}
    }

    using StmtResult = ActionRes<Stmt>;
    using CompoundStmtResult = ActionRes<CompoundStmt>;
    using ExprResult = ActionRes<Expr>;
    using CompileResult = ActionRes<CompilationUnit>;
    using DeclResult = ActionRes<Decl>;
    using ParamVector = ActionRes<llvm::SmallVector<ParamDecl>>;
    using ExprVector = ActionRes<llvm::SmallVector<Expr>>;
    StmtResult StmtError() { return StmtResult::InvalidRes(); }
    ExprResult ExprError() { return ExprResult::InvalidRes(); }
    DeclResult DeclError() { return DeclResult::InvalidRes(); }
    CompileResult program();

    DeclResult typeDecl();
    DeclResult functionDecl();
    ActionRes<std::pair<Symbol *, u_ptr<ParamDecl>>> fnNameAndParams();
    DeclResult nameDecl();
    DeclResult paramDecl();
    DeclResult parameters();
    StmtResult compoundStmt();
    StmtResult stmt();
    StmtResult declStmt();
    StmtResult forStmt();
    StmtResult whileStmt();
    StmtResult parseLoop();
    StmtResult returnStmt();
    StmtResult ifStmt();
    StmtResult exprStmt();
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
    ExprResult primaryExpr();
    ExprVector callArgs();
    ActionRes<TypeUse> typeUse();
  };
}
}// namespace funLang
