#pragma once
#include "AST/Decl.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"
#include "Sema/Sema.hpp"
#include <initializer_list>
#include <memory>
#include <utility>
import Lex;

using namespace funLang;
class Parser {
private:
  std::unique_ptr<Lexer> lexer;
  DiagEngine diags;
  std::unique_ptr<SemaAnalyzer> Semantics;

  bool error;

public:
  Parser(std::unique_ptr<Lexer> Lex, DiagEngine &Diags,
         std::unique_ptr<SemaAnalyzer> Sema)
      : lexer(std::move(Lex)), error(false), diags(Diags),
        Semantics(std::move(Sema)) {}
  Token peek();
  Token previous();
  Token &advance();
  bool expect(Basic::tok::Tag Tok);
  bool nextTokIs(Basic::tok::Tag Tok);
  template<typename... Ts>
  bool nextIsOneOf(tok::Tag T1, Ts... Tss) {
    return peek().isOneOf(T1, Tss...);
  }
  Token lookahead(uint32_t HowMuch);
  void reportExpect(Basic::tok::Tag Expected, Token Received);
  void emitWarning(unsigned int DiagId, llvm::SMLoc Loc, llvm::StringRef Name);

public:
  std::unique_ptr<CompilationUnit> program();
  std::unique_ptr<FunctionDecl> function();
  std::unique_ptr<RecordDecl> typeDecl();
  std::unique_ptr<TypeUse> type();
  u_ptr<llvm::SmallVector<u_ptr<ParamDecl>>> parameters();
  std::unique_ptr<VarDecl> nameDecl();
  std::unique_ptr<ParamDecl> paramDecl();
  std::unique_ptr<CompoundStmt> compoundStmt();
  std::unique_ptr<Stmt> simpleStmt();
  std::unique_ptr<DeclStmt> declStmt();
  std::unique_ptr<forStmt> forStmt();
  std::unique_ptr<whileStmt> whileStmt();
  std::unique_ptr<loopStmt> loopStmt();
  std::unique_ptr<ReturnStmt> returnStmt();
  std::unique_ptr<ifStmt> ifStmt();
  std::unique_ptr<Expr> match();
  std::unique_ptr<Expr> expr();
  std::unique_ptr<Expr> assign();
  std::unique_ptr<Expr> eqExpr();
  std::unique_ptr<Expr> cmpExpr();
  std::unique_ptr<Expr> addExpr();
  std::unique_ptr<Expr> multdiv();
  std::unique_ptr<Expr> unary();
  std::unique_ptr<Expr> primary();
  std::unique_ptr<Expr> fnCall();
  std::unique_ptr<Expr> postfix();
  std::unique_ptr<Expr> arrayIndex(std::unique_ptr<Expr> Input);
  std::unique_ptr<Expr> deref();
  u_ptr<llvm::SmallVector<u_ptr<Expr>>> callArgs();
};
