#pragma once
#include "AST/AST.hpp"
#include "Basic/Basic.hpp"
#include "Basic/Diag.hpp"
#include "Lex/Lex.hpp"
#include "Sema/Sema.hpp"
#include <initializer_list>
#include <memory>
#include <utility>

enum CurrentNonTerminal {
  STMT,
  FUNCTION
}; // what non-terminal we failed to parse
using namespace funLang;
class Parser {
private:
  std::unique_ptr<Lexer> lexer;
  DiagEngine diags;
  std::unique_ptr<SemaAnalyzer> semantics;

  bool error;

public:
  Parser(std::unique_ptr<Lexer> Lex, DiagEngine &Diags,
         std::unique_ptr<SemaAnalyzer> Sema)
      : lexer(std::move(Lex)), error(false), diags(Diags),
        semantics(std::move(Sema)) {}
  bool atEnd();
  bool isOneOf(std::initializer_list<Basic::tok::Tag> Tok, bool Peeking);
  Token peek();
  Token previous();
  Token &advance();
  bool expect(Basic::tok::Tag Tok);
  bool check(Basic::tok::Tag Tok);
  Token lookahead(uint32_t HowMuch);
  void reportExpect(Basic::tok::Tag Expected, Token Received);
  void emitWarning(unsigned int DiagId, llvm::SMLoc Loc, llvm::StringRef Name);

public:
  std::unique_ptr<CompilationUnit> program();
  std::unique_ptr<TopLevelDecls> topLevels();
  std::unique_ptr<FunctionNode> function();
  std::unique_ptr<TypeDecl> typeDecl();
  std::unique_ptr<TypeProperties> typeProperties();
  std::unique_ptr<TypeUse> type();
  std::unique_ptr<ArgsList> arguments();
  std::unique_ptr<ArgDecl> arg();
  std::unique_ptr<VarDecl> nameDecl();
  std::unique_ptr<CompoundStmt> compoundStmt();
  std::unique_ptr<Stmt> simpleStmt();
  std::unique_ptr<VarDeclStmt> declStmt();
  std::unique_ptr<forStmt> forStmt();
  std::unique_ptr<whileStmt> whileStmt();
  std::unique_ptr<loopStmt> loopStmt();
  std::unique_ptr<ReturnStmt> returnStmt();
  std::unique_ptr<Expr> expr();
  std::unique_ptr<Expr> assign();
  std::unique_ptr<Expr> eqExpr();
  std::unique_ptr<Expr> cmpExpr();
  std::unique_ptr<Expr> addExpr();
  std::unique_ptr<Expr> multdiv();
  std::unique_ptr<Expr> unary();
  std::unique_ptr<Expr> primary();
  std::unique_ptr<Expr> fnCall();
  std::unique_ptr<CallArgList> callArgs();
  bool recoverFromError(CurrentNonTerminal WhereWeFailed);
};
