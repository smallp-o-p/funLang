#pragma once
#include "AST.hpp"
#include "Lex.hpp"
#include "Basic.hpp"
#include "Diag.hpp"
#include "Sema.hpp"
#include <initializer_list>
#include <memory>
#include <utility>

enum currentNT { STMT, FUNCTION }; // what non-terminal we failed to parse
using namespace funLang;
class Parser {
private:
  std::unique_ptr<Lexer> lexer;
  DiagEngine diags;
  std::unique_ptr<SemaAnalyzer> semantics;

  bool error;

public:
  Parser(std::unique_ptr<Lexer> lex, DiagEngine &diags)
	  : lexer(std::move(lex)), error(false), diags(diags) {}
  bool atEnd();
  bool isOneOf(std::initializer_list<Basic::tok::Tag> toExpect, bool peeking);
  Token peek();
  Token &previous();
  Token &advance();
  bool expect(Basic::tok::Tag tok);
  bool check(Basic::tok::Tag tok);
  Token lookahead(uint32_t howMuch);
  void reportExpect(Basic::tok::Tag expected, Token received);
  void emitWarning(unsigned int diagID, llvm::SMLoc loc, llvm::StringRef name);
public:
  std::unique_ptr<CompilationUnit> functions();
  std::unique_ptr<TopLevelDecls> topLevels();
  std::unique_ptr<FunctionNode> function();
  std::unique_ptr<TypeDecl> typeDecl();
  std::unique_ptr<TypeProperties> typeProperties();
  std::unique_ptr<TypeUse> type();
  std::unique_ptr<ArgsList> arguments();
  std::unique_ptr<ArgDecl> arg();
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
  std::unique_ptr<FunctionCall> fnCall();
  std::unique_ptr<CallArgList> callArgs();
  bool recoverFromError(currentNT whereWeFailed);

public:
  std::unique_ptr<CompilationUnit> program();
};
