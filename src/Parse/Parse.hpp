#pragma once
#include "AST.hpp"
#include "Lex.hpp"
#include "TokenTags.hpp"
#include <initializer_list>
#include <memory>

enum currentNT { STMT, FUNCTION }; // what non-terminal we failed to parse

class Parser {
private:
  std::unique_ptr<Lexer> lexer;
  DiagEngine diags;
  bool error;

public:
  Parser(std::unique_ptr<Lexer> lex, DiagEngine &diags)
	  : lexer(std::move(lex)), error(false), diags(diags) {}
  bool atEnd();
  bool isOneOf(std::initializer_list<Basic::tok::Tag> toExpect, bool peeking);
  Token peek();
  Token &previous();
  Token & advance();
  bool expect(Basic::tok::Tag tok);
  bool check(Basic::tok::Tag tok);
  Token lookahead(uint32_t howMuch);
  void reportExpect(Basic::tok::Tag expected, Token received);
public:
  std::unique_ptr<FunctionsNode> functions();
  std::unique_ptr<FunctionNode> function();
  std::unique_ptr<TypeNode> type();
  std::unique_ptr<PrototypeNode> proto();
  std::unique_ptr<ArgumentsNode> arguments();
  std::unique_ptr<ArgNode> arg();
  std::unique_ptr<CompoundStmt> compoundStmt();
  std::unique_ptr<Stmt> simpleStmt();
  std::unique_ptr<VarDecl> decl();
  std::unique_ptr<returnNode> returnStmt();
  std::unique_ptr<Expr> expr();
  std::unique_ptr<Expr> assign();
  std::unique_ptr<Expr> eqExpr();
  std::unique_ptr<Expr> cmpExpr();
  std::unique_ptr<Expr> addExpr();
  std::unique_ptr<Expr> multdiv();
  std::unique_ptr<Expr> unary();
  std::unique_ptr<Expr> primary();
  std::unique_ptr<Expr> fnCall();
  std::unique_ptr<callArgList> callArgs();
  bool recoverFromError(currentNT whereWeFailed);

public:
  std::unique_ptr<ProgramNode> program();
};
