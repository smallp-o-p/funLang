#pragma once
#include "AST.hpp"
#include "Lex.hpp"
#include <cstdarg>
#include <cstdio>
#include <initializer_list>
#include <iostream>
#include <memory>
#include <stack>
#include <unordered_map>
#include <utility>
#include <vector>

enum currentNT { STMT, FUNCTION }; // what non-terminal we failed to parse
int initInstance(const std::string &fp, bool usingString = false);
bool atEnd();
bool match(std::initializer_list<Lexer::Tag> toExpect);
bool check(Lexer::Tag tok);
Lexer::Token peek();
Lexer::Token previous();
Lexer::Token advance();
Lexer::Token lookahead(int howMuch);
void backup();
void reportError(const char *format, ...);
void recoverFromError(currentNT whereWeFailed);

class Parser {
private:
  LexedTokensSingleton &lexer;
  bool error;

private:
  Parser(LexedTokensSingleton &lex) : lexer(lex), error(false) {}
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

public:
  std::unique_ptr<ProgramNode> program();
};
