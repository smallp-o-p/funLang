#pragma once
#include "Lex.hpp"
#include "ParseTree.hpp"
#include <cstdarg>
#include <cstdio>
#include <iostream>
#include <memory>
#include <stack>
#include <unordered_map>
#include <utility>
#include <vector>

int initInstance(const std::string &fp, bool usingString = false);
bool atEnd();
bool match(std::vector<Lexer::Tag> toks);
bool check(Lexer::Tag tok);
Lexer::LexerToken peek();
Lexer::LexerToken previous();
Lexer::LexerToken advance();
void backup();
void reportError(const char *format, ...);

std::unique_ptr<ProgramNode> program();
std::unique_ptr<FunctionsNode> functions();
std::unique_ptr<FunctionNode> function();
std::unique_ptr<PrototypeNode> proto();
std::unique_ptr<ArgumentsNode> arguments();
std::unique_ptr<ArgNode> arg();
std::unique_ptr<CompoundStmt> compoundStmt();
std::unique_ptr<Stmt> simpleStmt();
std::unique_ptr<Stmt> decl();
std::unique_ptr<Stmt> expr();
std::unique_ptr<Stmt> returnStmt();
std::unique_ptr<Expr> assign();
std::unique_ptr<Expr> eqExpr();
std::unique_ptr<Expr> cmpExpr();
std::unique_ptr<Expr> addExpr();
std::unique_ptr<Expr> multdiv();
std::unique_ptr<Expr> unary();
std::unique_ptr<Expr> primary();
std::unique_ptr<Expr> fnCall();
std::unique_ptr<callArgs> callArgs();
