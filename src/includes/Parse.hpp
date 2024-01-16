#pragma once
#include "AST.hpp"
#include "Lex.hpp"
#include <memory>
#include <vector>

int initInstance(const std::string &fp, bool usingString = false);
bool atEnd();
bool match(std::vector<Tok::Token> toks);
bool check(Tok::Token tok);
TokValCat peek();
TokValCat previous();
TokValCat advance();
void backup();
void reportError(const char *format, ...);

std::unique_ptr<programNode> program();
std::unique_ptr<functionsNode> functions();
std::unique_ptr<funcNode> func();
std::unique_ptr<compoundStmtNode> compoundStmt();
std::unique_ptr<typeNode> type();
std::unique_ptr<protoNode> proto();
std::unique_ptr<argsNode> args();
std::unique_ptr<argNode> arg();
std::unique_ptr<simpleListNode> simpleList();
std::unique_ptr<simpleStmtNode> simpleStmt();
std::unique_ptr<declareNode> declare();
std::unique_ptr<returnNode> ret();
std::unique_ptr<exprNode> expr();
std::unique_ptr<assignExprNode> assign();
std::unique_ptr<eqExprNode> eqExpr();
std::unique_ptr<cmpExprNode> cmpExpr();
std::unique_ptr<addExprNode> addExpr();
std::unique_ptr<multdivNode> multdiv();
std::unique_ptr<unaryNode> unary();
std::unique_ptr<primaryNode> primary();
std::unique_ptr<fnCallNode> fnCall();
std::unique_ptr<callArgsNode> callArgs();
