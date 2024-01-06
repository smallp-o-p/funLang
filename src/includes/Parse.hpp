#pragma once
#include "AST.hpp"
#include <memory>

extern TokValCat currentTok;

void reportError(std::string format, ...);

void pushTokInQueue();

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

std::unique_ptr<exprNode> expr();

std::unique_ptr<returnNode> ret();
