#pragma once
#include "AST.hpp"
#include <memory>

extern TokValCat currentTok;

void reportError(std::string errMessage);

std::unique_ptr<programNode> program();

std::unique_ptr<functionsNode> functions();

std::unique_ptr<funcNode> func();

std::unique_ptr<compoundStmtNode> compoundStmt();

std::unique_ptr<typeNode> type();

std::unique_ptr<protoNode> proto();
