#include "Parse.hpp"
#include "Lex.hpp"
#include <iostream>
#include <memory>
#include <utility>

TokValCat currentTok;

void reportError(std::string errMessage) {

  std::cout << errMessage << std::endl;

  exit(1);
}
std::unique_ptr<typeNode> type() {
  std::unique_ptr<typeNode> typeptr =
      std::make_unique<typeNode>(currentTok.syntactic_category);
  if (typeptr->invalid()) {
    reportError("Unexpected token, expected type specifier");
    return nullptr;
  }

  std::cout << "Parsed type" << std::endl;
  return std::move(typeptr);
}

std::unique_ptr<funcNode> func() {
  std::unique_ptr<typeNode> type_ptr = type();
  if (!type_ptr) {
    reportError("Failed at parsing type for function");
    return nullptr;
  }
  std::unique_ptr<protoNode> proto_ptr = proto();
  if (!proto_ptr) {
    reportError("Failed to parse prototype");
    return nullptr;
  }

  if ((currentTok = getNextTok()).syntactic_category != Tok::LPAREN) {
    reportError("Expected '{' after prototype declaration");
    return nullptr;
  }

  std::unique_ptr<compoundStmtNode> compoundStmt_ptr = compoundStmt();

  if (!compoundStmt_ptr) {
    reportError("Compound Statement failed to parse");
    return nullptr;
  }

  std::unique_ptr<funcNode> func_ptr = std::make_unique<funcNode>(
      std::move(type_ptr), std::move(proto_ptr), std::move(compoundStmt_ptr));
  return std::move(func_ptr);
}
std::unique_ptr<programNode> program() {
  std::unique_ptr<programNode> head = std::make_unique<programNode>();
  if (!functions()) {
    reportError("Failed to parse functions");
    return nullptr;
  }
  return std::move(head);
}

std::unique_ptr<functionsNode> functions() {
  // functionsNode()
  std::unique_ptr<funcNode> func_ptr = func();
  if (!func_ptr) {
    reportError("Failed to parse a function");
    return nullptr;
  }
  std::cout << "Parsed function" << std::endl;
  std::unique_ptr<functionsNode> myFuncs_ptr =
      std::make_unique<functionsNode>(std::move(func_ptr));
  if ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    myFuncs_ptr->setMoreFuncs(functions());
  }
  return std::move(myFuncs_ptr);
}

std::unique_ptr<protoNode> proto() { return nullptr; }

std::unique_ptr<compoundStmtNode> compoundStmt() { return nullptr; }

int parse() {
  std::unique_ptr<programNode> tree;
  initInp("./example_parse.txt");
  currentTok = getNextTok();
  if (!(tree = program())) {
    std::cout << "failed to parse program :(" << std::endl;

    return 1;
  } else {
    std::cout << "Yay we parsed!" << std::endl;
  }
  return 0;
}
int yay() { return 1; }

int main() { return parse(); }
