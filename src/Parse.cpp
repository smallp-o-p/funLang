#include "Parse.hpp"
#include "AST.hpp"
#include "Lex.hpp"
#include <cstdarg>
#include <cstdio>
#include <deque>
#include <iostream>
#include <memory>
#include <stack>
#include <utility>

TokValCat currentTok;

std::deque<TokValCat> push_backs;

std::stack<TokValCat> pastToks;

void reportError(std::string format, ...) {
  std::va_list errs;
  va_start(errs, format);
  std::vsnprintf(format.data(), format.size(), format.data(), errs);
}

TokValCat getTok() {
  if (!push_backs.empty()) {
    TokValCat tok = push_backs.front();
    push_backs.pop_front();
    std::cout << tok.lexeme << std::endl;
    return tok;
  }
  TokValCat tok = getNextTok();
  std::cout << "Current token: " << tok.lexeme << std::endl;
  return tok;
}
/*
 * Get next tok and check if it's what we want, don't eat it*/
bool check(Tok::Token tok) {
  currentTok = getTok();
  if (currentTok.syntactic_category != tok) {
    return false;
  }
  return true;
}

bool isDeclarableType(Tok::Token tok) {
  switch (tok) {
  case (Tok::I32):
  case (Tok::I64):
  case (Tok::F32):
  case (Tok::F64):
  case (Tok::CHAR):
  case (Tok::STRING):
  case (Tok::BOOL):
    return true;
  default:
    return false;
  }
}

void pushTokInQueue() { push_backs.push_back(currentTok); }

std::unique_ptr<typeNode> type() {
  currentTok = getTok();
  std::cout << "In type(), lexeme is: " << currentTok.lexeme << std::endl;
  std::unique_ptr<typeNode> typeptr =
      std::make_unique<typeNode>(currentTok.syntactic_category);
  if (typeptr->invalid()) {
    reportError("Unexpected token '%s', expected type specifier",
                currentTok.lexeme.c_str());

    return nullptr;
  }
  std::cout << "Parsed type" << std::endl;
  return std::move(typeptr);
}

std::unique_ptr<funcNode> func() {
  std::unique_ptr<typeNode> type_ptr = type();
  if (!type_ptr) {
    return nullptr;
  }
  std::unique_ptr<protoNode> proto_ptr = proto();
  if (!proto_ptr) {
    return nullptr;
  }

  std::unique_ptr<compoundStmtNode> compoundStmt_ptr = compoundStmt();

  if (!compoundStmt_ptr) {
    return nullptr;
  }

  std::unique_ptr<funcNode> func_ptr = std::make_unique<funcNode>(
      std::move(type_ptr), std::move(proto_ptr), std::move(compoundStmt_ptr));
  return std::move(func_ptr);
}

std::unique_ptr<protoNode> proto() {
  if (!check(Tok::IDENTIFIER)) {
    reportError("Expected IDENTIFIER, received '%s'",
                currentTok.lexeme.c_str());
    return nullptr;
  }
  std::string name_in_question = currentTok.lexeme;

  if (!check(Tok::LPAREN)) {
    reportError("Expected '(', received '%s'\n", currentTok.lexeme.c_str());
    return nullptr;
  }

  currentTok = getTok(); // eat '('
  std::unique_ptr<argsNode> args_ptr = args();

  if (!args_ptr) {
    return nullptr;
  }

  if (!check(Tok::RPAREN)) {
    reportError("Expected ')', received '%s'\n", currentTok.lexeme.c_str());
    return nullptr;
  }
  currentTok = getTok(); // eat ')'
  return std::move(
      std::make_unique<protoNode>(name_in_question, std::move(args_ptr)));
}

std::unique_ptr<argsNode> args() {

  std::unique_ptr<argNode> arg_ptr = arg();

  if (!arg_ptr) {
    return nullptr;
  }

  if (check(Tok::COMMA)) {
    return std::move(
        std::make_unique<argsNode>(std::move(arg_ptr), std::move(args())));
  }
  return std::move(std::make_unique<argsNode>(std::move(arg_ptr)));
}

std::unique_ptr<argNode> arg() {
  std::unique_ptr<typeNode> type_ptr = type();

  if (!type()) {
    return nullptr;
  }

  if (!check(Tok::IDENTIFIER)) {
    reportError("Expected IDENTIFIER, received %s", currentTok.lexeme.c_str());
    return nullptr;
  }
  std::string name_in_question = currentTok.lexeme;

  return std::move(
      std::make_unique<argNode>(std::move(type_ptr), name_in_question));
}
std::unique_ptr<compoundStmtNode> compoundStmt() {
  if (!check(Tok::LCURLY)) {
    reportError("Expected '{', received '%s'", currentTok.lexeme.c_str());
    return nullptr;
  }
  std::unique_ptr<simpleListNode> simple_list_ptr = simpleList();

  if (!simple_list_ptr) {
    return nullptr;
  }

  if (!check(Tok::RCURLY)) {
    reportError("Expected '}', received '%s'", currentTok.lexeme.c_str());
    return nullptr;
  }

  currentTok = getTok(); // eat '}'

  return std::move(
      std::make_unique<compoundStmtNode>(std::move(simple_list_ptr)));
}

std::unique_ptr<simpleListNode> simpleList() {
  std::unique_ptr<simpleStmtNode> stmt_ptr = simpleStmt();
  if (!stmt_ptr) {
    return nullptr;
  }

  return std::move(std::make_unique<simpleListNode>(std::move(stmt_ptr),
                                                    std::move(simpleList())));
}

std::unique_ptr<simpleStmtNode> simpleStmt() {
  if (check(Tok::RETURN)) {
    std::unique_ptr<returnNode> ret_ptr = ret();
    if (!ret_ptr) {
      return nullptr;
    }
    return std::move(std::make_unique<simpleStmtNode>(std::move(ret_ptr)));
  } else if (isDeclarableType(currentTok.syntactic_category)) {
    std::unique_ptr<declareNode> decl_ptr = declare();
    if (!decl_ptr) {
      return nullptr;
    }
    return std::move(std::make_unique<simpleStmtNode>(std::move(decl_ptr)));
  } else {
    std::unique_ptr<exprNode> expr_ptr = expr();
    if (!expr_ptr) {
      return nullptr;
    }
    return std::move(std::make_unique<simpleStmtNode>(std::move(expr_ptr)));
  }
}

std::unique_ptr<declareNode> declare() {
  std::cout << "Declare\n" << std::endl;

  std::unique_ptr<typeNode> type_ptr = type();
  if (!type()) {
    return nullptr;
  }

  if (!check(Tok::IDENTIFIER)) {
    reportError("Expected IDENTIFIER, received %s", currentTok.lexeme.c_str());
    return nullptr;
  }
  std::string name_in_question = currentTok.lexeme;

  std::unique_ptr<exprNode> expr_ptr = expr();
  if (!expr_ptr) {
    return nullptr;
  }

  return std::move(std::make_unique<declareNode>(
      std::move(type_ptr), name_in_question, std::move(expr_ptr)));
}

std::unique_ptr<returnNode> ret() {
  if (!check(Tok::RETURN)) {
    reportError("Expected 'return', received '%s'", currentTok.lexeme.c_str());
    return nullptr;
  }

  std::unique_ptr<exprNode> expr_ptr = expr();

  if (!expr_ptr) {
    return nullptr;
  }

  if (!check(Tok::SEMI)) {
    reportError("Expected ';', received '%s'", currentTok.lexeme.c_str());
    return nullptr;
  }

  return std::move(std::make_unique<returnNode>(std::move(expr_ptr)));
}

std::unique_ptr<functionsNode> functions() {
  // functionsNode()
  std::unique_ptr<funcNode> func_ptr = func();
  if (!func_ptr) {
    return nullptr;
  }
  std::cout << "Parsed function" << std::endl;
  std::unique_ptr<functionsNode> myFuncs_ptr =
      std::make_unique<functionsNode>(std::move(func_ptr));
  if (!check(Tok::ENDFILE)) {
    myFuncs_ptr->setMoreFuncs(functions());
  }
  return std::move(myFuncs_ptr);
}

std::unique_ptr<programNode> program() {
  std::unique_ptr<programNode> head = std::make_unique<programNode>();
  if (!functions()) {
    return nullptr;
  }
  return std::move(head);
}

std::unique_ptr<exprNode> expr() { return nullptr; }

int parse() {
  std::unique_ptr<programNode> tree;

  if (initInp("./example_parse.txt") == 1) {
    return 1;
  }
  if (!(tree = std::move(program()))) {
    std::cout << "failed to parse program :(" << std::endl;
    return 1;
  } else {
    std::cout << "Yay we parsed!" << std::endl;
  }
  return 0;
}
int yay() { return 1; }

int main() { return parse(); }
