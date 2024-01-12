#include "Parse.hpp"
#include "AST.hpp"
#include "Lex.hpp"
#include <cstdarg>
#include <cstdio>
#include <iostream>
#include <memory>
#include <stack>
#include <utility>

std::stack<TokValCat> pastToks;
LexedTokensSingleton &toks = LexedTokensSingleton::getInstance();

bool match(std::vector<Tok::Token> tokens) { return toks.match(tokens); }
bool check(Tok::Token tok) { return toks.check(tok); }
TokValCat previous() { return toks.previous(); }
TokValCat advance() { return toks.advance(); }
TokValCat peek() { return toks.peek(); }
void backup() { toks.backup(); }
bool atEnd(){
  return toks.atEnd();
}

void reportError(const char *format, ...) {
  va_list args;
  va_start(args, format);
  std::vprintf(format, args);
  va_end(args);
};

int initInstance(const std::string &fp) {
  toks.reset(); // reset for testing purposes
  std::unique_ptr<std::vector<TokValCat>> lexed = lex(fp);

  if (!lexed) {
    std::cout << "Failed to lex tokens." << std::endl;
    return 1;
  }

  toks.setTokens(std::move(lexed));
  std::cout << "Successfully lexed tokens." << std::endl;

  return 0;
}

int parse() {
  if (initInstance("foo") != 0) {
    return 1;
  };
  std::unique_ptr<programNode> tree;
  if (!(tree = std::move(program()))) {
    std::cout << "failed to parse program :(" << std::endl;
    return 1;
  } else {
    std::cout << "Yay we parsed!" << std::endl;
  }
  return 0;
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

std::unique_ptr<programNode> program() {
  std::unique_ptr<functionsNode> funcs_ptr = functions();

  if (!funcs_ptr) {
    return nullptr;
  }
  if (!match({Tok::ENDFILE})) {
    reportError("Expected End of File, received '%s'", peek().lexeme.c_str());
    return nullptr;
  }

  std::cout << "We parsed :D" << std::endl; 
  std::unique_ptr<programNode> head =
      std::make_unique<programNode>(std::move(funcs_ptr));
  return std::move(head);
}

std::unique_ptr<functionsNode> functions() {
  std::unique_ptr<functionsNode> myFuncs_ptr =
      std::make_unique<functionsNode>();
  while (true) {
    if (atEnd()) {
      break;
    }
    std::unique_ptr<funcNode> func_ptr = func();
    if (!func_ptr) {
      return nullptr;
    }

    std::cout << "Parsed function" << std::endl;
    myFuncs_ptr->addFunc(func());
  }
  return std::move(myFuncs_ptr);
}

std::unique_ptr<funcNode> func() {
  std::cout << "In func()" << std::endl;
  std::unique_ptr<typeNode> type_ptr = type();
  if (!type_ptr) {
    return nullptr;
  } // from here it should be an identifier
  std::unique_ptr<protoNode> proto_ptr = proto();
  if (!proto_ptr) {
    return nullptr;
  }
  std::cout << "Parsed proto" << std::endl;

  std::unique_ptr<compoundStmtNode> compoundStmt_ptr = compoundStmt();

  if (!compoundStmt_ptr) {
    return nullptr;
  }

  std::unique_ptr<funcNode> func_ptr = std::make_unique<funcNode>(
      std::move(type_ptr), std::move(proto_ptr), std::move(compoundStmt_ptr));
  return std::move(func_ptr);
}

std::unique_ptr<typeNode> type() {
  std::cout << "type()\n";
  if (!match({Tok::I32, Tok::I64, Tok::STRING, Tok::CHAR, Tok::F32, Tok::F64,
              Tok::VOID})) {
    reportError("Unexpected token '%s', expected type specifier",
                peek().lexeme.c_str());
    return nullptr;
  }
  std::unique_ptr<typeNode> typeptr =
      std::make_unique<typeNode>(previous().syntactic_category);
  return std::move(typeptr);
}

std::unique_ptr<protoNode> proto() {
  std::cout << "In proto()" << std::endl;
  if (!match({Tok::IDENTIFIER})) {
    reportError("Expected IDENTIFIER, received '%s'",
                previous().lexeme.c_str());
    return nullptr;
  }
  std::string name_in_question = previous().lexeme;

  std::cout << "Got Identifier, lexeme is: " << name_in_question << std::endl;
  if (!match({Tok::LPAREN})) { // advance it to an '(' then eat it
    reportError("Expected '(', received '%s'\n", peek().lexeme.c_str());
    return nullptr;
  }
  std::unique_ptr<argsNode> args_ptr = args();

  if (!args_ptr) {
    return nullptr;
  }

  if (!match({Tok::RPAREN})) {
    reportError("Expected ')', received '%s'\n", peek().lexeme.c_str());
    return nullptr;
  }
  std::cout << "Got ')'" << std::endl;
  return std::move(
      std::make_unique<protoNode>(name_in_question, std::move(args_ptr)));
}

std::unique_ptr<argsNode> args() {
  std::unique_ptr<argsNode> args_ptr = std::make_unique<argsNode>();
  while (true) {
    if (peek().syntactic_category == Tok::RPAREN) {
      break;
    }
    std::unique_ptr<argNode> arg_ptr = arg();
    if (!arg_ptr) {
      return nullptr;
    }
    args_ptr->addArg(std::move(arg_ptr));
    if (!match({Tok::COMMA}) && peek().syntactic_category != Tok::RPAREN) {
      reportError(
          "Assuming there are multiple argument declarations: Expected COMMA, "
          "received '%s'",
          peek().lexeme.c_str());
      return nullptr;
    }
  }
  return std::move(args_ptr);
}

std::unique_ptr<argNode> arg() { // current token
  std::cout << "In arg()" << std::endl;
  std::unique_ptr<typeNode> type_ptr = type();
  if (!type_ptr) {
    return nullptr;
  }
  if (!match({Tok::IDENTIFIER})) {
    reportError("Expected IDENTIFIER, received %s", peek().lexeme.c_str());
    return nullptr;
  }

  return std::move(
      std::make_unique<argNode>(std::move(type_ptr), previous().lexeme));
}

std::unique_ptr<compoundStmtNode> compoundStmt() {
  std::cout << "In compoundStmt()" << std::endl;
  if (!match({Tok::LCURLY})) {
    reportError("Expected '{', received '%s'", peek().lexeme.c_str());
    return nullptr;
  }
  std::unique_ptr<simpleListNode> simple_list_ptr = simpleList();

  if (!simple_list_ptr) {
    return nullptr;
  }

  if (!match({Tok::RCURLY})) {
    reportError("Expected '}', received '%s'", peek().lexeme.c_str());
    return nullptr;
  }

  return std::move(
      std::make_unique<compoundStmtNode>(std::move(simple_list_ptr)));
}

std::unique_ptr<simpleListNode> simpleList() {

  bool returnStatementFound = false;
  std::unique_ptr<simpleListNode> list_ptr = std::make_unique<simpleListNode>();
  while (true) {
    if (peek().syntactic_category == Tok::RCURLY) {
      break;
    }
    std::unique_ptr<simpleStmtNode> stmt_ptr = simpleStmt();
    if (!stmt_ptr) {
      return nullptr;
    }
    list_ptr->addStmt(std::move(stmt_ptr));
  }

  return std::move(list_ptr);
}

std::unique_ptr<simpleStmtNode> simpleStmt() {
  std::cout << "In simpleStmt()" << std::endl;
  if (match({Tok::RETURN})) {
    std::unique_ptr<returnNode> ret_ptr = ret();
    if (!ret_ptr) {
      return nullptr;
    }
    return std::move(std::make_unique<simpleStmtNode>(std::move(ret_ptr)));
  } else if (isDeclarableType(peek().syntactic_category)) {
    std::unique_ptr<declareNode> decl_ptr = declare();
    if (!decl_ptr) {
      return nullptr;
    }
    return std::move(std::make_unique<simpleStmtNode>(std::move(decl_ptr)));
  } else if (match({Tok::CALL})) {
    std::unique_ptr<fnCallNode> fn_ptr = fnCall();
    if (!fn_ptr) {
      return nullptr;
    }
    return std::move(std::make_unique<simpleStmtNode>(std::move(fn_ptr)));
  } else {
    std::unique_ptr<exprNode> expr_ptr = expr();
    if (!expr_ptr) {
      return nullptr;
    }
    return std::move(std::make_unique<simpleStmtNode>(std::move(expr_ptr)));
  }
}

std::unique_ptr<declareNode> declare() {
  std::cout << "Declare" << std::endl;

  std::unique_ptr<typeNode> type_ptr = type();
  if (!type_ptr) {
    return nullptr;
  }
  if (!match({Tok::IDENTIFIER})) {
    reportError("Expected IDENTIFIER in declaration statement, received '%s'",
                peek().lexeme.c_str());
    return nullptr;
  }
  std::string name_in_question = previous().lexeme;

  if (!match({Tok::EQ})) {
    reportError("Expected '=' after variable declaration.");
    return nullptr;
  }

  std::unique_ptr<exprNode> expr_ptr = expr();
  if (!expr_ptr) {
    return nullptr;
  }

  return std::move(std::make_unique<declareNode>(
      std::move(type_ptr), name_in_question, std::move(expr_ptr)));
}

std::unique_ptr<returnNode> ret() {
  std::cout << "In ret()" << std::endl;
  std::unique_ptr<exprNode> expr_ptr = expr();

  if (!expr_ptr) {
    return nullptr;
  }

  if (!match({Tok::SEMI})) {
    reportError("Expected ';', received '%s'", peek().lexeme.c_str());
    return nullptr;
  }

  return std::move(std::make_unique<returnNode>(std::move(expr_ptr)));
}

std::unique_ptr<exprNode> expr() {
  std::cout << "In expr()\n";
  std::unique_ptr<assignExprNode> assign_ptr = assign();
  if (!assign_ptr) {
    return nullptr;
  }
  return std::move(std::make_unique<exprNode>(std::move(assign_ptr)));
}

std::unique_ptr<assignExprNode> assign() { // some amount of lookahead needed...
  std::cout << "In assign()\n";
  std::string id_name;
  if (match({Tok::IDENTIFIER})) {
    id_name = previous().lexeme;
    if (match({Tok::EQ})) {
      std::unique_ptr<exprNode> expr_ptr = expr();
      if (!expr_ptr) {
        return nullptr;
      }
      return std::move(
          std::make_unique<assignExprNode>(id_name, std::move(expr_ptr)));
    }
    backup();
  }

  std::cout << "Not identifier\n";
  std::unique_ptr<eqExprNode> eq_ptr = eqExpr();
  if (!eq_ptr) {
    return nullptr;
  }
  return std::move(std::make_unique<assignExprNode>(std::move(eq_ptr)));
}

std::unique_ptr<eqExprNode> eqExpr() {
  std::cout << "In eqExpr()\n";
  std::unique_ptr<cmpExprNode> cmp_ptr = cmpExpr();

  if (!cmp_ptr) {
    return nullptr;
  }
  if (match({Tok::EQCMP, Tok::NECMP})) {
    return std::move(
        std::make_unique<eqExprNode>(std::move(cmp_ptr), std::move(cmpExpr()),
                                     previous().syntactic_category));
  }
  return std::move(std::make_unique<eqExprNode>(std::move(cmp_ptr)));
}

std::unique_ptr<cmpExprNode> cmpExpr() {
  std::cout << "In cmpExpr()\n";
  std::unique_ptr<addExprNode> add_ptr = addExpr();
  if (!add_ptr) {
    return nullptr;
  }
  if (match({Tok::LTCMP, Tok::LTECMP, Tok::GTCMP, Tok::GTECMP})) {
    return std::move(
        std::make_unique<cmpExprNode>(std::move(add_ptr), std::move(addExpr()),
                                      previous().syntactic_category));
  }
  return std::move(std::make_unique<cmpExprNode>(std::move(add_ptr)));
}

std::unique_ptr<addExprNode> addExpr() {
  std::cout << "In addExpr()\n";
  std::unique_ptr<multdivNode> multdiv_ptr = multdiv();

  if (!multdiv_ptr) {
    return nullptr;
  }

  if (match({Tok::PLUS, Tok::MINUS})) {
    return std::move(std::make_unique<addExprNode>(
        std::move(multdiv_ptr), previous().syntactic_category,
        std::move(multdiv())));
  }

  return std::move(std::make_unique<addExprNode>(std::move(multdiv_ptr)));
}

std::unique_ptr<multdivNode> multdiv() {
  std::cout << "multdiv()\n";
  std::unique_ptr<unaryNode> unary_ptr = unary();

  if (!unary_ptr) {
    return nullptr;
  }

  if (match({Tok::MULT, Tok::DIV})) {
    return std::move(std::make_unique<multdivNode>(
        std::move(unary_ptr), previous().syntactic_category,
        std::move(unary())));
  }
  return std::make_unique<multdivNode>(std::move(unary_ptr));
}

std::unique_ptr<unaryNode> unary() {
  std::cout << "unary()\n";
  if (match({Tok::PLUSPLUS, Tok::MINUSMINUS, Tok::BANG})) {
    return std::make_unique<unaryNode>(previous().syntactic_category,
                                       std::move(primary()));
  }
  return std::make_unique<unaryNode>(std::move(primary()));
}

std::unique_ptr<primaryNode> primary() {
  std::cout << "primary()\n";
  if (match({Tok::LPAREN})) {
    std::unique_ptr<exprNode> expr_ptr = expr();
    if (!expr_ptr) {
      reportError("Could not parse expression as primary.");
      return nullptr;
    }
    if (!match({Tok::RPAREN})) {
      reportError("Expected ')', received '%s", peek().lexeme.c_str());
      return nullptr;
    }
    return std::move(
        std::make_unique<primaryNode>(std::move(expr_ptr))); // expr
  } else if (match({Tok::IDENTIFIER})) {
    if(peek().syntactic_category == Tok::RPAREN){
      reportError("Missing 'call' keyword before function call.");
      return nullptr;  
    }

    return std::move(
        std::make_unique<primaryNode>(previous().lexeme)); // variable name
  } else if (match({Tok::CALL})) {                         // fnCall
    std::unique_ptr<fnCallNode> fncall_ptr = fnCall();
    if (!fncall_ptr) {
      return nullptr;
    }
    return std::move(std::make_unique<primaryNode>(std::move(fncall_ptr)));
  } else if (match({Tok::NUM, Tok::POINTNUM, Tok::STRINGLIT, Tok::TRUE,
                    Tok::FALSE})) {
    return std::move(std::make_unique<primaryNode>(
        previous().lexeme,
        previous().syntactic_category)); // literal value
  } else {
    reportError("Unexpected token '%s' in primary.", peek().lexeme.c_str());
    return nullptr;
  }
}

std::unique_ptr<fnCallNode> fnCall() {
  if (!match({Tok::IDENTIFIER})) {
    reportError("Expected IDENTIFIER in fnCall(), received '%s'",
                peek().lexeme.c_str());
    return nullptr;
  }
  std::string name_in_question = previous().lexeme;

  if (!match({Tok::LPAREN})) {
    reportError("Expected '(' after identifier in function call, received '%s'",
                peek().lexeme.c_str());
    return nullptr;
  }
  std::unique_ptr<callArgsNode> callargs_ptr = callArgs();

  if (!callargs_ptr) {
    return nullptr;
  }
  if (!match({Tok::RPAREN})) {
    reportError("Expected ')' after function call arguments, received '%s'",
                peek().lexeme.c_str());
  }

  return std::move(
      std::make_unique<fnCallNode>(name_in_question, std::move(callargs_ptr)));
}

std::unique_ptr<callArgsNode> callArgs() {
  std::vector<std::unique_ptr<primaryNode>> primaries;
  while (true) {
    if (peek().syntactic_category == Tok::RPAREN) {
      break;
    }
    std::unique_ptr<primaryNode> primary_ptr = primary();
    if (!primary_ptr) {
      return nullptr;
    }
    primaries.push_back(std::move(primary_ptr));
    if (!match({Tok::COMMA}) && !(peek().syntactic_category == Tok::RPAREN)) {
      reportError("Assuming there are multiple arguments to this call, "
                  "expected ',', received '%s'",
                  peek().lexeme.c_str());
      return nullptr;
    }
  }
  return std::make_unique<callArgsNode>(std::move(primaries));
}

int yay() { return 1; }
