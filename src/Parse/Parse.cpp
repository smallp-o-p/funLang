#include "Parse.hpp"
#include "Lex.hpp"
#include "ParseTree.hpp"
#include <memory>

std::stack<Lexer::LexerToken> pastToks;
LexedTokensSingleton &toks = LexedTokensSingleton::getInstance();
bool match(std::vector<Lexer::Tag> tokens) { return toks.match(tokens); }
bool check(Lexer::Tag tok) { return toks.check(tok); }
Lexer::LexerToken previous() { return toks.previous(); }
Lexer::LexerToken advance() { return toks.advance(); }
Lexer::LexerToken peek() { return toks.peek(); }
Lexer::LexerToken lookahead(int howMuch) { return toks.lookahead(howMuch); }
void backup() { toks.backup(); }
bool atEnd() { return toks.atEnd(); }

void reportError(const char *format, ...) {
  va_list args;
  va_start(args, format);
  std::vprintf(format, args);
  va_end(args);
};

int initInstance(const std::string &fp, bool usingString) {
  toks.reset(); // reset for testing purposes
  std::unique_ptr<std::vector<Lexer::LexerToken>> lexed =
      Lexer::lex(fp, usingString);

  if (!lexed) {
    std::cout << "Failed to lex tokens." << std::endl;
    return 1;
  }

  toks.setTokens(std::move(lexed));
  std::cout << "Successfully lexed tokens." << std::endl;

  return 0;
}

std::unique_ptr<ProgramNode> program() {
  // TODO: globals
  std::unique_ptr<FunctionsNode> fns = std::move(functions());

  if (!match({Lexer::Tag::ENDFILE})) {
    reportError("Expected End of File.");
    return nullptr;
  }
  return std::make_unique<ProgramNode>(std::move(fns));
}

std::unique_ptr<FunctionsNode> functions() {
  std::vector<std::unique_ptr<FunctionNode>> funcList;
  while (true) {
    std::unique_ptr<FunctionNode> fn = function();
    if (!fn) {
      reportError("Failed at functions()");
      return nullptr;
    }
    funcList.push_back(std::move(fn));
    if (check(Lexer::Tag::ENDFILE)) {
      break;
    }
  }
  return std::make_unique<FunctionsNode>(funcList);
}
std::unique_ptr<FunctionNode> function() {
  std::unique_ptr<PrototypeNode> prototype = proto();
  if (!prototype) {
    reportError("Failed at function()");
    return nullptr;
  }

  std::unique_ptr<CompoundStmt> compound = compoundStmt();
  if (!compound) {
    reportError("Failed at compoundStmt()");
    return nullptr;
  }

  if (!match({Lexer::Tag::RCURLY})) {
    reportError("Expected '}', received %s", peek().lexeme.c_str());
    return nullptr;
  }
  return std::make_unique<FunctionNode>(std::move(prototype),
                                        std::move(compound));
}

std::unique_ptr<PrototypeNode> proto() {
  std::unique_ptr<TypeNode> type_node = type();

  if (!type_node) {
    return nullptr;
  }
  if (!match({Lexer::Tag::IDENTIFIER})) {
    reportError("Expected IDENTIFIER, received '%s'\n", peek().lexeme.c_str());
    return nullptr;
  }
  std::string id = previous().lexeme;

  if (!match({Lexer::Tag::LPAREN})) {
    reportError("Expected '(', received '%s'\n", peek().lexeme.c_str());
    return nullptr;
  }
  std::unique_ptr<ArgumentsNode> args = arguments();

  if (!args) {
    return nullptr;
  }

  return std::make_unique<PrototypeNode>(std::move(type_node), id,
                                         std::move(args));
}

std::unique_ptr<ArgumentsNode> arguments() {
  std::vector<std::unique_ptr<ArgNode>> argList;

  while (true) {
    std::unique_ptr<ArgNode> argument = arg();
    if (!argument) {
      return nullptr;
    }
    argList.push_back(argument);
    if (peek().syntactic_category == Lexer::Tag::RPAREN) {
      break;
    }
  }
  return std::make_unique<ArgumentsNode>(argList);
}

std::unique_ptr<ArgNode> arg() {
  std::unique_ptr<TypeNode> t_node = type();
  if (!t_node) {
    reportError("Expected type in prototype argument declaration\n");
    return nullptr;
  }

  if (!match({Lexer::Tag::IDENTIFIER})) {
    reportError("Expected Identifier, received '%s'", peek().lexeme.c_str());
    return nullptr;
  }
  const std::string &id = previous().lexeme;

  return std::make_unique<ArgNode>(std::move(t_node), id);
}

std::unique_ptr<CompoundStmt> compoundStmt() {
  std::vector<std::unique_ptr<Stmt>> stmts;

  while (true) {
    std::unique_ptr<Stmt> s = simpleStmt();
    if (!s) {
      return nullptr;
    }
    stmts.push_back(s);

    if (peek().syntactic_category == Lexer::Tag::RCURLY) {
      break;
    }
  }
  return std::make_unique<CompoundStmt>(stmts);
}

std::unique_ptr<Stmt> simpleStmt() {
  using namespace Lexer;
  std::unique_ptr<Stmt> stmt_inq;
  switch (peek().syntactic_category) {
  case Tag::VOID:
  case Tag::BOOL:
  case Tag::I32:
  case Tag::I64:
  case Tag::F32:
  case Tag::F64:
  case Tag::STRING:
    stmt_inq = decl();
    break;
  case Tag::IDENTIFIER: {
    if (lookahead(1).syntactic_category ==
        Tag::EQ) { // unsure how we'll handle struct members but we'll find out
      stmt_inq = decl();
    } else {
      stmt_inq = expr();
    }
    break;
  }
  case Tag::RETURN: {
    stmt_inq = returnStmt();
    break;
  }
  default:
    reportError("Current token '%s' is not viable to expand in Stmt\n",
                peek().lexeme.c_str());
    return nullptr;
  }
  if (!stmt_inq) {
    return nullptr;
  }
  if (!match({Lexer::Tag::SEMI})) {
    reportError("Expected ';' after statement, received '%s'",
                peek().lexeme.c_str());
    return nullptr;
  }
  return stmt_inq;
}
std::unique_ptr<VarDecl> decl() {
  std::unique_ptr<TypeNode> type_node = type();
  if (!type_node) {
    return nullptr;
  }
  if (!match({Lexer::Tag::IDENTIFIER})) {
    reportError("Expected Identifier, received '%s'\n", peek().lexeme.c_str());
    return nullptr;
  }
  std::string id = previous().lexeme;

  if (!match({Lexer::Tag::EQ})) {
    reportError("Expected '=', received '%s'\n", peek().lexeme.c_str());
    return nullptr;
  }
  std::unique_ptr<Expr> expr_node = expr();
  if (!expr_node) {
    return nullptr;
  }
  return std::make_unique<VarDecl>(std::move(type_node), id,
                                   std::move(expr_node));
}

std::unique_ptr<returnNode> returnStmt() {
  std::unique_ptr<Expr> expr_node = expr();
  return std::make_unique<returnNode>(std::move(expr_node));
}

std::unique_ptr<Expr> expr() { return assign(); }

std::unique_ptr<Expr> assign() {
  if (match({Lexer::Tag::IDENTIFIER})) {
    std::unique_ptr<leafNode> leaf = std::make_unique<leafNode>(previous());
    if (!match({Lexer::Tag::EQ})) {
      reportError(
          "Expected '=' in (assumed) variable assignment, received '%s'",
          peek().lexeme.c_str());
      return nullptr;
    }
    std::unique_ptr<Expr> expr_node = expr();
    if (!expr_node) {
      return nullptr;
    }
    return std::make_unique<BinaryOp>(std::move(leaf), std::move(expr_node),
                                      Parse::BinaryOperators::ASSIGN);
  } else {
    return eqExpr();
  }
}

std::unique_ptr<Expr> eqExpr() {
  std::unique_ptr<Expr> cmp_node = cmpExpr();
  if (!cmp_node) {
    return nullptr;
  }
  if (match({Lexer::Tag::EQCMP, Lexer::Tag::NECMP})) {
    Parse::BinaryOperators opcode =
        previous().syntactic_category == Lexer::Tag::EQCMP
            ? Parse::BinaryOperators::EQEQ
            : Parse::BinaryOperators::NE;
    std::unique_ptr<Expr> cmp_node2 = cmpExpr();

    if (!cmp_node2) {
      return nullptr;
    }
    return std::make_unique<BinaryOp>(std::move(cmp_node), std::move(cmp_node2),
                                      opcode);
  }
  return cmp_node;
}

std::unique_ptr<Expr> cmpExpr() {
  std::unique_ptr<Expr> add_node = addExpr();
  if (!add_node) {
    return nullptr;
  }
  using namespace Lexer;
  if (match({Tag::LTCMP, Tag::LTECMP, Tag::GTCMP, Tag::GTECMP})) {
    using namespace Parse;
    BinaryOperators opcode;
    switch (previous().syntactic_category) {
    case Tag::LTCMP:
      opcode = BinaryOperators::LT;
      break;
    case Tag::LTECMP:
      opcode = BinaryOperators::LTE;
      break;
    case Tag::GTCMP:
      opcode = BinaryOperators::GT;
      break;
    case Tag::GTECMP:
      opcode = BinaryOperators::GTE;
      break;
    default:
      opcode = BinaryOperators::UNDEFINED;
      break;
    }
    std::unique_ptr<Expr> add_node2 = addExpr();
    if (!add_node2) {
      return nullptr;
    }
    return std::make_unique<BinaryOp>(std::move(add_node), std::move(add_node2),
                                      opcode);
  }
  return add_node;
}

std::unique_ptr<Expr> addExpr() {
  std::unique_ptr<Expr> multdiv_node = multdiv();
  if (!multdiv_node) {
    return nullptr;
  }
  if (match({Lexer::Tag::PLUS, Lexer::Tag::MINUS})) {
    Parse::BinaryOperators opcode =
        previous().syntactic_category == Lexer::Tag::PLUS
            ? Parse::BinaryOperators::ADD
            : Parse::BinaryOperators::SUBTRACT;
    std::unique_ptr<Expr> multdiv_node2 = multdiv();
    if (!multdiv_node2) {
      return nullptr;
    }
    return std::make_unique<BinaryOp>(std::move(multdiv_node),
                                      std::move(multdiv_node2), opcode);
  }
  return multdiv_node;
}

std::unique_ptr<Expr> multdiv() {
  std::unique_ptr<Expr> unary_node = unary();

  if (!unary_node) {
    return nullptr;
  }
  if (match({Lexer::Tag::MULT, Lexer::Tag::DIV})) {
    Parse::BinaryOperators opcode =
        previous().syntactic_category == Lexer::Tag::MULT
            ? Parse::BinaryOperators::MULT
            : Parse::BinaryOperators::DIV;
    std::unique_ptr<Expr> unary_node2 = unary();
    if (!unary_node2) {
      return nullptr;
    }
    return std::make_unique<BinaryOp>(std::move(unary_node),
                                      std::move(unary_node2), opcode);
  }
  return unary_node;
}

std::unique_ptr<Expr> unary() {
  using namespace Lexer;
  Parse::UnaryOperators opcode = Parse::UnaryOperators::NOP;
  if (match({Lexer::Tag::PLUSPLUS, Lexer::Tag::MINUSMINUS, Lexer::Tag::BANG,
             Lexer::Tag::MINUS})) {
    switch (previous().syntactic_category) {
    case Tag::PLUSPLUS:
      opcode = Parse::UnaryOperators::PREINCREMENT;
      break;
    case Tag::BANG:
      opcode = Parse::UnaryOperators::BANG;
      break;
    case Tag::MINUSMINUS:
      opcode = Parse::UnaryOperators::PREDECREMENT;
      break;
    case Tag::MINUS:
      opcode = Parse::UnaryOperators::NEGATE;
      break;
    default:;
    }
  }
  std::unique_ptr<Expr> primary_node = primary();
  if (!primary_node) {
    return nullptr;
  }
  if (opcode != Parse::UnaryOperators::NOP) {
    return std::make_unique<UnaryOp>(std::move(primary_node), opcode);
  } else {
    return primary_node;
  }
}

std::unique_ptr<Expr> primary() {
  if (match({Lexer::Tag::LPAREN})) {
    backup();
    std::unique_ptr<Expr> expr_node = expr();
    if (!match({Lexer::Tag::RPAREN})) {
      reportError("Expected ')', received '%s'\n", peek().lexeme.c_str());
      return nullptr;
    }
    return expr_node;
  } else if (match({Lexer::Tag::IDENTIFIER})) {
    if (peek().syntactic_category == Lexer::Tag::LPAREN) {
      std::unique_ptr<Expr> fncall_node = fnCall();
      if (!fncall_node) {
        return nullptr;
      }
    } else { /* identifier */
      std::unique_ptr<leafNode> leaf = std::make_unique<leafNode>(previous());
      return leaf;
    }
  } else if (match({Lexer::Tag::POINTNUM, Lexer::Tag::NUM,
                    Lexer::Tag::STRINGLIT, Lexer::Tag::TRUE,
                    Lexer::Tag::FALSE})) {
    return std::make_unique<leafNode>(previous());
  }
  reportError("Unexpected token '%s' in primary.\n", peek().lexeme.c_str());
  return nullptr;
}

std::unique_ptr<Expr> fnCall() {
  if (!match({Lexer::Tag::IDENTIFIER})) {
    return nullptr;
  }
  std::string id = previous().lexeme;
  if (!match({Lexer::Tag::LPAREN})) {
    reportError("Expected '(', received '%s' in function call declaration \n",
                peek().lexeme.c_str());
    return nullptr;
  }
  std::unique_ptr<callArgList> callargs_node = callArgs();

  if (!callargs_node) {
    return nullptr;
  }

  if (!match({Lexer::Tag::RPAREN})) {
    reportError("Expected ')', received '%s' in argumment list declaration. \n",
                peek().lexeme.c_str());
    return nullptr;
  }
  return std::make_unique<fnCallNode>(id, std::move(callargs_node));
}

int parse() {
  if (initInstance("foo") != 0) {
    return 1;
  };

  return 0;
}
