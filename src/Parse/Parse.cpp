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
  switch (peek().syntactic_category) {
  case Tag::VOID:
  case Tag::BOOL:
  case Tag::I32:
  case Tag::I64:
  case Tag::F32:
  case Tag::F64:
  case Tag::STRING:
    return decl();
  case Tag::IDENTIFIER: {
    if (lookahead(1).syntactic_category == Tag::EQ) {
      return decl();
    } else {
      return expr();
    }
  }
  case Tag::RETURN: {
    return returnStmt();
  }
  default:
    reportError("Current token '%s' is not viable to expand in Stmt\n",
                peek().lexeme.c_str());
    return nullptr;
  }
}
std::unique_ptr<Stmt> decl() {
  std::unique_ptr<TypeNode> t_node = type();
  if (!t_node) {
    return nullptr;
  }
  if (!match({Lexer::Tag::IDENTIFIER})) {
    reportError("Expected Identifier, received '%s'\n", peek().lexeme.c_str());
    return nullptr;
  }
  const std::string &id = previous().lexeme;

  if (!match({Lexer::Tag::EQ})) {
    reportError("Expected '=', received '%s'\n", peek().lexeme.c_str());
    return nullptr;
  }
  std::unique_ptr<Stmt> expr_node = expr();
  if (!expr_node) {
    return nullptr;
  }
}

int parse() {
  if (initInstance("foo") != 0) {
    return 1;
  };

  return 0;
}
