#include "Parse.hpp"
#include "Lex.hpp"
#include "ParseTree.hpp"

std::stack<Lexer::LexerToken> pastToks;
std::unordered_map<uint32_t, const std::string &> symbol_table;
uint32_t identifier_counter = 0;
LexedTokensSingleton &toks = LexedTokensSingleton::getInstance();
bool match(std::vector<Lexer::Tag> tokens) { return toks.match(tokens); }
bool check(Lexer::Tag tok) { return toks.check(tok); }
Lexer::LexerToken previous() { return toks.previous(); }
Lexer::LexerToken advance() { return toks.advance(); }
Lexer::LexerToken peek() { return toks.peek(); }
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

int parse() {
  if (initInstance("foo") != 0) {
    return 1;
  };

  return 0;
}
