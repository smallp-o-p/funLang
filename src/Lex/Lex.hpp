#pragma once
#include <cstdint>
#include <iostream>
#include <memory>
#include <ostream>
#include <string>
#include <vector>
class LexedTokensSingleton;
namespace Lexer {
enum Tag {
  LPAREN = -1000,
  RPAREN,
  LCURLY,
  RCURLY,
  EQ,
  COMMA,
  SEMI,
  COLON,
  VOID,
  BOOL,
  CALL,
  I32,
  I64,
  F32,
  F64,
  STRING,
  STRINGLIT,
  CHAR,
  TRUE,
  FALSE,
  RETURN,
  EQCMP,
  NECMP,
  LTECMP,
  GTECMP,
  LTCMP,
  GTCMP,
  PLUS,
  PLUSPLUS,
  MINUS,
  MINUSMINUS,
  MULT,
  DIV,
  BANG,
  NUM,
  POINTNUM,
  IDENTIFIER,
  ERR = 1000,
  ENDFILE = 1001,
};
struct Token {
  std::string lexeme;
  Tag syntactic_category;
};

extern LexedTokensSingleton &toks;
std::unique_ptr<std::vector<Token>> lex(const std::string &filepath,
                                        bool usingString = false);
std::unique_ptr<std::istream> initInp(const std::string &filepath,
                                      bool usingString = false);
bool nextIs(char c, const std::unique_ptr<std::istream> &inp);
static std::string current_identifier_str;
Token getNextTok(const std::unique_ptr<std::istream> &inp);
Token isString(const std::unique_ptr<std::istream> &inp);
Token isNum(const std::unique_ptr<std::istream> &inp);
Token isIdentifier(const std::unique_ptr<std::istream> &inp);
} // namespace Lexer
// singleton
class LexedTokensSingleton {
private:
  std::unique_ptr<std::vector<Lexer::Token>> tokens;
  uint32_t tok_tracker;

  LexedTokensSingleton(){};

public:
  LexedTokensSingleton(LexedTokensSingleton const &) = delete;
  void operator=(LexedTokensSingleton const &) = delete;

  static LexedTokensSingleton &getInstance() {
    static LexedTokensSingleton instance;
    return instance;
  }

  void setTokens(std::unique_ptr<std::vector<Lexer::Token>> toks) {
    tokens = std::move(toks);
  }

  Lexer::Token advance() {
    if (tokens == nullptr) {
      std::cout << "Not initialized." << std::endl;
    }
    if (tok_tracker + 1 >= tokens->size()) {
      std::cout << "At last token." << std::endl;
      return tokens->at(tok_tracker);
    }
    return tokens->at(tok_tracker++);
  }

  Lexer::Token peek() { return tokens->at(tok_tracker); }
  Lexer::Token previous() { return tokens->at(tok_tracker - 1); }
  bool check(Lexer::Tag tok) {
    return tokens->at(tok_tracker).syntactic_category == tok;
  }

  bool atEnd() { return peek().syntactic_category == Lexer::Tag::ENDFILE; }

  void backup() { tok_tracker--; }

  bool match(std::vector<Lexer::Tag> toks) {
    for (Lexer::Tag tok : toks) {
      if (check(tok)) {
        advance();
        return true;
      }
    }
    return false;
  }
  Lexer::Token lookahead(int howMuch) {
    return tokens->at(tok_tracker + howMuch);
  }
  void reset() {
    tok_tracker = 0;
    tokens = nullptr;
  }
};
