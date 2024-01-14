#pragma once
#include <cstdint>
#include <iostream>
#include <memory>
#include <ostream>
#include <string>
#include <vector>
namespace Tok {

enum Token {
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

} // namespace Tok
struct TokValCat {
  std::string lexeme;
  Tok::Token syntactic_category;
};

// singleton
class LexedTokensSingleton {
private:
  std::unique_ptr<std::vector<TokValCat>> tokens;
  uint32_t tok_tracker;

  LexedTokensSingleton(){};

public:
  LexedTokensSingleton(LexedTokensSingleton const &) = delete;
  void operator=(LexedTokensSingleton const &) = delete;

  static LexedTokensSingleton &getInstance() {
    static LexedTokensSingleton instance;
    return instance;
  }

  void setTokens(std::unique_ptr<std::vector<TokValCat>> toks) {
    tokens = std::move(toks);
  }

  TokValCat advance() {
    if (tokens == nullptr) {
      std::cout << "Not initialized." << std::endl;
    }
    if (tok_tracker + 1 >= tokens->size()) {
      std::cout << "At last token." << std::endl;
      return tokens->at(tok_tracker);
    }
    return tokens->at(tok_tracker++);
  }

  TokValCat peek() { return tokens->at(tok_tracker); }

  bool check(Tok::Token tok) {
    return tokens->at(tok_tracker).syntactic_category == tok;
  }

  bool atEnd() { return peek().syntactic_category == Tok::ENDFILE; }

  void backup() { tok_tracker--; }

  bool match(std::vector<Tok::Token> toks) {
    for (Tok::Token tok : toks) {
      if (check(tok)) {
        advance();
        return true;
      }
    }
    return false;
  }
  void reset() {
    tok_tracker = 0;
    tokens = nullptr;
  }

  TokValCat previous() { return tokens->at(tok_tracker - 1); }
};

extern LexedTokensSingleton &toks;
std::unique_ptr<std::vector<TokValCat>> lex(const std::string &filepath,
                                            bool usingString = false);
std::unique_ptr<std::istream> initInp(const std::string &filepath,
                                      bool usingString = false);
bool nextIs(char c, const std::unique_ptr<std::istream> &inp);
static std::string current_identifier_str;
TokValCat getNextTok(const std::unique_ptr<std::istream> &inp);
TokValCat isString(const std::unique_ptr<std::istream> &inp);
TokValCat isNum(const std::unique_ptr<std::istream> &inp);
TokValCat isIdentifier(const std::unique_ptr<std::istream> &inp);
