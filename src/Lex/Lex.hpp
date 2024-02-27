#pragma once
#include "TokenTags.hpp"
#include <cstdint>
#include <fstream>
#include <initializer_list>
#include <iostream>
#include <istream>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <sys/types.h>
#include <vector>

class Lexer {
public:
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
    BANG,
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

private:
  std::unique_ptr<std::vector<Lexer::Token>> tokens;
  uint32_t tok_tracker;
  std::unique_ptr<std::istream> in_stream;

  uint32_t colNum = 1;
  uint32_t lineNum = 1;
  Lexer::Token lexString();
  Lexer::Token lexNum();
  Lexer::Token lexIdentifier();
  bool nextIs(char c);

public:
  Lexer(std::unique_ptr<std::istream> in) : in_stream(std::move(in)) {}
  void setTokens(std::unique_ptr<std::vector<Lexer::Token>> toks) {
    tokens = std::move(toks);
  }

  static std::unique_ptr<Lexer> init(const std::string &filename,
                                     bool usingString = false);
  Lexer::Token getNext();
  Lexer::Token peek();
  Lexer::Token previous();
  bool match(std::initializer_list<Lexer::Tag> toks);
  Lexer::Token lookahead(uint32_t howMuch);
  void backtrack(uint32_t howMuch);
  void atEnd();
};
std::unique_ptr<Lexer> lex(const std::string &filepath,
                           bool usingString = false);
std::unique_ptr<std::istream> initInp(const std::string &filepath,
                                      bool usingString = false);
bool nextIs(char c, const std::unique_ptr<std::istream> &inp);
Lexer::Token getNextTok(const std::unique_ptr<std::istream> &inp);
Lexer::Token isString(const std::unique_ptr<std::istream> &inp);
Lexer::Token isNum(const std::unique_ptr<std::istream> &inp);
Lexer::Token isIdentifier(const std::unique_ptr<std::istream> &inp);
// singleton
