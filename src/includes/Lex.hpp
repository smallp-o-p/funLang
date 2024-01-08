#pragma once
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <ostream>
#include <sstream>
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
}
struct TokValCat {
  std::string lexeme;
  Tok::Token syntactic_category;
};

int initInp(std::string filepath);
void closeInp();
bool nextIs(char c);
static std::string current_identifier_str;
TokValCat getNextTok();
TokValCat isString();
TokValCat isNum();
TokValCat isIdentifier();
std::vector<TokValCat> lex();
