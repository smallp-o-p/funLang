#include "includes/Lex.hpp"
#include <cctype>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <ostream>
#include <sstream>

static int lineCount;

static std::ifstream inp;

static std::istringstream inp_str;

int initInp(std::string filepath) {
  if (filepath.empty()) {
    std::cout << "Usage: funLang {FILE_PATH_DIR}" << std::endl;
    return 1;
  } else {
    inp.open(filepath, std::ifstream::in);
    if (inp.fail()) {
      std::cerr << "Failed to open file :(" << std::endl;
      return 1;
    }
    return 0;
  }
}

void closeInp() { inp.close(); }

TokValCat getNextTok() {
  using namespace Tok;

  char c = ' ';
  while (isspace(c)) {
    c = inp.get();
  }
  switch (c) {

  case '{':
    return TokValCat{"{", LCURLY};
  case '}':
    return TokValCat{"}", RCURLY};
  case '(':
    return TokValCat{"(", LPAREN};
  case ')':
    return TokValCat{")", RPAREN};
  case '=':
    if (nextIs('=')) {
      return TokValCat{"==", EQCMP};
    } else {
      return TokValCat{"=", EQ};
    }
  case ':':
    return TokValCat{":", COLON};
  case ';':
    return TokValCat{";", SEMI};
  case '!':
    if (nextIs('=')) {
      return TokValCat{"!=", NECMP};
    } else {
      return TokValCat{"!", BANG};
    }
  case '<':
    if (nextIs('=')) {
      return TokValCat{"<=", LTECMP};
    } else {
      return TokValCat{"<", LTCMP};
    }
  case '>':
    if (nextIs('=')) {
      return TokValCat{">=", GTECMP};
    } else {
      return TokValCat{">", GTCMP};
    }
  case '+':
    return nextIs('+') ? TokValCat{"++", PLUSPLUS} : TokValCat{"+", PLUS};
  case '-':
    return nextIs('-') ? TokValCat{"++", Tok::MINUSMINUS}
                       : TokValCat{"-", MINUS};
  case '*':
    return TokValCat{"*", MULT};
  case '/':
    return TokValCat{"/", DIV};
  case '\"':
    return isString();
  }
  if (isdigit(c)) {
    inp.putback(c);
    return isNum();
  }
  if (inp.eof()) {
    return TokValCat{"\0", Tok::ENDFILE};
  }
  inp.putback(c);
  return isIdentifier();
}
/*
 * nextIs(char c) checks if the next character in the input buffer is c, if
 * so it eats the next character and returns true. Does nothing and returns
 * false otherwise.
 * */
bool nextIs(char c) {
  if (inp.peek() == c) {
    inp.get();
    return true;
  }
  return false;
}
TokValCat isString() {
  char c;
  std::string string_lit = "\"";

  while ((c = inp.get()) != '\"' && c != EOF) {
    string_lit.push_back(c);
  };

  if (inp.eof()) {
    fprintf(stderr, "Unclosed string literal :(\n");
    return TokValCat{"UH OH", Tok::ERR};
  }
  string_lit.push_back('\"');
  return TokValCat{string_lit, Tok::STRINGLIT};
}

TokValCat isNum() {
  std::string numStr = "";
  char c;
  do {
    c = inp.get();
    numStr.push_back(c);
  } while ((isdigit(c) || c == '.'));

  if (numStr.back() == '.') { // allow numbers like 12.
    numStr.push_back('0');
  }
  if (!isspace(c) &&
      !inp.eof()) { // check if we stopped at anything that isn't whitespace
    fprintf(stderr, "Unexpected character '%c' in number literal\n.", c);
    return TokValCat{"UH OH,", Tok::ERR};
  }
  if (numStr.find('.') != std::string::npos) {
    return TokValCat{numStr, Tok::POINTNUM};
  }
  return TokValCat{numStr, Tok::NUM};
}

TokValCat isIdentifier() {

  std::string id_str = "";

  char c;

  while (isalnum(c = inp.get())) {
    id_str.push_back(c);
  }

  if (id_str.compare("void") == 0) {
    return TokValCat{id_str, Tok::VOID};
  } else if (id_str.compare("bool") == 0) {
    return TokValCat{id_str, Tok::BOOL};
  } else if (id_str.compare("char") == 0) {
    return TokValCat{id_str, Tok::CHAR};
  } else if (id_str.compare("string") == 0) {
    return TokValCat{id_str, Tok::STRING};
  } else if (id_str == "i32") {
    return TokValCat{id_str, Tok::I32};
  } else if (id_str == "i64") {
    return TokValCat{id_str, Tok::I64};
  } else if (id_str == "f32") {
    return TokValCat{id_str, Tok::F32};
  } else if (id_str == "f64") {
    return TokValCat{id_str, Tok::F64};
  } else if (id_str == "return") {
    return TokValCat{id_str, Tok::RETURN};
  } else if (id_str == "true") {
    return TokValCat{id_str, Tok::TRUE};
  } else if (id_str == "false") {
    return TokValCat{id_str, Tok::FALSE};
  }
  return TokValCat{id_str, Tok::IDENTIFIER};
}
