#include "includes/Lex.hpp"
#include <cstdint>
#include <sys/types.h>

uint32_t colNum = 1;
uint32_t lineNum = 1;
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

std::vector<TokValCat> lex() {
  std::vector<TokValCat> tokens;
  TokValCat tokPair;
  bool failed = false;
  while ((tokPair = getNextTok()).syntactic_category != Tok::ENDFILE) {
    if (tokPair.syntactic_category == Tok::ERR) {
      failed = true;
    }
    tokens.push_back(tokPair);
  }
  if (failed) {
    return {};
  }
  inp.close();
  return tokens;
}

void closeInp() { inp.close(); }

TokValCat getNextTok() {
  using namespace Tok;
  char c = ' ';
  while (isspace(c)) {
    if (c == '\n') {
      lineNum++;
    } else {
    }
    c = inp.get();
  }
  colNum++;
  switch (c) {
  case ',':
    return TokValCat{",", Tok::COMMA};
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
  if (isalpha(c)) {
    inp.putback(c);
    return isIdentifier();
  }
  if (inp.eof()) {
    return TokValCat{"\0", Tok::ENDFILE};
  } else {
    std::cout << "Fatal: Unexpected character '" << c << "' on line " << lineNum
              << std::endl;
    return TokValCat{"UH OH", Tok::ERR};
  }
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

  while ((isdigit(inp.peek()) || inp.peek() == '.')) {
    c = inp.get();
    numStr.push_back(c);
  }
  if (numStr.back() == '.') { // allow numbers like 12.
    numStr.push_back('0');
  }
  if (numStr.find('.') != std::string::npos) {
    return TokValCat{numStr, Tok::POINTNUM};
  }
  return TokValCat{numStr, Tok::NUM};
}

TokValCat isIdentifier() {

  std::string id_str = "";

  char c;

  while (isalnum(inp.peek())) {
    c = inp.get();
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
  } else if (id_str == "call") {
    return TokValCat{id_str, Tok::CALL};
  }
  return TokValCat{id_str, Tok::IDENTIFIER};
}
