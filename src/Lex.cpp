#include "includes/Lex.hpp"
#include <cstdint>
#include <fstream>
#include <istream>
#include <memory>
#include <sstream>
#include <sys/types.h>

uint32_t colNum = 1;
uint32_t lineNum = 1;
extern std::string fileName;

std::unique_ptr<std::istream> initInp(const std::string &filepath,
                                      bool usingString) {
  if (usingString) {
    std::cout << "Using input string as input." << std::endl;
    if (filepath.empty()) {
      std::cout << "Empty string provided when using string mode." << std::endl;
      return nullptr;
    }
    auto inp_sstream = std::make_unique<std::istringstream>(filepath);

    if (inp_sstream->fail()) {
      std::cerr << "Failed to process string :(" << std::endl;
      return nullptr;
    }
    return inp_sstream;
  } else {
    if (filepath.empty()) {
      std::cout << "Usage: funLang {FILE_PATH_DIR}" << std::endl;
      return nullptr;
    } else {
      fileName = filepath;
      auto inp_fstream =
          std::make_unique<std::ifstream>(filepath, std::ifstream::in);
      if (inp_fstream->fail()) {
        std::cerr << "Failed to open file :(" << std::endl;
        return nullptr;
      }
      return inp_fstream;
    }
  }
}

std::unique_ptr<std::vector<TokValCat>> lex(const std::string &filepath,
                                            bool usingString) {
  auto input_init = std::move(initInp(filepath, usingString));
  if (!input_init) {
    return nullptr;
  }
  std::unique_ptr<std::vector<TokValCat>> tokensPtr =
      std::make_unique<std::vector<TokValCat>>();
  TokValCat tokPair;
  bool failed = false;
  while ((tokPair = getNextTok(input_init)).syntactic_category !=
         Tok::ENDFILE) {
    if (tokPair.syntactic_category == Tok::ERR) {
      failed = true;
    }
    tokensPtr->push_back(tokPair);
  }
  if (failed) {
    return nullptr;
  }
  tokensPtr->push_back({"\0", Tok::ENDFILE});
  if (!usingString) {
    dynamic_cast<std::ifstream *>(input_init.get())->close();
  }
  return std::move(tokensPtr);
}

TokValCat getNextTok(const std::unique_ptr<std::istream> &inp) {
  using namespace Tok;
  char c = ' ';
  while (isspace(c)) {
    if (c == '\n') {
      lineNum++;
    }
    c = inp->get();
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
    if (nextIs('=', inp)) {
      return TokValCat{"==", EQCMP};
    } else {
      return TokValCat{"=", EQ};
    }
  case ':':
    return TokValCat{":", COLON};
  case ';':
    return TokValCat{";", SEMI};
  case '!':
    if (nextIs('=', inp)) {
      return TokValCat{"!=", NECMP};
    } else {
      return TokValCat{"!", BANG};
    }
  case '<':
    if (nextIs('=', inp)) {
      return TokValCat{"<=", LTECMP};
    } else {
      return TokValCat{"<", LTCMP};
    }
  case '>':
    if (nextIs('=', inp)) {
      return TokValCat{">=", GTECMP};
    } else {
      return TokValCat{">", GTCMP};
    }
  case '+':
    return nextIs('+', inp) ? TokValCat{"++", PLUSPLUS} : TokValCat{"+", PLUS};
  case '-':
    return nextIs('-', inp) ? TokValCat{"++", Tok::MINUSMINUS}
                            : TokValCat{"-", MINUS};
  case '*':
    return TokValCat{"*", MULT};
  case '/':
    return TokValCat{"/", DIV};
  case '\"':
    return isString(inp);
  }
  if (isdigit(c)) {
    inp->putback(c);
    return isNum(inp);
  }
  if (isalpha(c)) {
    inp->putback(c);
    return isIdentifier(inp);
  }
  if (inp->eof()) {
    return TokValCat{"!!EOF!!", Tok::ENDFILE};
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
bool nextIs(char c, const std::unique_ptr<std::istream> &inp) {
  if (inp->peek() == c) {
    inp->get();
    return true;
  }
  return false;
}
TokValCat isString(const std::unique_ptr<std::istream> &inp) {
  char c;
  std::string string_lit = "\"";

  while ((c = inp->get()) != '\"' && c != EOF) {
    string_lit.push_back(c);
  };

  if (inp->eof()) {
    std::cerr << "Unclosed string literal :(" << std::endl;
    return TokValCat{"UH OH", Tok::ERR};
  }
  string_lit.push_back('\"');
  return TokValCat{string_lit, Tok::STRINGLIT};
}

TokValCat isNum(const std::unique_ptr<std::istream> &inp) {
  std::string numStr = "";
  char c;
  bool seenDot = false;
  while ((isdigit(inp->peek()) || inp->peek() == '.')) {
    if (seenDot && inp->peek() == '.') {
      break;
    }
    if (inp->peek() == '.' && !seenDot) {
      seenDot = true;
    }
    c = inp->get();
    numStr.push_back(c);
  }
  if (numStr.back() == '.') { // allow numbers like 12.
    numStr.push_back('0');
  }
  if (seenDot) {
    return TokValCat{numStr, Tok::POINTNUM};
  }
  return TokValCat{numStr, Tok::NUM};
}

TokValCat isIdentifier(const std::unique_ptr<std::istream> &inp) {

  std::string id_str = "";

  char c;

  while (isalnum(inp->peek()) || inp->peek() == '_') {
    c = inp->get();
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
