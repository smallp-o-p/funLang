#include "Lex.hpp"
#include <cstdint>
#include <fstream>
#include <istream>
#include <memory>
#include <sstream>
#include <sys/types.h>

uint32_t colNum = 1;
uint32_t lineNum = 1;
std::string fileName;
namespace Lexer {
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

std::unique_ptr<std::vector<LexerToken>> lex(const std::string &filepath,
                                             bool usingString) {
  auto input_init = std::move(initInp(filepath, usingString));
  if (!input_init) {
    return nullptr;
  }
  std::unique_ptr<std::vector<LexerToken>> tokensPtr =
      std::make_unique<std::vector<LexerToken>>();
  LexerToken tokPair;
  bool failed = false;
  while ((tokPair = getNextTok(input_init)).syntactic_category !=
         Tag::ENDFILE) {
    if (tokPair.syntactic_category == Tag::ERR) {
      failed = true;
    }
    tokensPtr->push_back(tokPair);
  }
  if (failed) {
    return nullptr;
  }
  tokensPtr->push_back({"\0", Tag::ENDFILE});
  if (!usingString) {
    dynamic_cast<std::ifstream *>(input_init.get())->close();
  }
  return std::move(tokensPtr);
}

LexerToken getNextTok(const std::unique_ptr<std::istream> &inp) {
  using namespace Lexer;
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
    return LexerToken{",", Tag::COMMA};
  case '{':
    return LexerToken{"{", Tag::LCURLY};
  case '}':
    return LexerToken{"}", Tag::RCURLY};
  case '(':
    return LexerToken{"(", Tag::LPAREN};
  case ')':
    return LexerToken{")", Tag::RPAREN};
  case '=':
    if (nextIs('=', inp)) {
      return LexerToken{"==", Tag::EQCMP};
    } else {
      return LexerToken{"=", Tag::EQ};
    }
  case ':':
    return LexerToken{":", Tag::COLON};
  case ';':
    return LexerToken{";", Tag::SEMI};
  case '!':
    if (nextIs('=', inp)) {
      return LexerToken{"!=", Tag::NECMP};
    } else {
      return LexerToken{"!", Tag::BANG};
    }
  case '<':
    if (nextIs('=', inp)) {
      return LexerToken{"<=", LTECMP};
    } else {
      return LexerToken{"<", LTCMP};
    }
  case '>':
    if (nextIs('=', inp)) {
      return LexerToken{">=", GTECMP};
    } else {
      return LexerToken{">", GTCMP};
    }
  case '+':
    return nextIs('+', inp) ? LexerToken{"++", PLUSPLUS}
                            : LexerToken{"+", PLUS};
  case '-':
    return nextIs('-', inp) ? LexerToken{"++", Tag::MINUSMINUS}
                            : LexerToken{"-", MINUS};
  case '*':
    return LexerToken{"*", MULT};
  case '/':
    return LexerToken{"/", DIV};
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
    return LexerToken{"!!EOF!!", Tag::ENDFILE};
  } else {
    std::cout << "Fatal: Unexpected character '" << c << "' on line " << lineNum
              << std::endl;
    return LexerToken{"UH OH", Tag::ERR};
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
LexerToken isString(const std::unique_ptr<std::istream> &inp) {
  char c;
  std::string string_lit = "\"";

  while ((c = inp->get()) != '\"' && c != EOF) {
    string_lit.push_back(c);
  };

  if (inp->eof()) {
    std::cerr << "Unclosed string literal :(" << std::endl;
    return LexerToken{"UH OH", Tag::ERR};
  }
  string_lit.push_back('\"');
  return LexerToken{string_lit, Tag::STRINGLIT};
}

LexerToken isNum(const std::unique_ptr<std::istream> &inp) {
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
    return LexerToken{numStr, Tag::POINTNUM};
  }
  return LexerToken{numStr, Tag::NUM};
}

LexerToken isIdentifier(const std::unique_ptr<std::istream> &inp) {

  std::string id_str = "";

  char c;

  while (isalnum(inp->peek()) || inp->peek() == '_') {
    c = inp->get();
    id_str.push_back(c);
  }
  if (id_str.compare("void") == 0) {
    return LexerToken{id_str, Tag::VOID};
  } else if (id_str.compare("bool") == 0) {
    return LexerToken{id_str, Tag::BOOL};
  } else if (id_str.compare("char") == 0) {
    return LexerToken{id_str, Tag::CHAR};
  } else if (id_str.compare("string") == 0) {
    return LexerToken{id_str, Tag::STRING};
  } else if (id_str == "i32") {
    return LexerToken{id_str, Tag::I32};
  } else if (id_str == "i64") {
    return LexerToken{id_str, Tag::I64};
  } else if (id_str == "f32") {
    return LexerToken{id_str, Tag::F32};
  } else if (id_str == "f64") {
    return LexerToken{id_str, Tag::F64};
  } else if (id_str == "return") {
    return LexerToken{id_str, Tag::RETURN};
  } else if (id_str == "true") {
    return LexerToken{id_str, Tag::TRUE};
  } else if (id_str == "false") {
    return LexerToken{id_str, Tag::FALSE};
  } else if (id_str == "call") {
    return LexerToken{id_str, Tag::CALL};
  }
  return LexerToken{id_str, Tag::IDENTIFIER};
}

}; // namespace Lexer
