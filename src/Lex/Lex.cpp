#include "Lex.hpp"

std::unique_ptr<Lexer> Lexer::init(const std::string &filename,
                                   bool usingString) {
  std::unique_ptr<std::istream> i_stream;
  if (filename.empty()) {
    std::cout << "Usage: funLang {FILE_PATH_DIR}" << std::endl;
    return nullptr;
  }
  if (usingString) {
    std::cout << "Using String" << std::endl;
    i_stream = std::make_unique<std::istringstream>(filename);
  } else {
    i_stream = std::make_unique<std::ifstream>(filename);
  }
  if (i_stream->fail()) {
    std::cerr << "Failed to process input :(" << std::endl;
    return nullptr;
  }
  return std::make_unique<Lexer>(std::move(i_stream));
}

Lexer::Token Lexer::getNext() {
  char c = ' ';
  while (isspace(c)) {
    if (c == '\n') {
      lineNum++;
      colNum = 0;
    }
    colNum++;
    c = in_stream->get();
  }
  switch (c) {
  case ',':
    return Token{",", Tag::COMMA};
  case '{':
    return Token{"{", Tag::LCURLY};
  case '}':
    return Token{"}", Tag::RCURLY};
  case '(':
    return Token{"(", Tag::LPAREN};
  case ')':
    return Token{")", Tag::RPAREN};
  case '=':
    if (nextIs('=')) {
      return Token{"==", Tag::EQCMP};
    } else {
      return Token{"=", Tag::EQ};
    }
  case ':':
    return Token{":", Tag::COLON};
  case ';':
    return Token{";", Tag::SEMI};
  case '!':
    if (nextIs('=')) {
      return Token{"!=", Tag::NECMP};
    } else {
      return Token{"!", Tag::BANG};
    }
  case '<':
    if (nextIs('=')) {
      return Token{"<=", LTECMP};
    } else {
      return Token{"<", LTCMP};
    }
  case '>':
    if (nextIs('=')) {
      return Token{">=", GTECMP};
    } else {
      return Token{">", GTCMP};
    }
  case '+':
    return nextIs('+') ? Token{"++", PLUSPLUS} : Token{"+", PLUS};
  case '-':
    return nextIs('-') ? Token{"++", Tag::MINUSMINUS} : Token{"-", MINUS};
  case '*':
    return Token{"*", MULT};
  case '/':
    return Token{"/", DIV};
  case '\"':
    return lexString();
  }
  if (isdigit(c)) {
    in_stream->putback(c);
    return lexNum();
  }
  if (isalpha(c)) {
    in_stream->putback(c);
    return lexIdentifier();
  }
  if (in_stream->eof()) {
    return Token{"!!EOF!!", Tag::ENDFILE};
  } else {
    std::cout << "Fatal: Unexpected character '" << c << "' on line " << lineNum
              << std::endl;
    return Token{"UH OH", Tag::ERR};
  }
}

/*
 * nextIs(char c) checks if the next character in the input buffer is c, if
 * so it eats the next character and returns true. Does nothing and returns
 * false otherwise.
 * */
bool Lexer::nextIs(char c) {
  if (in_stream->peek() == c) {
    in_stream->get();
    return true;
  }
  return false;
}
Lexer::Token Lexer::lexString() {
  char c;
  std::string string_lit = "\"";

  while ((c = in_stream->get()) != '\"' && c != EOF) {
    string_lit.push_back(c);
  };

  if (in_stream->eof()) {
    std::cerr << "Unclosed string literal :(" << std::endl;
    return Token{"UH OH", Tag::ERR};
  }
  string_lit.push_back('\"');
  return Token{string_lit, Tag::STRINGLIT};
}

Lexer::Token Lexer::lexNum() {
  std::string numStr = "";
  char c;
  bool seenDot = false;
  while ((isdigit(in_stream->peek()) || in_stream->peek() == '.')) {
    if (seenDot && in_stream->peek() == '.') {
      break;
    }
    if (in_stream->peek() == '.' && !seenDot) {
      seenDot = true;
    }
    c = in_stream->get();
    numStr.push_back(c);
  }
  if (numStr.back() == '.') { // allow numbers like 12.
    numStr.push_back('0');
  }
  if (seenDot) {
    return Token{numStr, Tag::POINTNUM};
  }
  return Token{numStr, Tag::NUM};
}

Lexer::Token Lexer::lexIdentifier() {

  std::string id_str = "";

  char c;

  while (isalnum(in_stream->peek()) || in_stream->peek() == '_') {
    c = in_stream->get();
    id_str.push_back(c);
  }
  if (id_str.compare("void") == 0) {
    return Token{id_str, Tag::VOID};
  } else if (id_str.compare("bool") == 0) {
    return Token{id_str, Tag::BOOL};
  } else if (id_str.compare("char") == 0) {
    return Token{id_str, Tag::CHAR};
  } else if (id_str.compare("string") == 0) {
    return Token{id_str, Tag::STRING};
  } else if (id_str == "i32") {
    return Token{id_str, Tag::I32};
  } else if (id_str == "i64") {
    return Token{id_str, Tag::I64};
  } else if (id_str == "f32") {
    return Token{id_str, Tag::F32};
  } else if (id_str == "f64") {
    return Token{id_str, Tag::F64};
  } else if (id_str == "return") {
    return Token{id_str, Tag::RETURN};
  } else if (id_str == "true") {
    return Token{id_str, Tag::TRUE};
  } else if (id_str == "false") {
    return Token{id_str, Tag::FALSE};
  }
  return Token{id_str, Tag::IDENTIFIER};
}
