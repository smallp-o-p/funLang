#include "Lex.hpp"
#include "TokenTags.hpp"
#include <cstdint>
#include <fstream>
#include <sstream>
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

Token Lexer::getNext() {
  char c = ' ';
  while (isspace(c)) {
    if (c == '\n') {
      lineNum++;
      colNum = 1;
    }
    colNum++;
    c = in_stream->get();
  }
  switch (c) {
  case ',':
    return formKwToken(Basic::tok::Tag::comma);
  case '{':
    return formKwToken(Basic::tok::l_brace);
  case '}':
    return formKwToken(Basic::tok::r_brace);
  case '(':
    return formKwToken(Basic::tok::Tag::l_paren);
  case ')':
    return formKwToken(Basic::tok::Tag::r_paren);
  case '=':
    if (nextIs('=')) {
      return formKwToken(Basic::tok::Tag::equalequal);
    } else {
      return formKwToken(Basic::tok::Tag::equal);
    }
  case ':':
    return formKwToken(Basic::tok::Tag::colon);
  case ';':
    return formKwToken(Basic::tok::Tag::semi);
  case '!':
    if (nextIs('=')) {
      return formKwToken(Basic::tok::Tag::exclaimequal);
    } else {
      return formKwToken(Basic::tok::Tag::exclaim);
    }
  case '<':
    if (nextIs('=')) {
      return formKwToken(Basic::tok::Tag::lessequal);
    } else {
      return formKwToken(Basic::tok::Tag::less);
    }
  case '>':
    if (nextIs('=')) {
      return formKwToken(Basic::tok::Tag::greaterequal);
    } else {
      return formKwToken(Basic::tok::Tag::greater);
    }
  case '+':
    return nextIs('+') ? formKwToken(Basic::tok::Tag::plusplus)
                       : formKwToken(Basic::tok::Tag::plus);
  case '-':
    return nextIs('-') ? formKwToken(Basic::tok::Tag::minusminus)
                       : formKwToken(Basic::tok::minus);
  case '*':
    return formKwToken(Basic::tok::Tag::star);
  case '/':
    return formKwToken(Basic::tok::Tag::slash);
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
    return formKwToken(Basic::tok::Tag::eof);
  } else {
    std::cout << "Fatal: Unexpected character '" << c << "' on line " << lineNum
              << std::endl;
    return formKwToken(Basic::tok::Tag::err);
  }
}
/**
  Consume the next token and return it. Check for any retrieved but unconsumed
  tokens.
*/
Token Lexer::advance() {
  Token tok = unconsumed.empty() ? getNext() : unconsumed.front();
  if (!unconsumed.empty()) {
    unconsumed.pop_front();
  }
  tokens.push_back(tok);
  tok_tracker++;
  return tok;
}

/**
Lookahead of 1 token.
*/
Token Lexer::peek() {
  if (!unconsumed.empty()) {
    return unconsumed.front();
  } else {
    unconsumed.push_back(getNext());
  }
  return unconsumed.back();
}

/**
Lookahead of n tokens.
*/
Token Lexer::lookahead(uint32_t howMuch) {
  while (unconsumed.size() < howMuch) {
    unconsumed.push_back(getNext());
  }
  return unconsumed.at(howMuch);
}

bool Lexer::nextIs(char c) {
  if (in_stream->peek() == c) {
    in_stream->get();
    return true;
  }
  return false;
}

Token Lexer::lexString() {
  uint32_t howLong, howTall = 0;
  char c;
  std::string string_lit = "\"";

  while ((c = in_stream->get()) != '\"' && c != EOF) {
    if (c == '\n') {
      howTall++;
      colNum = 0;
    } else {
      howLong++;
    }
    string_lit.push_back(c);
  };

  if (in_stream->eof()) {
    std::cerr << "Unclosed string literal :(" << std::endl;
    return formKwToken(Basic::tok::err);
  }
  string_lit.push_back('\"');
  colNum += howLong;
  lineNum += howTall;
  return formToken(string_lit, Basic::tok::string_literal);
}

Token Lexer::lexNum() {
  std::string numStr = "";
  uint32_t howLong = -1; // we pushed back a character
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
    return formToken(numStr, Basic::tok::Tag::floating_constant);
  }
  colNum += howLong;
  return formToken(numStr, Basic::tok::Tag::numeric_constant);
}

Token Lexer::lexIdentifier() {
  std::string id_str = "";
  char c;

  while (isalnum(in_stream->peek()) || in_stream->peek() == '_') {
    c = in_stream->get();
    id_str.push_back(c);
  }
  Basic::tok::Tag kw = findKeyword(id_str);
  if (kw != Basic::tok::Tag::identifier) {
    return formKwToken(kw);
  }
  return formToken(id_str, findKeyword(id_str));
}
