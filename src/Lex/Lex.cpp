#include "Lex.hpp"
#include "TokenTags.hpp"
#include <cstdint>

Token Lexer::getNext() {
  char c = *bufPtr++;
  while (iswspace(c)) {
    c = *bufPtr++;
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
    --bufPtr;
    return lexNum();
  }
  if (isalpha(c)) {
    --bufPtr;
    return lexIdentifier();
  }
  if (!c) {
    return formKwToken(Basic::tok::Tag::eof);
  } else {
    return formKwToken(Basic::tok::Tag::err);
  }
}

bool Lexer::nextIs(char c) {
  if (*(bufPtr) == c) { // bufPtr is already looking at the next character
    bufPtr++;
    return true;
  }
  return false;
}

Token Lexer::lexString() {
  auto start = bufPtr - 1;
  while (*bufPtr && *bufPtr++ != '\"') {
  };
  if (!*bufPtr) {
    std::cerr << "Unclosed string literal :(" << std::endl;
    return formKwToken(Basic::tok::err);
  }
  return formToken(std::string(start, bufPtr), Basic::tok::string_literal);
}

Token Lexer::lexNum() {
  auto start = bufPtr;
  bool seenDot = false;
  while (*bufPtr && (isdigit(*bufPtr) || *bufPtr == '.')) {
    if (seenDot && *bufPtr == '.') {
      break;
    }
    if (*bufPtr == '.' && !seenDot) {
      seenDot = true;
    }
    bufPtr++;
  }
  if (!*bufPtr) {
    return formKwToken(Basic::tok::Tag::err);
  }
  if (seenDot) {
    return formToken(std::string(start, bufPtr),
                     Basic::tok::Tag::floating_constant);
  }
  return formToken(std::string(start, bufPtr),
                   Basic::tok::Tag::numeric_constant);
}

Token Lexer::lexIdentifier() {
  auto start = bufPtr;

  while (isalnum(*bufPtr) || *bufPtr == '_') {
    bufPtr++;
  }
  std::string temp = std::string(start, bufPtr);
  Basic::tok::Tag kw = findKeyword(temp);
  return formToken(temp, findKeyword(temp));
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
  return unconsumed.at(howMuch - 1);
}
