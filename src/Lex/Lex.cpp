#include "Lex.hpp"
#include "Basic.hpp"
#include <cstdint>

Token Lexer::getNext() {
  while (*bufPtr && iswspace(*bufPtr)) {
	++bufPtr;
  }
  switch (*bufPtr) {
  case ',':return formToken(bufPtr + 1, Basic::tok::Tag::comma);
  case '{':return formToken(bufPtr + 1, Basic::tok::l_brace);
  case '}':return formToken(bufPtr + 1, Basic::tok::r_brace);
  case '(':return formToken(bufPtr + 1, Basic::tok::Tag::l_paren);
  case ')':return formToken(bufPtr + 1, Basic::tok::Tag::r_paren);
  case '=':
	if (nextIs('=')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::equalequal);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::equal);
	}
  case '.':
	if (nextIs('.')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::dotdot);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::dot);
	}
  case ':':
	if (nextIs(':')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::coloncolon);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::colon);
	}
  case ';':return formToken(bufPtr + 1, Basic::tok::Tag::semi);
  case '!':
	if (nextIs('=')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::exclaimequal);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::exclaim);
	}
  case '<':
	if (nextIs('=')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::lessequal);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::less);
	}
  case '>':
	if (nextIs('=')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::greaterequal);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::greater);
	}
  case '+':
	if (nextIs('=')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::plusequal);
	} else if (nextIs('+')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::plusplus);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::plus);
	}
  case '-':
	if (nextIs('=')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::minusequal);
	} else if (nextIs('-')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::minusminus);
	} else if (isdigit(*(bufPtr + 1))) {
	  return lexNum(true);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::minus);
	}
  case '*':
	if (nextIs('=')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::starequal);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::star);
	}
  case '/':
	if (nextIs('=')) {
	  return formToken(bufPtr + 2, Basic::tok::Tag::slashequal);
	} else {
	  return formToken(bufPtr + 1, Basic::tok::Tag::slash);
	}
  case '\"':return lexString();
  default: {
	if (isdigit(*bufPtr)) {
	  return lexNum();
	}
	if (isalpha(*bufPtr)) {
	  return lexIdentifier();
	}
	if (!*bufPtr) {
	  return formToken(bufPtr, Basic::tok::Tag::eof);
	} else {
	  diagnostics.emitDiagMsg(getCurLoc(), diag::err_unexpected_char, *bufPtr);
	  return formErr();
	}
  }
  }
}

bool Lexer::nextIs(char c) {
  if (*(bufPtr + 1)==c) {
	return true;
  }
  return false;
}

Token Lexer::lexString() {
  auto end = bufPtr + 1;
  while (*end && *end++!='\"') {
  }
  if (!*end) {
	diagnostics.emitDiagMsg(getLocFrom(bufPtr), diag::err_unterminated_char_or_string);
	return formErr();
  }
  return formToken(end, Basic::tok::string_literal);
}

Token Lexer::lexNum(bool negative) {
  auto end = negative ? bufPtr + 1 : bufPtr; // handle negative case
  bool seenDot = false;
  while (*end && (isdigit(*end) || *end=='.')) {
	if (seenDot && *end=='.') {
	  diagnostics.emitDiagMsg(getLocFrom(end + 1), diag::err_unexpected_char, *end);
	  return formErr();
	}
	if (*end=='.' && !seenDot) {
	  seenDot = true;
	}
	end++;
  }
  if (!*end) {
	return formErr();
  }
  if (seenDot) {
	return formToken(end,
					 Basic::tok::Tag::floating_constant);
  }
  return formToken(end,
				   Basic::tok::Tag::numeric_constant);
}

Token Lexer::lexIdentifier() {
  auto end = bufPtr;

  while (isalnum(*end) || *end=='_') {
	end++;
  }
  std::string temp = std::string(bufPtr, end);
  return formToken(end, findKeyword(temp));
}

/**
  Consume the next token and return it. Check for any retrieved but unconsumed
  tokens.
*/
Token &Lexer::advance() {
  Token tok = unconsumed.empty() ? getNext() : unconsumed.front();
  if (!unconsumed.empty()) {
	unconsumed.pop_front();
  }
  tokens.push_back(tok);
  return tokens.back();
}

/**
Lookahead of 1 token.
*/
Token &Lexer::peek() {
  if (!unconsumed.empty()) {
	return unconsumed.front();
  } else {
	unconsumed.push_back(getNext());
  }
  return unconsumed.front();
}

/**
Lookahead of n tokens.
*/
Token &Lexer::lookahead(uint32_t howMuch) {
  while (howMuch > unconsumed.size()) {
	unconsumed.push_back(getNext());
  }
  return unconsumed.at(howMuch - 1);
}

Basic::tok::Tag Lexer::findKeyword(std::string_view name) {
  auto result = keywordMap.find(name);
  if (result!=keywordMap.end()) {
	return result->second;
  }
  return Basic::tok::Tag::identifier;
}

bool Lexer::atEnd() {
  return !*bufPtr;
}

Token Lexer::formToken(const char *tokEnd, Basic::tok::Tag kind) {
  Token tok = Token{llvm::StringRef(bufPtr, static_cast<size_t>(tokEnd - bufPtr)), kind};
  bufPtr = tokEnd;
  return tok;
}

Token Lexer::formErr() {
  bufPtr++;
  return {llvm::StringRef(""), Basic::tok::Tag::err};
}

Token::Token(llvm::StringRef lexeme, Basic::tok::Tag syntactic_category)
	: lexeme(lexeme), syntactic_category(syntactic_category) {}
