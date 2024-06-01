#include "Lex.hpp"
#include "Basic.hpp"

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

bool Lexer::nextIs(char C) {
  if (*(bufPtr + 1) == C) {
	return true;
  }
  return false;
}

Token Lexer::lexString() {
  auto End = bufPtr + 1;
  while (*End && *End++ != '\"') {
  }
  if (!*End) {
	diagnostics.emitDiagMsg(getLocFrom(bufPtr), diag::err_unterminated_char_or_string);
	return formErr();
  }
  return formToken(End, Basic::tok::string_literal);
}

Token Lexer::lexNum() {
  auto End = bufPtr; // handle negative case
  bool SeenDot = false;
  while (*End && (isdigit(*End) || *End == '.')) {
	if (SeenDot && *End == '.') {
	  diagnostics.emitDiagMsg(getLocFrom(End + 1), diag::err_unexpected_char, *End);
	  return formErr();
	}
	if (*End == '.' && !SeenDot) {
	  SeenDot = true;
	}
	End++;
  }
  if (!*End) {
	return formErr();
  }
  if (SeenDot) {
	return formToken(End,
					 Basic::tok::Tag::floating_constant);
  }
  return formToken(End,
				   Basic::tok::Tag::numeric_constant);
}

Token Lexer::lexIdentifier() {
  auto End = bufPtr;

  while (isalnum(*End) || *End == '_') {
	End++;
  }
  return formToken(End, findKeyword(std::string(bufPtr, End)));
}

/**
  Consume the next token and return it. Check for any retrieved but unconsumed
  tokens.
*/
Token &Lexer::advance() {
  Token Tok = unconsumed.empty() ? getNext() : unconsumed.front();
  if (!unconsumed.empty()) {
	unconsumed.pop_front();
  }
  tokens.push_back(Tok);
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
Token Lexer::lookahead(size_t HowMuch) {
  while (HowMuch > unconsumed.size()) {
	unconsumed.push_back(getNext());
  }
  return unconsumed.at(HowMuch - 1);
}

Basic::tok::Tag Lexer::findKeyword(std::string_view Name) {
  auto Result = keywordMap.find(Name);
  if (Result != keywordMap.end()) {
	return Result->second;
  }
  return Basic::tok::Tag::identifier;
}

bool Lexer::atEnd() {
  return !*bufPtr;
}

Token Lexer::formToken(const char *TokEnd, Basic::tok::Tag Kind) {
  Token Tok = Token{llvm::StringRef(bufPtr, static_cast<size_t>(TokEnd - bufPtr)), Kind};
  bufPtr = TokEnd;
  return Tok;
}

Token Lexer::formErr() {
  bufPtr++;
  return {llvm::StringRef(""), Basic::tok::Tag::err};
}
Token Lexer::previous() { return tokens.back(); }

Token::Token(llvm::StringRef Lexeme, Basic::tok::Tag SyntacticCategory)
	: lexeme(Lexeme), syntactic_category(SyntacticCategory) {}
