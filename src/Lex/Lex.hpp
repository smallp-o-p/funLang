#pragma once
#include "Diag.hpp"
#include "Basic.hpp"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormatVariadicDetails.h"
#include <cassert>
#include <cstdint>
#include <deque>
#include <initializer_list>
#include <iostream>
#include <llvm/Support/SourceMgr.h>
#include <memory>
#include <string>
#include <sys/types.h>
#include <vector>

class Token {
  friend class Lexer;
private:

  llvm::StringRef lexeme; // string ref because we need the pointer to the location in the memory buffer
  Basic::tok::Tag syntactic_category;
  Token(llvm::StringRef Lexeme, Basic::tok::Tag SyntacticCategory);
public:

  Basic::tok::Tag getTag() const { return syntactic_category; }
  bool isIdentifier() const { return syntactic_category == Basic::tok::Tag::identifier; }
  bool isPunctuator() const {
	return syntactic_category < Basic::tok::Tag::last_punc && syntactic_category > Basic::tok::Tag::last_tok;
  }
  bool isBaseType() const {
	return syntactic_category < Basic::tok::Tag::kw_last_type && syntactic_category > Basic::tok::Tag::last_punc;
  };
  llvm::StringRef getIdentifier() {
	assert(syntactic_category == Basic::tok::identifier &&
		"Cannot get identifier of non-identifier token.");
	return lexeme;
  }

  llvm::StringRef getLexeme() { return lexeme; }

  void prettyPrint() {
	std::cout << Basic::tok::getTokenName(syntactic_category) << ": " << lexeme.str()
			  << std::endl;
  }

  llvm::SMLoc getLoc() {
	return llvm::SMLoc::getFromPointer(lexeme.data());
  }

  llvm::SMRange getLocRange() {
	llvm::SMLoc End = llvm::SMLoc::getFromPointer(lexeme.data() + lexeme.size());
	assert(End.isValid());
	return {llvm::SMLoc::getFromPointer(lexeme.data()),
			End};
  }

  bool is(Basic::tok::Tag K) const { return K == syntactic_category; }
};

class Lexer {
private:
  llvm::StringMap<Basic::tok::Tag> keywordMap;
  void addKeywords() {
	#define KEYWORD(NAME, FLAGS) addKeyword(#NAME, Basic::tok::kw_##NAME);
	#include "defs/TokenTags.def"
  }

  Token formToken(const char *TokEnd, Basic::tok::Tag Kind);

  Token formErr();

  void addKeyword(const std::string &Kw, Basic::tok::Tag Tag) {
	keywordMap.insert(std::make_pair(Kw, Tag));
  }

  Basic::tok::Tag findKeyword(std::string_view Name);
  std::vector<Token> tokens;    // keep track of old tokens for error messages
  std::deque<Token> unconsumed; // unconsumed tokens that are stored if we looked ahead.
  Token lexString();
  Token lexNum();
  Token lexIdentifier();
  Token getNext();
  bool nextIs(char C);

private:
  std::shared_ptr<llvm::SourceMgr> srcManager;
  DiagEngine diagnostics;
  llvm::StringRef curBuf;
  llvm::StringRef::iterator bufPtr;
  uint32_t currentBuffer = 0;
public:
  Lexer(const std::shared_ptr<llvm::SourceMgr> &SrcMgr, DiagEngine &Diags)
	  : srcManager(SrcMgr), diagnostics(Diags) {
	currentBuffer = SrcMgr->getMainFileID();
	curBuf = SrcMgr->getMemoryBuffer(currentBuffer)->getBuffer();
	bufPtr = curBuf.begin();
	addKeywords();
  }

  llvm::SMLoc getCurLoc() {
	return llvm::SMLoc::getFromPointer(bufPtr);
  }

  static llvm::SMLoc getLocFrom(const char *Ptr) {
	return llvm::SMLoc::getFromPointer(Ptr - 1);
  }

  Token &advance();
  Token &peek(); // equivalent to lookahead(1)
  Token previous();;
  Token lookahead(size_t HowMuch);
  bool atEnd();
};
