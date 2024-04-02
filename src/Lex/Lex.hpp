#pragma once
#include "Diag.hpp"
#include "TokenTags.hpp"
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
  Token(llvm::StringRef lexeme, Basic::tok::Tag syntactic_category);
public:

  Basic::tok::Tag getTag() const { return syntactic_category; }

  const char *getIdentifier() {
	assert(syntactic_category==Basic::tok::identifier &&
		"Cannot get identifier of non-identifier token.");
	return lexeme.data();
  }

  llvm::StringRef getLexeme() { return lexeme; }

  void prettyPrint() {
	std::cout << Basic::tok::getTokenName(syntactic_category) << ": " << lexeme.str()
			  << std::endl;
  }

  llvm::SMLoc getFromPtr() {
	return llvm::SMLoc::getFromPointer(lexeme.data());
  }

  bool is(Basic::tok::Tag K) const { return K==syntactic_category; }
};

class Lexer {
private:
  llvm::StringMap<Basic::tok::Tag> keywordMap;
  void addKeywords() {
	#define KEYWORD(NAME, FLAGS) addKeyword(#NAME, Basic::tok::kw_##NAME);
	#include "TokenTags.def"
  }

  Token formToken(const char *tokEnd, Basic::tok::Tag kind);

  Token formErr();

  void addKeyword(const std::string &kw, Basic::tok::Tag tag) {
	keywordMap.insert(std::make_pair(kw, tag));
  }

  Basic::tok::Tag findKeyword(std::string_view name);

  std::vector<Token> tokens;    // keep track of old tokens for error messages
  std::deque<Token> unconsumed; // unconsumed tokens, only here cause we peeked
  // or did some lookahead
  int32_t tok_tracker = -1; // should be pointing to the most recently consumed
  // token, might not be necessary
  Token lexString();
  Token lexNum();
  Token lexIdentifier();
  Token getNext();
  bool nextIs(char c);

private:
  std::shared_ptr<llvm::SourceMgr> srcManager;
  DiagEngine diagnostics;
  llvm::StringRef curBuf;
  llvm::StringRef::iterator bufPtr;
  uint32_t currentBuffer = 0;

public:
  Lexer(const std::shared_ptr<llvm::SourceMgr> &srcMgr, DiagEngine &diags)
	  : srcManager(srcMgr), diagnostics(diags) {
	currentBuffer = srcMgr->getMainFileID();
	curBuf = srcMgr->getMemoryBuffer(currentBuffer)->getBuffer();
	bufPtr = curBuf.begin();
	addKeywords();
  }

  llvm::SMLoc getCurLoc() {
	return llvm::SMLoc::getFromPointer(bufPtr);
  }

  llvm::SMLoc getLocFrom(const char *ptr) {
	return llvm::SMLoc::getFromPointer(ptr - 1);
  }

  Token advance();
  Token peek(); // equivalent to lookahead(1)
  Token previous() { return tokens.at(tok_tracker); };
  Token lookahead(uint32_t howMuch);
  bool atEnd();
};
