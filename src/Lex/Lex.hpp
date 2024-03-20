#pragma once
#include "Diag.hpp"
#include "TokenTags.hpp"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/ErrorHandling.h"
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
  std::string lexeme;
  Basic::tok::Tag syntactic_category;

private:
  Token(const std::string &lexeme, Basic::tok::Tag syntactic_category)
      : lexeme(lexeme), syntactic_category(syntactic_category) {}
  Token(Basic::tok::Tag syn_cat) {
    assert((syn_cat != Basic::tok::identifier &&
            syn_cat != Basic::tok::NUM_TOKENS) &&
           "Attempted to invoke Token enum-only constructor with identifer "
           "or num_tokens\n");
    syntactic_category = syn_cat;
  }

public:
  Basic::tok::Tag getTag() const { return syntactic_category; }
  const std::string &getIdentifier() {
    assert(syntactic_category == Basic::tok::identifier &&
           "Cannot get identifier of non-identifier token.");
    return lexeme;
  }

  const std::string_view getLexeme() { return lexeme; }
  void prettyPrint() {
    std::cout << Basic::tok::getTokenName(syntactic_category) << ": " << lexeme
              << std::endl;
  }
  bool is(Basic::tok::Tag K) const { return K == syntactic_category; }
};

class Lexer {
private:
  llvm::StringMap<Basic::tok::Tag> keywordMap;

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

  Token formToken(std::string name, Basic::tok::Tag kind) {
    return Token(name, kind);
  };
  Token formKwToken(Basic::tok::Tag kind) { return Token(kind); }
  void addKeyword(const std::string &kw, Basic::tok::Tag tag) {
    keywordMap.insert(std::make_pair(kw, tag));
  }
  Basic::tok::Tag findKeyword(std::string &name) {
    auto result = keywordMap.find(name);
    if (result != keywordMap.end()) {
      return result->second;
    }
    return Basic::tok::Tag::identifier;
  }

  void addKeywords() {
#define KEYWORD(NAME, FLAGS) addKeyword(#NAME, Basic::tok::kw_##NAME);
#include "TokenTags.def"
  }

private:
  std::shared_ptr<llvm::SourceMgr> srcManager;
  DiagEngine diagnostics;
  llvm::StringRef curBuf;
  llvm::StringRef::iterator bufPtr;
  uint32_t currentBuffer = 0;

public:
  Lexer(std::shared_ptr<llvm::SourceMgr> srcMgr, DiagEngine &diags)
      : srcManager(srcMgr), diagnostics(diags) {
    currentBuffer = srcMgr->getMainFileID();
    curBuf = srcMgr->getMemoryBuffer(currentBuffer)->getBuffer();
    bufPtr = curBuf.begin();
    addKeywords();
  }

  Token advance();
  Token peek(); // equivalent to lookahead(1)
  Token previous() { return tokens.at(tok_tracker); };
  bool match(std::initializer_list<Basic::tok::Tag> toks);
  Token lookahead(uint32_t howMuch);
  void backtrack(uint32_t howMuch);
  void atEnd();
};
