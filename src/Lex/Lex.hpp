#pragma once
#include "TokenTags.hpp"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>
#include <cstdint>
#include <deque>
#include <initializer_list>
#include <iostream>
#include <istream>
#include <memory>
#include <string>
#include <sys/types.h>
#include <vector>

class Token {
  friend class Lexer;

private:
  std::string lexeme;
  Basic::tok::Tag syntactic_category;
  uint32_t colNum, lineNum;
  Token(const std::string &lexeme, Basic::tok::Tag syntactic_category,
        uint32_t col, uint32_t line)
      : lexeme(lexeme), syntactic_category(syntactic_category), colNum(col),
        lineNum(line) {}
  Token(Basic::tok::Tag syn_cat, uint32_t col, uint32_t line) {
    assert((syn_cat != Basic::tok::identifier &&
            syn_cat != Basic::tok::NUM_TOKENS) &&
           "Attempted to invoke Token enum-only constructor with identifer "
           "or num_tokens\n");
    syntactic_category = syn_cat;
    colNum = col;
    lineNum = line;
  }

public:
  void prettyPrint() {
    std::cout << Basic::tok::getTokenName(syntactic_category) << ": " << lexeme
              << std::endl;
  }
  Basic::tok::Tag getTag() { return syntactic_category; }
  std::string &getLexeme() { return lexeme; }
};

class Lexer {
private:
  llvm::StringMap<Basic::tok::Tag> keywordMap;
  std::vector<Token> tokens;    // keep track of old tokens for error messages
  std::deque<Token> unconsumed; // unconsumed tokens, only here cause we peeked
                                // or did some lookahead
  int32_t tok_tracker = -1; // should be pointing to the most recently consumed
                            // token, might not be necessary
  std::unique_ptr<std::istream> in_stream;

  uint32_t colNum = 1;
  uint32_t lineNum = 1;
  Token lexString();
  Token lexNum();
  Token lexIdentifier();
  Token getNext();
  bool nextIs(char c);
  Token formToken(std::string &name, Basic::tok::Tag kind) {
    return Token(name, kind, colNum, lineNum);
  };
  Token formKwToken(Basic::tok::Tag kind) {
    return Token(kind, colNum, lineNum);
  }
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

public:
  Lexer(std::unique_ptr<std::istream> in) : in_stream(std::move(in)) {
    addKeywords();
  }
  Token advance();
  static std::unique_ptr<Lexer> init(const std::string &filename,
                                     bool usingString = false);
  Token peek(); // equivalent to lookahead(1)
  Token previous() { return tokens.at(tok_tracker); };
  bool match(std::initializer_list<Basic::tok::Tag> toks);
  Token lookahead(uint32_t howMuch);
  void backtrack(uint32_t howMuch);
  void atEnd();
};
