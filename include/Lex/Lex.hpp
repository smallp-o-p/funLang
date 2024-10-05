#pragma once
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormatVariadicDetails.h"
#include <cassert>
#include <cstdint>
#include <deque>
#include <initializer_list>
#include <iostream>
#include <llvm/Support/SMLoc.h>
#include <llvm/Support/SourceMgr.h>
#include <memory>
#include <string>
#include <sys/types.h>
#include <vector>

import Basic;
import Basic.IdentifierTable;
import Basic.Diag;

namespace funLang {
using namespace Basic;

class Token {
  friend class Lexer;

private:
  Basic::tok::Tag LexicalTag;
  void *TheData;
  size_t DataSize;

  llvm::SMLoc LocStart;
  llvm::SMLoc LocEnd;

  // error
  explicit Token(llvm::SMLoc ErrLoc)
      : TheData(nullptr), LocStart(ErrLoc), LocEnd(llvm::SMLoc()), LexicalTag(Basic::tok::err), DataSize(0) {}
  // literal
  Token(llvm::StringRef Lexeme, size_t Size, Basic::tok::Tag LexicalTag, llvm::SMLoc Start, llvm::SMLoc End)
      : LexicalTag(LexicalTag), LocStart(Start), LocEnd(End), TheData((void *) Lexeme.data()), DataSize(Size) {};
  // punctuator or keyword
  Token(Basic::tok::Tag Kind, llvm::SMLoc LocStart, llvm::SMLoc LocEnd, size_t Size = 1)
      : LexicalTag(Kind), LocStart(LocStart),
        LocEnd(LocEnd),
        TheData(nullptr), DataSize(Size) {}
  // identifier
  Token(Basic::tok::Tag LexicalTag,
        llvm::StringMapEntry<std::nullopt_t> *IdentifierTableEntry,
        llvm::SMLoc LocStart,
        llvm::SMLoc LocEnd,
        size_t Size)
      : LexicalTag(LexicalTag), LocStart(LocStart), LocEnd(LocEnd), TheData((void *) IdentifierTableEntry),
        DataSize(Size) {};

public:
  Basic::tok::Tag getTag() const { return LexicalTag; }

  llvm::StringRef getLexeme() const;

  llvm::StringRef getLiteral() const {
    assert(isLiteral() && "Not a literal!");
    return {reinterpret_cast<const char *>(TheData), DataSize};
  }

  llvm::StringRef getIdentifier() const {
    assert(isIdentifier() && "Not an identifier!");
    return static_cast<llvm::StringMapEntry<std::nullopt_t> *>(TheData)->first();
  }

  llvm::StringMapEntry<std::nullopt_t> *getIdentifierTableEntry() const {
    assert(isIdentifier() && "Not an identifier!");
    return static_cast<llvm::StringMapEntry<std::nullopt_t> *>(TheData);
  }

  bool isOneOf(tok::Tag T1, tok::Tag T2) const {
    return is(T1) || is(T2);
  }

  template<typename... Ts>
  bool isOneOf(tok::Tag T1, Ts... Tss) const {
    return is(T1) || isOneOf(Tss...);
  }

  inline bool isIdentifier() const {
    return LexicalTag == Basic::tok::Tag::identifier;
  }

  bool isPunctuator() const {
    return LexicalTag < Basic::tok::Tag::last_punc && LexicalTag > Basic::tok::Tag::last_tok;
  }

  bool isKeyword() const {
    return LexicalTag > Basic::tok::last_punc && LexicalTag < Basic::tok::kw_last_keyword;
  }

  bool isLiteral() const {
    return LexicalTag == Basic::tok::floating_constant || LexicalTag == Basic::tok::numeric_constant
        || LexicalTag == Basic::tok::string_literal;
  }

  static bool isLiteral(Basic::tok::Tag Kind) {
    return Kind == Basic::tok::floating_constant || Kind == Basic::tok::numeric_constant
        || Kind == Basic::tok::string_literal;
  }

  bool isBaseType() const {
    return LexicalTag < Basic::tok::Tag::kw_last_type && LexicalTag > Basic::tok::Tag::last_punc;
  };

  void prettyPrint() {
    std::cout << Basic::tok::getTokenName(LexicalTag) << ": ";
    if (isIdentifier()) {
      std::cout << getIdentifier().str();
    } else if (isLiteral()) {
      std::cout << getLiteral().str();
    } else if (isKeyword()) {
      std::cout << Basic::tok::getKeywordSpelling(LexicalTag);
    } else if (isPunctuator()) {
      std::cout << Basic::tok::getPunctuatorSpelling(LexicalTag);
    }
  }

  llvm::SMLoc getLoc() { return LocStart; }

  llvm::SMLoc getRightmostLoc() {
    return LocEnd;
  }

  llvm::SMRange getLocRange() {
    return {LocStart, LocEnd};
  }

  bool is(Basic::tok::Tag K) const { return K == LexicalTag; }
};

class Lexer {
  friend class funLang::IdentifierTable;

private:
  std::vector<Token> tokens;   // keep track of old tokens for error messages
  std::deque<Token> unconsumed;// unconsumed tokens that are stored if we looked ahead.
  llvm::StringMap<Basic::tok::Tag> KeywordTable;

  std::shared_ptr<llvm::SourceMgr> srcManager;
  DiagEngine diagnostics;
  llvm::StringRef curBuf;
  llvm::StringRef::iterator bufPtr;
  funLang::IdentifierTable IdentTable;
  uint32_t currentBuffer = 0;

  void addKeywords() {
#define KEYWORD(NAME, FLAGS) addKeyword(#NAME, Basic::tok::kw_##NAME);
#include "Basic/defs/TokenTags.def"
  }

  Token formToken(const char *TokEnd, Basic::tok::Tag Kind);
  Token formErr();
  void addKeyword(const std::string &Kw, Basic::tok::Tag Tag) { KeywordTable.insert(std::make_pair(Kw, Tag)); }
  Basic::tok::Tag findKeyword(llvm::StringRef Name);
  Token lexString();
  Token lexNum();
  Token lexIdentifier();
  Token getNext();
  bool nextIs(char C);

public:
  Lexer(const std::shared_ptr<llvm::SourceMgr> &SrcMgr, DiagEngine &Diags)
      : srcManager(SrcMgr), diagnostics(Diags), IdentTable(IdentifierTable()) {
    currentBuffer = SrcMgr->getMainFileID();
    curBuf = SrcMgr->getMemoryBuffer(currentBuffer)->getBuffer();
    bufPtr = curBuf.begin();
    addKeywords();
  }

  llvm::SMLoc getCurLoc() { return llvm::SMLoc::getFromPointer(bufPtr); }

  static llvm::SMLoc getLocFrom(const char *Ptr) {
    return llvm::SMLoc::getFromPointer(Ptr - 1);
  }

  Token &advance();
  Token &peek();// equivalent to lookahead(1)
  Token previous();
  Token lookahead(size_t HowMuch);
  bool atEnd();
};

}// namespace funLang
