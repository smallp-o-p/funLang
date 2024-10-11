//
// Created by will on 10/5/24.
//
module;
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <iostream>
#include <unistd.h>
export module Lex:Token;
import Basic;
import Basic.IdentifierTable;

namespace funLang {
using namespace Basic;
export class Token {
  friend class Lexer;

private:
  tok::Tag LexicalTag;
  void *TheData;
  size_t DataSize;

  llvm::SMLoc LocStart;
  llvm::SMLoc LocEnd;

  // error
  explicit Token(llvm::SMLoc ErrLoc)
      : LexicalTag(tok::err), TheData(nullptr), DataSize(0), LocStart(ErrLoc), LocEnd(llvm::SMLoc()) {}
  // literal
  Token(llvm::StringRef Lexeme, size_t Size, tok::Tag LexicalTag, llvm::SMLoc Start, llvm::SMLoc End)
      : LexicalTag(LexicalTag), TheData((void *) Lexeme.data()), DataSize(Size), LocStart(Start), LocEnd(End){};
  // punctuator or keyword
  Token(Basic::tok::Tag Kind, llvm::SMLoc LocStart, llvm::SMLoc LocEnd, size_t Size = 1)
      : LexicalTag(Kind), TheData(nullptr),
        DataSize(Size),
        LocStart(LocStart), LocEnd(LocEnd) {}
  // identifier
  Token(Basic::tok::Tag LexicalTag,
        llvm::StringMapEntry<std::nullopt_t> *IdentifierTableEntry,
        llvm::SMLoc LocStart,
        llvm::SMLoc LocEnd,
        size_t Size)
      : LexicalTag(LexicalTag), TheData((void *) IdentifierTableEntry), DataSize(Size), LocStart(LocStart),
        LocEnd(LocEnd){};

public:
  tok::Tag getTag() const { return LexicalTag; }

  llvm::StringRef getLexeme() const {
    if (isLiteral()) {
      return getLiteral();
    } else if (isIdentifier()) {
      return getIdentifier();
    } else if (isPunctuator()) {
      return {getPunctuatorSpelling(LexicalTag)};
    }
    return getTokenName(LexicalTag);
  }

  llvm::StringRef getLiteral() const {
    assert(isLiteral() && "Not a literal!");
    return {static_cast<const char *>(TheData), DataSize};
  }

  llvm::StringRef getIdentifier() const {
    assert(isIdentifier() && "Not an identifier!");
    return static_cast<llvm::StringMapEntry<std::nullopt_t> *>(TheData)->first();
  }

  IDTableEntry *getIdentifierTableEntry() const {
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
    return LexicalTag == tok::Tag::identifier;
  }

  bool isPunctuator() const {
    return LexicalTag < tok::Tag::last_punc && LexicalTag > tok::Tag::last_tok;
  }

  bool isKeyword() const {
    return LexicalTag > tok::last_punc && LexicalTag < tok::kw_last_keyword;
  }

  bool isLiteral() const {
    return LexicalTag == tok::floating_constant || LexicalTag == tok::numeric_constant
        || LexicalTag == tok::string_literal;
  }

  static bool isLiteral(tok::Tag Kind) {
    return Kind == tok::floating_constant || Kind == tok::numeric_constant
        || Kind == tok::string_literal;
  }

  bool isBaseType() const {
    return LexicalTag < tok::Tag::kw_last_type && LexicalTag > tok::Tag::last_punc;
  }

  void prettyPrint() const {
    std::cout << getTokenName(LexicalTag) << ": ";
    if (isIdentifier()) {
      std::cout << getIdentifier().str();
    } else if (isLiteral()) {
      std::cout << getLiteral().str();
    } else if (isKeyword() || isPunctuator()) {
      std::cout << getTokenName(LexicalTag);
    }
  }

  llvm::SMLoc getLoc() const { return LocStart; }

  llvm::SMLoc getRightmostLoc() const { return LocEnd; }

  llvm::SMRange getLocRange() { return {LocStart, LocEnd}; }

  bool is(const tok::Tag K) const { return K == LexicalTag; }
};

}// namespace funLang
