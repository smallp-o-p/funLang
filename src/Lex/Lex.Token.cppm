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

namespace funLang {
using namespace Basic;
export class Token {
  friend class Lexer;

  tok::Tag LexicalTag{};
  const void *TheData{};
  size_t DataSize;
  llvm::SMLoc LocStart, LocEnd;

  // error
  explicit Token(const llvm::SMLoc ErrLoc)
      : LexicalTag(tok::err), DataSize(0), LocStart(ErrLoc),
        LocEnd(llvm::SMLoc()) {}
  // literal
  Token(const llvm::StringRef Lexeme, const size_t Size,
        const tok::Tag LexicalTag, const llvm::SMLoc Start,
        const llvm::SMLoc End)
      : LexicalTag(LexicalTag),
        TheData(static_cast<const void *>(Lexeme.data())), DataSize(Size),
        LocStart(Start), LocEnd(End) {}
  // punctuator or keyword
  Token(const tok::Tag Kind, const llvm::SMLoc LocStart,
        const llvm::SMLoc LocEnd, const size_t Size = 1)
      : LexicalTag(Kind), DataSize(Size), LocStart(LocStart), LocEnd(LocEnd) {}
  // identifier
  Token(const tok::Tag LexicalTag,
        const llvm::StringMapEntry<std::nullopt_t> *IdentifierTableEntry,
        const llvm::SMLoc LocStart, const llvm::SMLoc LocEnd, const size_t Size)
      : LexicalTag(LexicalTag),
        TheData(static_cast<const void *>(IdentifierTableEntry)),
        DataSize(Size), LocStart(LocStart), LocEnd(LocEnd) {}

public:
  [[nodiscard]] tok::Tag getTag() const { return LexicalTag; }

  [[nodiscard]] llvm::StringRef getLexeme() const {
    if (isLiteral())
      return getLiteral();
    if (isIdentifier())
      return getIdentifier();
    if (isPunctuator()) {
      return {getPunctuatorSpelling(LexicalTag)};
    }

    return getTokenName(LexicalTag);
  }

  [[nodiscard]] llvm::StringRef getLiteral() const {
    assert(isLiteral() && "Not a literal!");
    return {static_cast<const char *>(TheData), DataSize};
  }

  [[nodiscard]] llvm::StringRef getIdentifier() const {
    assert(isIdentifier() && "Not an identifier!");
    return static_cast<const llvm::StringMapEntry<std::nullopt_t> *>(TheData)
        ->first();
  }

  [[nodiscard]] const IDTableEntry *getIdentifierTableEntry() const {
    assert(isIdentifier() && "Not an identifier!");
    return static_cast<const IDTableEntry *>(TheData);
  }

  [[nodiscard]] bool isOneOf(const tok::Tag T1, const tok::Tag T2) const {
    return is(T1) || is(T2);
  }

  template<typename... Ts> bool isOneOf(const tok::Tag T1, Ts... Tss) const {
    return is(T1) || isOneOf(Tss...);
  }

  [[nodiscard]] inline bool isIdentifier() const {
    return LexicalTag == tok::Tag::identifier;
  }

  [[nodiscard]] bool isPunctuator() const {
    return LexicalTag < tok::Tag::last_punc && LexicalTag > tok::Tag::last_tok;
  }

  [[nodiscard]] bool isKeyword() const {
    return LexicalTag > tok::last_punc && LexicalTag < tok::kw_last_keyword;
  }

  [[nodiscard]] bool isLiteral() const {
    return LexicalTag == tok::floating_constant
        || LexicalTag == tok::numeric_constant
        || LexicalTag == tok::string_literal;
  }

  static bool isLiteral(const tok::Tag Kind) {
    return Kind == tok::floating_constant || Kind == tok::numeric_constant
        || Kind == tok::string_literal;
  }

  [[nodiscard]] bool isBaseType() const {
    return LexicalTag < tok::Tag::kw_last_type
        && LexicalTag > tok::Tag::last_punc;
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

  [[nodiscard]] llvm::SMLoc getLoc() const { return LocStart; }

  [[nodiscard]] llvm::SMLoc getRightmostLoc() const { return LocEnd; }

  [[nodiscard]] llvm::SMRange getLocRange() const { return {LocStart, LocEnd}; }

  [[nodiscard]] bool is(const tok::Tag K) const { return K == LexicalTag; }
};
}// namespace funLang
