//
// Created by will on 10/5/24.
//
module;
#include <cassert>
export module Lex:Token;
import Basic;

namespace funLang {
using namespace Basic;
export class Token {
  friend class Lexer;

  tok::Tag LexicalTag{};
  const void *TheData{};
  std::size_t DataSize;
  llvm::SMLoc LocStart{}, LocEnd{};

  // error
  explicit Token(const llvm::SMLoc ErrLoc)
      : LexicalTag(tok::err), DataSize(0), LocStart(ErrLoc),
        LocEnd(llvm::SMLoc()) {}
  // literal
  Token(const llvm::StringRef Lexeme, const std::size_t Size,
        const tok::Tag LexicalTag, const llvm::SMLoc Start,
        const llvm::SMLoc End)
      : LexicalTag(LexicalTag),
        TheData(static_cast<const void *>(Lexeme.data())), DataSize(Size),
        LocStart(Start), LocEnd(End) {}
  // punctuator or keyword
  Token(const tok::Tag Kind, const llvm::SMLoc LocStart,
        const llvm::SMLoc LocEnd, const std::size_t Size = 1)
      : LexicalTag(Kind), DataSize(Size), LocStart(LocStart), LocEnd(LocEnd) {}
  // identifier
  Token(const tok::Tag LexicalTag, const Symbol *IdentifierTableEntry,
        const llvm::SMLoc LocStart, const llvm::SMLoc LocEnd,
        const std::size_t Size)
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
  [[nodiscard]] const char *getIdentifierData() const {
    assert(isIdentifier() && "Not an identifier!");
    return static_cast<const Symbol *>(TheData)->getKeyData();
  }

  [[nodiscard]] std::size_t getIdentifierSize() const {
    assert(isIdentifier() && "Not an identifier!");
    return static_cast<const Symbol *>(TheData)->getKeyLength();
  }

  [[nodiscard]] llvm::StringRef getIdentifier() const {
    assert(isIdentifier() && "Not an identifier!");
    return static_cast<const Symbol *>(TheData)->getKey();
  }

  [[nodiscard]] const Symbol *getIdentifierTableEntry() const {
    assert(isIdentifier() && "Not an identifier!");
    return static_cast<const Symbol *>(TheData);
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

  [[nodiscard]] std::string prettyPrint() const {
    std::string str = "unknown";
    if (isIdentifier()) {
      str = getIdentifier();
    } else if (isLiteral()) {
      str = getLiteral().str();
    } else if (isKeyword() || isPunctuator()) {
      str = getTokenName(LexicalTag);
    }
    std::string fmt =
        llvm::formatv("<{}>: {}", getTokenName(LexicalTag), str).str();
    return fmt;
  }

  [[nodiscard]] llvm::SMLoc getLoc() const { return LocStart; }

  [[nodiscard]] llvm::SMLoc getRightmostLoc() const {
    return llvm::SMLoc::getFromPointer(LocStart.getPointer() + (DataSize - 1));
  }

  [[nodiscard]] llvm::SMRange getLocRange() const {
    return {LocStart, getRightmostLoc()};
  }

  [[nodiscard]] bool is(const tok::Tag K) const { return K == LexicalTag; }
};
}// namespace funLang
