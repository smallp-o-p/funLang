//
// Created by will on 10/5/24.
//
module;
#include <iostream>
#include <unistd.h>
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"
export module Lex:Token;
import Basic;
import Basic.IdentifierTable;

namespace funLang {
  using namespace Basic;
  export class Token {
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

    llvm::StringRef getLexeme() const {
      if (isLiteral()) {
        return getLiteral();
      } else if (isIdentifier()) {
        return getIdentifier();
      } else if (isPunctuator()) {
        return {Basic::tok::getPunctuatorSpelling(LexicalTag)};
      }
      return Basic::tok::getTokenName(LexicalTag);
    }

    llvm::StringRef getLiteral() const {
      assert(isLiteral() && "Not a literal!");
      return {reinterpret_cast<const char *>(TheData), DataSize};
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
    }

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

    llvm::SMLoc getRightmostLoc() { return LocEnd; }

    llvm::SMRange getLocRange() { return {LocStart, LocEnd}; }

    bool is(Basic::tok::Tag K) const { return K == LexicalTag; }
  };

}



