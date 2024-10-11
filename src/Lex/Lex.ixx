//
// Created by will on 10/5/24.
//
module;
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormatVariadicDetails.h"
#include "llvm/ADT/StringRef.h"
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
#include <unistd.h>
export module Lex;
export import :Token;
export import Basic;
export import Basic.Diag;
export import Basic.IdentifierTable;

namespace funLang {
  export class Lexer {
    friend class IdentifierTable;

  private:
    std::vector<Token> tokens;   // keep track of old tokens for error messages
    std::deque<Token> unconsumed;// unconsumed tokens that are stored if we looked ahead.
    llvm::StringMap<Basic::tok::Tag> KeywordTable;

    std::shared_ptr<llvm::SourceMgr> srcManager;
    DiagEngine diagnostics;
    llvm::StringRef curBuf;
    llvm::StringRef::iterator bufPtr;
    IdentifierTable IdentTable;
    uint32_t currentBuffer = 0;

    void addKeywords() {
      #define KEYWORD(NAME, FLAGS) addKeyword(#NAME, Basic::tok::kw_##NAME);
      #include "Basic/defs/TokenTags.def"
    }

    Token formToken(const char *TokEnd, Basic::tok::Tag Kind) {
      llvm::StringRef Lexed = llvm::StringRef(bufPtr, static_cast<size_t>(TokEnd - bufPtr));
      if (Kind == Basic::tok::identifier) {
        auto EntryPtr = IdentTable.insert(Lexed);
        Token Tok = {Kind,
                     EntryPtr,
                     llvm::SMLoc::getFromPointer(bufPtr),
                     llvm::SMLoc::getFromPointer(TokEnd),
                     Lexed.size()};
        bufPtr = TokEnd;
        return Tok;
      } else if (Token::isLiteral(Kind)) {
        Token Tok = {Lexed, static_cast<size_t>(TokEnd - bufPtr),
                     Kind, llvm::SMLoc::getFromPointer(bufPtr), llvm::SMLoc::getFromPointer(TokEnd)};
        bufPtr = TokEnd;
        return Tok;
      }
      Token Tok = {Kind, llvm::SMLoc::getFromPointer(bufPtr), llvm::SMLoc::getFromPointer(TokEnd),
                   static_cast<size_t>(TokEnd - bufPtr)};
      bufPtr = TokEnd;
      return Tok;
    }
    Token formErr() {
      bufPtr++;
      return Token(llvm::SMLoc::getFromPointer(bufPtr - 1));
    }

    void addKeyword(const std::string &Kw, Basic::tok::Tag Tag) { KeywordTable.insert(std::make_pair(Kw, Tag)); }

    Basic::tok::Tag findKeyword(llvm::StringRef Name) {
      auto Result = KeywordTable.find(Name);
      if (Result != KeywordTable.end()) {
        return Result->second;
      }
      return Basic::tok::Tag::identifier;
    }

    Token lexString() {
      auto End = bufPtr + 1;
      while (*End && *End++ != '\"') {
      }
      if (!*End) {
        diagnostics.emitDiagMsg(getLocFrom(bufPtr),
                                Diag::err_unterminated_char_or_string);
        return formErr();
      }
      return formToken(End, Basic::tok::string_literal);
    }

    Token lexNum() {
      auto End = bufPtr;// handle negative case
      bool SeenDot = false;
      while (*End && (isdigit(*End) || *End == '.')) {
        if (SeenDot && *End == '.') {
          diagnostics.emitDiagMsg(getLocFrom(End + 1), Diag::err_unexpected_char,
                                  *End);
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
        return formToken(End, Basic::tok::Tag::floating_constant);
      }
      return formToken(End, Basic::tok::Tag::numeric_constant);
    }

    Token lexIdentifier() {
      auto End = bufPtr;

      while (isalnum(*End) || *End == '_') {
        End++;
      }
      return formToken(End, findKeyword(std::string(bufPtr, End)));
    }

    Token getNext() {
      while (*bufPtr && iswspace(*bufPtr)) {
        ++bufPtr;
      }
      switch (*bufPtr) {
      case ',': return formToken(bufPtr + 1, Basic::tok::Tag::comma);
      case '{': return formToken(bufPtr + 1, Basic::tok::l_brace);
      case '}': return formToken(bufPtr + 1, Basic::tok::r_brace);
      case '(': return formToken(bufPtr + 1, Basic::tok::Tag::l_paren);
      case ')': return formToken(bufPtr + 1, Basic::tok::Tag::r_paren);
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
      case ';': return formToken(bufPtr + 1, Basic::tok::Tag::semi);
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
      case '\"': return lexString();
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
          diagnostics.emitDiagMsg(getCurLoc(), Diag::err_unexpected_char, *bufPtr);
          return formErr();
        }
      }
      }
    }

    bool nextIs(char C) {
      if (*(bufPtr + 1) == C) {
        return true;
      }
      return false;
    }

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

    /**
      Consume the next token and return it. Check for any retrieved but unconsumed
      tokens.
    */
    Token &advance() {
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
    Token &peek() {
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
    Token lookahead(size_t HowMuch) {
      while (HowMuch > unconsumed.size()) {
        unconsumed.push_back(getNext());
      }
      return unconsumed.at(HowMuch - 1);
    }
    Token previous() { return tokens.back(); }
    bool atEnd() { return !*bufPtr; }
  };
}
