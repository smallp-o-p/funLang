//
// Created by will on 10/5/24.
//
module;
#include <cassert>
export module Lex;
export import :Token;
import Basic;
import Diag;

namespace funLang {
using namespace Basic;
export class Lexer {
  friend class SymbolTable;
  // keep track of old tokens for error messages
  std::vector<Token> Tokens{};
  // unconsumed tokens that are stored if we looked ahead.
  std::deque<Token> UnconsumedTokens{};
  llvm::StringMap<tok::Tag> KeywordTable{};

  DiagEngine &Diagnostics;
  llvm::StringRef CurrentBuffer;
  llvm::StringRef::iterator BufferPtr;
  SymbolTable IdentTable{};
  std::size_t BufferID = 0;

  constexpr void addKeywords() {
#define KEYWORD(NAME, FLAGS) addKeyword(#NAME, Basic::tok::kw_##NAME);
#include "Basic/defs/TokenTags.def"
  }

  Token formToken(const char *TokEnd, tok::Tag Kind) {
    const auto Current = BufferPtr;
    const auto Lexed =
        llvm::StringRef(Current, static_cast<std::size_t>(TokEnd - Current));
    BufferPtr = TokEnd;
    if (Kind == tok::identifier) {
      auto Key = IdentTable.get(Lexed);
      if (!Key) {
        Key = IdentTable.insert(Lexed);
      }
      assert(Key && "Couldn't insert into symbol table!");

      return {Kind, Key, llvm::SMLoc::getFromPointer(Current),
              llvm::SMLoc::getFromPointer(TokEnd), Lexed.size()};
    }
    if (Token::isLiteral(Kind)) {
      return {Lexed, static_cast<std::size_t>(TokEnd - Current), Kind,
              llvm::SMLoc::getFromPointer(Current),
              llvm::SMLoc::getFromPointer(TokEnd)};
    }
    return {Kind, llvm::SMLoc::getFromPointer(Current),
            llvm::SMLoc::getFromPointer(TokEnd),
            static_cast<std::size_t>(TokEnd - Current)};
  }
  Token formErr() {
    BufferPtr++;
    return Token(llvm::SMLoc::getFromPointer(BufferPtr - 1));
  }

  constexpr void addKeyword(const llvm::StringRef Kw, tok::Tag Tag) {
    KeywordTable.insert(std::make_pair(Kw, Tag));
  }

  tok::Tag findKeyword(const llvm::StringRef Name) {
    if (const auto Result = KeywordTable.find(Name);
        Result != KeywordTable.end()) {
      return Result->second;
    }
    return tok::Tag::identifier;
  }

  Token lexString() {
    auto End = BufferPtr + 1;
    while (*End && *End++ != '\"') {}
    if (!*End) {
      Diagnostics.emitDiagMsg(getLocFrom(BufferPtr),
                              Diag::err_unterminated_char_or_string);
      return formErr();
    }
    return formToken(End, tok::string_literal);
  }

  Token lexNum() {
    auto End = BufferPtr;
    bool SeenDot = false;
    while (*End && (std::isdigit(*End) || *End == '.')) {
      if (SeenDot && *End == '.') {
        Diagnostics.emitDiagMsg(getLocFrom(End + 1), Diag::err_unexpected_char,
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
      return formToken(End, tok::Tag::floating_constant);
    }
    return formToken(End, tok::Tag::numeric_constant);
  }

  Token lexIdentifier() {
    auto End = BufferPtr;

    while (std::isalnum(*End) || *End == '_') {
      End++;
    }
    return formToken(End, findKeyword(std::string(BufferPtr, End)));
  }

  Token getNext() {
    while (*BufferPtr && std::iswspace(*BufferPtr)) {
      ++BufferPtr;
    }
    switch (*BufferPtr) {
    case ',': return formToken(BufferPtr + 1, tok::Tag::comma);
    case '{': return formToken(BufferPtr + 1, tok::l_brace);
    case '}': return formToken(BufferPtr + 1, tok::r_brace);
    case '(': return formToken(BufferPtr + 1, tok::Tag::l_paren);
    case ')': return formToken(BufferPtr + 1, tok::Tag::r_paren);
    case '=':
      return nextIs('=') ? formToken(BufferPtr + 2, tok::Tag::equalequal)
                         : formToken(BufferPtr + 1, tok::Tag::equal);
    case '.':
      return nextIs('.') ? formToken(BufferPtr + 2, tok::Tag::dotdot)
                         : formToken(BufferPtr + 1, tok::Tag::dot);
    case ':':
      return nextIs(':') ? formToken(BufferPtr + 2, tok::Tag::coloncolon)
                         : formToken(BufferPtr + 1, tok::Tag::colon);
    case ';': return formToken(BufferPtr + 1, tok::Tag::semi);
    case '!': {
      if (nextIs('=')) {
        return formToken(BufferPtr + 2, tok::Tag::exclaimequal);
      }
      return formToken(BufferPtr + 1, tok::Tag::exclaim);
    }
    case '<': {
      if (nextIs('=')) {
        return formToken(BufferPtr + 2, tok::Tag::lessequal);
      }
      return formToken(BufferPtr + 1, tok::Tag::less);
    }
    case '>': {
      if (nextIs('=')) {
        return formToken(BufferPtr + 2, tok::Tag::greaterequal);
      }
      return formToken(BufferPtr + 1, tok::Tag::greater);
    }
    case '+':
      if (nextIs('=')) {
        return formToken(BufferPtr + 2, tok::Tag::plusequal);
      }
      return nextIs('+') ? formToken(BufferPtr + 2, tok::Tag::plusplus)
                         : formToken(BufferPtr + 1, tok::Tag::plus);
    case '-': {
      if (nextIs('=')) {
        return formToken(BufferPtr + 2, tok::Tag::minusequal);
      }
      if (nextIs('-')) {
        return formToken(BufferPtr + 2, tok::Tag::minusminus);
      }
      return formToken(BufferPtr + 1, tok::Tag::minus);
    }
    case '*': {
      return nextIs('=') ? formToken(BufferPtr + 2, tok::Tag::starequal)
                         : formToken(BufferPtr + 1, tok::Tag::star);
    }
    case '/': {
      return nextIs('=') ? formToken(BufferPtr + 2, tok::Tag::slashequal)
                         : formToken(BufferPtr + 1, tok::Tag::slash);
    }
    case '\"': return lexString();
    default: {
      if (std::isdigit(*BufferPtr)) {
        return lexNum();
      }
      if (std::isalpha(*BufferPtr)) {
        return lexIdentifier();
      }
      if (!*BufferPtr) {
        return formToken(BufferPtr, tok::Tag::eof);
      }
      Diagnostics.emitDiagMsg(getCurLoc(), Diag::err_unexpected_char,
                              *BufferPtr);
      return formErr();
    }
    }
  }

  [[nodiscard]] bool nextIs(const char C) const {
    if (*(BufferPtr + 1) == C) {
      return true;
    }
    return false;
  }

public:
  Lexer(const llvm::SourceMgr &SrcMgr, DiagEngine &Diags)
      : Diagnostics(Diags), BufferID(SrcMgr.getMainFileID()) {
    CurrentBuffer = SrcMgr.getMemoryBuffer(BufferID)->getBuffer();
    BufferPtr = CurrentBuffer.begin();
    addKeywords();
  }

  [[nodiscard]] llvm::SMLoc getCurLoc() const {
    return llvm::SMLoc::getFromPointer(BufferPtr);
  }

  static llvm::SMLoc getLocFrom(const char *Ptr) {
    return llvm::SMLoc::getFromPointer(Ptr - 1);
  }

  /**
        Consume the next token and return it. Check for any retrieved but unconsumed
        tokens.
      */
  Token &advance() {
    const Token Tok =
        UnconsumedTokens.empty() ? getNext() : UnconsumedTokens.front();
    if (!UnconsumedTokens.empty()) {
      UnconsumedTokens.pop_front();
    }
    Tokens.push_back(Tok);
    return Tokens.back();
  }

  /**
      Lookahead of 1 token.
      */
  Token &peek() {
    if (UnconsumedTokens.empty()) {
      UnconsumedTokens.push_back(getNext());
    }
    return UnconsumedTokens.front();
  }

  /**
      Lookahead of n tokens.
      */
  Token lookahead(std::size_t HowMuch) {
    while (HowMuch > UnconsumedTokens.size()) {
      UnconsumedTokens.push_back(getNext());
    }
    return UnconsumedTokens.at(HowMuch - 1);
  }
  [[nodiscard]] Token previous() const { return Tokens.back(); }
  [[nodiscard]] bool atEnd() const { return !*BufferPtr; }
};
}// namespace funLang
