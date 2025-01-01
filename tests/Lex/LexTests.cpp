#include "gtest/gtest.h"
#include <filesystem>
#include <iostream>
#include <llvm/Support/SMLoc.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>
#ifndef BASIC
#error "Basic file not defined!"
#endif
#ifndef INTNUMLITS
#error
#endif
#ifndef KEYWORDS
#error
#endif
#ifndef REALNUMLITS
#error
#endif
#ifndef STRINGLITS
#error
#endif
#ifndef ERRORS
#error
#endif
#ifndef FUNC
#error
#endif
import Basic;
import Lex;
import Diag;
using namespace funLang;
using namespace Basic;

class LexTests : public testing::Test {
protected:
  Lexer *LexerObj{};
  DiagEngine Diags{SrcMgr};
  llvm::SourceMgr SrcMgr;
  bool setBuffer(const llvm::StringRef Buf, const bool UsingString = false) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrError = nullptr;
    FileOrError = UsingString ? llvm::MemoryBuffer::getMemBufferCopy(Buf)
                              : llvm::MemoryBuffer::getFile(Buf);
    if (const std::error_code BufferErr = FileOrError.getError()) {
      std::cout << BufferErr.message() << " " << BufferErr.value() << std::endl;
      return false;
    }
    SrcMgr.AddNewSourceBuffer(std::move(*FileOrError), llvm::SMLoc());
    LexerObj = new Lexer(SrcMgr, Diags);
    return true;
  }
  LexTests() : SrcMgr(llvm::SourceMgr()) {}
};

// file should get copied over to executable directory
//+-*/>< =(){}!,:;
//<=
//>=
//==
//!=
TEST_F(LexTests, LexBasicTokens) {
  ASSERT_TRUE(setBuffer(BASIC));
  std::vector expected = {
      tok::plus,         tok::minus,      tok::star,         tok::slash,
      tok::greater,      tok::less,       tok::equal,        tok::l_paren,
      tok::r_paren,      tok::l_brace,    tok::r_brace,      tok::exclaim,
      tok::comma,        tok::colon,      tok::semi,         tok::lessequal,
      tok::greaterequal, tok::equalequal, tok::exclaimequal, tok::plusequal,
      tok::minusequal,   tok::starequal,  tok::slashequal};
  for (auto tok : expected) {
    Token lexed_token = LexerObj->advance();
    EXPECT_EQ(lexed_token.getTag(), tok)
        << "Expected: " << getTokenName(tok)
        << " Received: " << getTokenName(lexed_token.getTag());
  }
  EXPECT_EQ(LexerObj->advance().getTag(), tok::eof);
}

TEST_F(LexTests, LexKeywords) {
  std::vector expected = {
      tok::kw_void,   tok::kw_bool, tok::kw_char,   tok::kw_string,
      tok::kw_i32,    tok::kw_i64,  tok::kw_f32,    tok::kw_f64,
      tok::kw_return, tok::kw_true, tok::kw_false,  tok::kw_for,
      tok::kw_while,  tok::kw_loop, tok::kw_struct, tok::kw_enum};
  ASSERT_TRUE(setBuffer(KEYWORDS));
  for (auto expected_tok : expected) {
    Token lexed_token = LexerObj->advance();
    EXPECT_EQ(lexed_token.getTag(), expected_tok)
        << "Expected: " << getTokenName(expected_tok)
        << " Received: " << getTokenName(lexed_token.getTag());
  }
}

TEST_F(LexTests, IntNumLiterals) {
  ASSERT_TRUE(setBuffer(INTNUMLITS));
  Token tok = LexerObj->advance();
  while (tok.getTag() != tok::eof) {
    EXPECT_EQ(tok.getTag(), tok::numeric_constant)
        << "\nExpected: " << getTokenName(tok::numeric_constant)
        << " Received: " << getTokenName(tok.getTag());
    tok = LexerObj->advance();
  }
  EXPECT_EQ(tok.getTag(), tok::eof);
}

TEST_F(LexTests, RealNumLiterals) {
  ASSERT_TRUE(setBuffer(REALNUMLITS));
  Token tok = LexerObj->advance();
  while (tok.getTag() != tok::eof) {
    EXPECT_EQ(tok.getTag(), tok::floating_constant)
        << "\nExpected: " << getTokenName(tok::floating_constant)
        << " Received: " << getTokenName(tok.getTag());
    tok = LexerObj->advance();
  }
  EXPECT_EQ(tok.getTag(), tok::eof);
}

TEST_F(LexTests, StringLits) {
  ASSERT_TRUE(setBuffer(STRINGLITS));

  Token tok = LexerObj->advance();
  while (tok.getTag() != tok::eof) {
    EXPECT_EQ(tok::string_literal, tok.getTag())
        << "\nExpected: " << getTokenName(tok::string_literal)
        << " Received: " << getTokenName(tok.getTag());
    tok = LexerObj->advance();
  }
  EXPECT_EQ(tok::eof, LexerObj->previous().getTag());
}

TEST_F(LexTests, ExampleFunc) {
  ASSERT_TRUE(setBuffer(FUNC));
  std::vector expected = {
      tok::kw_i32,    tok::identifier,       tok::l_paren,
      tok::kw_i32,    tok::identifier,       tok::comma,
      tok::kw_i32,    tok::identifier,       tok::r_paren,
      tok::l_brace,   tok::kw_string,        tok::identifier,
      tok::equal,     tok::string_literal,   tok::semi,
      tok::kw_return, tok::numeric_constant, tok::semi,
      tok::r_brace};
  for (auto expected_tok : expected) {
    EXPECT_EQ(expected_tok, LexerObj->advance().getTag())
        << "Mismatch; Expected: " << getTokenName(expected_tok)
        << "\nReceived: " << getTokenName(LexerObj->previous().getTag());
  }
}

TEST_F(LexTests, WithErrs) {
  ASSERT_TRUE(setBuffer(ERRORS));
  std::vector expected = {tok::err,    tok::err,     tok::err,     tok::err,
                          tok::err,    tok::kw_void, tok::kw_bool, tok::kw_i32,
                          tok::kw_i64, tok::err,     tok::err,     tok::err,
                          tok::err};

  for (auto expected_tok : expected) {
    EXPECT_EQ(expected_tok, LexerObj->advance().getTag())
        << "Mismatch; Expected: " << getTokenName(expected_tok)
        << "\nReceived: " << getTokenName(LexerObj->previous().getTag());
  }
}

TEST_F(LexTests, IdentifierEqualityTest) {
  ASSERT_TRUE(setBuffer("id id", true));
  const auto id = LexerObj->advance();
  const auto id2 = LexerObj->advance();

  ASSERT_EQ(id.getIdentifier(), id2.getIdentifier());
  ASSERT_EQ(id.getIdentifierTableEntry(), id2.getIdentifierTableEntry());
}

TEST_F(LexTests, IdentifierInequalityTest) {
  ASSERT_TRUE(setBuffer("one id two id one", true));
  const auto one = LexerObj->advance();
  const auto id = LexerObj->advance();
  const auto two = LexerObj->advance();
  const auto id2 = LexerObj->advance();
  const auto one2 = LexerObj->advance();
  ASSERT_NE(one.getIdentifierTableEntry(), two.getIdentifierTableEntry());
  ASSERT_NE(id.getIdentifierTableEntry(), one.getIdentifierTableEntry());
  ASSERT_NE(id2.getIdentifierTableEntry(), two.getIdentifierTableEntry());
  ASSERT_EQ(one.getIdentifierTableEntry(), one2.getIdentifierTableEntry());
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
