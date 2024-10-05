#include "Lex/Lex.hpp"
#include "gtest/gtest.h"
#include <filesystem>
#include <iostream>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/SMLoc.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>

using namespace funLang;
using namespace Basic;

class LexTests : public testing::Test {
protected:
  Lexer *LexerObj;
  std::shared_ptr<DiagEngine> Diags;
  std::shared_ptr<llvm::SourceMgr> SrcMgr;
  bool setBuffer(llvm::StringRef Buf, bool Str = false) {

    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrError = nullptr;
    if (Str) {
      FileOrError = llvm::MemoryBuffer::getMemBufferCopy(Buf);
    } else {
      FileOrError = llvm::MemoryBuffer::getFile(Buf);
    }
    if (std::error_code BufferErr = FileOrError.getError()) {
      std::cout << BufferErr.message() << " " << BufferErr.value();
      return false;
    }
    SrcMgr->AddNewSourceBuffer(std::move(*FileOrError), llvm::SMLoc());
    Diags = std::make_shared<DiagEngine>(SrcMgr);
    LexerObj = new Lexer(SrcMgr, *Diags);
    return true;
  }
  LexTests()
      : SrcMgr(std::make_shared<llvm::SourceMgr>()), Diags(nullptr), LexerObj(nullptr) {
  }
};

// file should get copied over to executable directory
//+-*/>< =(){}!,:;
//<=
//>=
//==
//!=
TEST_F(LexTests, LexBasicTokens) {
  ASSERT_TRUE(setBuffer("./lex_Basic.txt"));
  std::vector<Basic::tok::Tag> expected = {
      tok::plus, tok::minus, tok::star, tok::slash,
      tok::greater, tok::less, tok::equal, tok::l_paren,
      tok::r_paren, tok::l_brace, tok::r_brace, tok::exclaim,
      tok::comma, tok::colon, tok::semi, tok::lessequal,
      tok::greaterequal, tok::equalequal, tok::exclaimequal, tok::plusequal,
      tok::minusequal, tok::starequal, tok::slashequal};
  for (auto tok : expected) {
    Token lexed_token = LexerObj->advance();
    EXPECT_EQ(lexed_token.getTag(), tok)
        << "Expected: " << tok::getTokenName(tok)
        << " Received: " << tok::getTokenName(lexed_token.getTag());
  }
  EXPECT_EQ(LexerObj->advance().getTag(), tok::eof);
}

TEST_F(LexTests, LexKeywords) {
  std::vector<Basic::tok::Tag> expected = {
      tok::kw_void, tok::kw_bool, tok::kw_char, tok::kw_string,
      tok::kw_i32, tok::kw_i64, tok::kw_f32, tok::kw_f64,
      tok::kw_return, tok::kw_true, tok::kw_false, tok::kw_for,
      tok::kw_while, tok::kw_loop, tok::kw_struct, tok::kw_enum};
  ASSERT_TRUE(setBuffer("./lex_Keywords.txt"));
  for (auto expected_tok : expected) {
    Token lexed_token = LexerObj->advance();
    EXPECT_EQ(lexed_token.getTag(), expected_tok)
        << "Expected: " << tok::getTokenName(expected_tok)
        << " Received: " << tok::getTokenName(lexed_token.getTag());
  }
}

TEST_F(LexTests, IntNumLiterals) {
  ASSERT_TRUE(setBuffer("./lex_IntNumLits.txt"));
  Token tok = LexerObj->advance();
  while (tok.getTag() != tok::eof) {
    EXPECT_EQ(tok.getTag(), tok::numeric_constant)
        << "\nExpected: " << tok::getTokenName(tok::numeric_constant)
        << " Received: " << tok::getTokenName(tok.getTag());
    tok = LexerObj->advance();
  }

  EXPECT_EQ(tok.getTag(), tok::eof);
}

TEST_F(LexTests, RealNumLiterals) {
  ASSERT_TRUE(setBuffer("./lex_RealNumLits.txt"));
  Token tok = LexerObj->advance();
  while (tok.getTag() != tok::eof) {
    EXPECT_EQ(tok.getTag(), tok::floating_constant)
        << "\nExpected: " << tok::getTokenName(tok::floating_constant)
        << " Received: " << tok::getTokenName(tok.getTag());
    tok = LexerObj->advance();
  }
  EXPECT_EQ(tok.getTag(), tok::eof);
}

TEST_F(LexTests, StringLits) {
  ASSERT_TRUE(setBuffer("./lex_StringLits.txt"));

  Token tok = LexerObj->advance();
  while (tok.getTag() != tok::eof) {
    EXPECT_EQ(tok::string_literal, tok.getTag())
        << "\nExpected: " << tok::getTokenName(tok::string_literal)
        << " Received: " << tok::getTokenName(tok.getTag());
    tok = LexerObj->advance();
  }
  EXPECT_EQ(tok::eof, LexerObj->previous().getTag());
}

TEST_F(LexTests, ExampleFunc) {
  ASSERT_TRUE(setBuffer("./lex_ExampleFunc.txt"));
  std::vector<Basic::tok::Tag> expected = {
      tok::kw_i32, tok::identifier, tok::l_paren,
      tok::kw_i32, tok::identifier, tok::comma,
      tok::kw_i32, tok::identifier, tok::r_paren,
      tok::l_brace, tok::kw_string, tok::identifier,
      tok::equal, tok::string_literal, tok::semi,
      tok::kw_return, tok::numeric_constant, tok::semi,
      tok::r_brace};
  for (auto expected_tok : expected) {
    EXPECT_EQ(expected_tok, LexerObj->advance().getTag())
        << "Mismatch; Expected: " << tok::getTokenName(expected_tok)
        << "\nReceived: " << tok::getTokenName(LexerObj->previous().getTag());
  }
}

TEST_F(LexTests, WithErrs) {
  ASSERT_TRUE(setBuffer("./lex_withErrors.txt"));
  std::vector<Basic::tok::Tag> expected = {
      tok::err, tok::err, tok::err,
      tok::err, tok::err, tok::kw_void,
      tok::kw_bool, tok::kw_i32, tok::kw_i64,
      tok::err, tok::err, tok::err, tok::err};

  for (auto expected_tok : expected) {
    EXPECT_EQ(expected_tok, LexerObj->advance().getTag())
        << "Mismatch; Expected: " << tok::getTokenName(expected_tok)
        << "\nReceived: " << tok::getTokenName(LexerObj->previous().getTag());
  }
}

TEST_F(LexTests, IdentifierEqualityTest) {
  ASSERT_TRUE(setBuffer("id id", true));
  auto id = LexerObj->advance();
  auto id2 = LexerObj->advance();

  ASSERT_EQ(id.getIdentifier(), id2.getIdentifier());
  ASSERT_EQ(id.getIdentifierTableEntry(), id2.getIdentifierTableEntry());
}

TEST_F(LexTests, IdentifierInequalityTest) {
  ASSERT_TRUE(setBuffer("one id two id one", true));
  auto one = LexerObj->advance();
  auto id = LexerObj->advance();
  auto two = LexerObj->advance();
  auto id2 = LexerObj->advance();
  auto one2 = LexerObj->advance();
  ASSERT_NE(one.getIdentifierTableEntry(), two.getIdentifierTableEntry());
  ASSERT_NE(id.getIdentifierTableEntry(), one.getIdentifierTableEntry());
  ASSERT_NE(id2.getIdentifierTableEntry(), two.getIdentifierTableEntry());
  ASSERT_EQ(one.getIdentifierTableEntry(), one2.getIdentifierTableEntry());
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
