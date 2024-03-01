#include "Lex.hpp"
#include "TokenTags.hpp"
#include "gtest/gtest.h"
#include <iostream>
// file should get copied over to executable directory
//+-*/>< =(){}!,:;
//<=
//>=
//==
//!=
TEST(LexTests, LexBasicToken) {
  using namespace Basic;
  auto lex = Lexer::init("./lex_Basic.txt");
  EXPECT_NE(lex, nullptr) << "Lexer init returned nullptr.";

  std::vector<Basic::tok::Tag> expected = {
      tok::plus,         tok::minus,      tok::star,        tok::slash,
      tok::greater,      tok::less,       tok::equal,       tok::l_paren,
      tok::r_paren,      tok::l_brace,    tok::r_brace,     tok::exclaim,
      tok::comma,        tok::colon,      tok::semi,        tok::lessequal,
      tok::greaterequal, tok::equalequal, tok::exclaimequal};

  for (auto expected_tok : expected) {
    EXPECT_EQ(expected_tok, lex->advance().getTag())
        << "Mismatch; Expected: " << tok::getTokenName(expected_tok)
        << "\nReceived: " << tok::getTokenName(lex->previous().getTag());
  }
}

TEST(LexTests, LexKeywords) {
  using namespace Basic;
  auto lex = Lexer::init("./lex_Keywords.txt");
  EXPECT_NE(lex, nullptr) << "Lexer init returned nullptr.";
  std::vector<Basic::tok::Tag> expected = {
      tok::kw_void,   tok::kw_bool, tok::kw_char, tok::kw_string,
      tok::kw_i32,    tok::kw_i64,  tok::kw_f32,  tok::kw_f64,
      tok::kw_return, tok::kw_true, tok::kw_false};
  for (auto expected_tok : expected) {
    EXPECT_EQ(expected_tok, lex->advance().getTag())
        << "Mismatch; Expected: " << tok::getTokenName(expected_tok)
        << "\nReceived: " << tok::getTokenName(lex->previous().getTag());
  }
}

TEST(LexTests, IntNumLiterals) {
  using namespace Basic;
  auto lex = Lexer::init("./lex_IntNumLits.txt");
  EXPECT_NE(lex, nullptr) << "Lexer init returned nullptr.";
  while (lex->advance().getTag() != tok::eof) {
    EXPECT_EQ(tok::numeric_constant, lex->previous().getTag())
        << "Mismatch; Expected: " << tok::getTokenName(tok::numeric_constant)
        << "\nReceived: " << tok::getTokenName(lex->previous().getTag());
  }
  EXPECT_EQ(tok::eof, lex->previous().getTag());
}

TEST(LexTests, RealNumLiterals) {
  using namespace Basic;
  auto lex = Lexer::init("./lex_RealNumLits.txt");
  EXPECT_NE(lex, nullptr) << "Lexer init returned nullptr.";
  while (lex->advance().getTag() != tok::eof) {
    EXPECT_EQ(tok::floating_constant, lex->previous().getTag())
        << "Mismatch; Expected: " << tok::getTokenName(tok::floating_constant)
        << "\nReceived: " << tok::getTokenName(lex->previous().getTag());
  }
  EXPECT_EQ(tok::eof, lex->previous().getTag());
}

TEST(LexTests, StringLits) {
  using namespace Basic;
  auto lex = Lexer::init("./lex_StringLits.txt");
  EXPECT_NE(lex, nullptr) << "Lexer init returned nullptr.";
  while (lex->advance().getTag() != tok::eof) {
    EXPECT_EQ(tok::string_literal, lex->previous().getTag())
        << "Mismatch; Expected: " << tok::getTokenName(tok::string_literal)
        << "\nReceived: " << tok::getTokenName(lex->previous().getTag());
  }
  EXPECT_EQ(tok::eof, lex->previous().getTag());
}

TEST(LexTests, ExampleFunc) {
  using namespace Basic;
  auto lex = Lexer::init("./lex_ExampleFunc.txt");
  EXPECT_NE(lex, nullptr) << "Lexer init returned nullptr.";
  std::vector<Basic::tok::Tag> expected = {
      tok::kw_i32,    tok::identifier,       tok::l_paren,
      tok::kw_i32,    tok::identifier,       tok::comma,
      tok::kw_i32,    tok::identifier,       tok::r_paren,
      tok::l_brace,   tok::kw_string,        tok::identifier,
      tok::equal,     tok::string_literal,   tok::semi,
      tok::kw_return, tok::numeric_constant, tok::semi,
      tok::r_brace};
  for (auto expected_tok : expected) {
    EXPECT_EQ(expected_tok, lex->advance().getTag())
        << "Mismatch; Expected: " << tok::getTokenName(expected_tok)
        << "\nReceived: " << tok::getTokenName(lex->previous().getTag());
  }
}

TEST(LexTests, StringMode) { SUCCEED(); }

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
