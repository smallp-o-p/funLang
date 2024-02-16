#include "Lex.hpp"
#include "gtest/gtest.h"
#include <deque>
#include <iostream>
#include <string>

// file should get copied over to executable directory
//+-*/>< =(){}!,:;
//<=
//>=
//==
//!=
using namespace Lexer;
TEST(LexTests, LexBasicToken) {
  std::deque<Tag> tok_queue{
      Tag::PLUS,   Tag::MINUS,  Tag::MULT,   Tag::DIV,    Tag::GTCMP,
      Tag::LTCMP,  Tag::EQ,     Tag::LPAREN, Tag::RPAREN, Tag::LCURLY,
      Tag::RCURLY, Tag::BANG,   Tag::COMMA,  Tag::COLON,  Tag::SEMI,
      Tag::LTECMP, Tag::GTECMP, Tag::EQCMP,  Tag::NECMP,  Tag::ENDFILE};
  std::unique_ptr<std::vector<LexerToken>> scanned =
      std::move(lex("lex_Basic.txt"));
  EXPECT_NE(scanned, nullptr);
  for (LexerToken tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, tok_queue.front());
    tok_queue.pop_front();
  }
}

TEST(LexTests, LexKeywords) {
  std::deque<Tag> tok_queue{Tag::VOID,   Tag::BOOL, Tag::CHAR,  Tag::STRING,
                            Tag::I32,    Tag::I64,  Tag::F32,   Tag::F64,
                            Tag::RETURN, Tag::TRUE, Tag::FALSE, Tag::ENDFILE};

  std::unique_ptr<std::vector<LexerToken>> scanned =
      std::move(lex("lex_Keywords.txt"));
  EXPECT_NE(scanned, nullptr);
  for (LexerToken tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, tok_queue.front());
    tok_queue.pop_front();
  }
}

TEST(LexTests, IntNumLiterals) {
  std::unique_ptr<std::vector<LexerToken>> scanned =
      std::move(lex("lex_IntNumLits.txt"));
  EXPECT_NE(scanned, nullptr);
  scanned->pop_back();
  for (LexerToken tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, Tag::NUM);
  }
}

TEST(LexTests, RealNumLiterals) {
  LexerToken currentTok;
  std::unique_ptr<std::vector<LexerToken>> scanned =
      std::move(lex("lex_RealNumLits.txt"));
  EXPECT_NE(scanned, nullptr);
  scanned->pop_back();
  for (LexerToken tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, Tag::POINTNUM);
  }
}

TEST(LexTests, StringLits) {
  std::deque<Tag> tok_queue{Tag::STRINGLIT, Tag::STRINGLIT, Tag::STRINGLIT,
                            Tag::STRINGLIT, Tag::STRINGLIT, Tag::STRINGLIT,
                            Tag::ENDFILE};
  std::unique_ptr<std::vector<LexerToken>> scanned =
      std::move(lex("lex_StringLits.txt"));
  EXPECT_NE(scanned, nullptr);
  for (LexerToken tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, tok_queue.front());
    tok_queue.pop_front();
  }
}

TEST(LexTests, ExampleFunc) {
  std::deque<Tag> toks = {
      Tag::I32,        Tag::IDENTIFIER, Tag::LPAREN, Tag::I32,
      Tag::IDENTIFIER, Tag::COMMA,      Tag::I32,    Tag::IDENTIFIER,
      Tag::RPAREN,     Tag::LCURLY,     Tag::STRING, Tag::IDENTIFIER,
      Tag::EQ,         Tag::STRINGLIT,  Tag::RETURN, Tag::NUM,
      Tag::SEMI,       Tag::RCURLY,     Tag::ENDFILE};
  std::unique_ptr<std::vector<LexerToken>> scanned =
      lex("./lex_ExampleFunc.txt");
  EXPECT_NE(scanned, nullptr);
  for (LexerToken tok : *scanned) {
    EXPECT_EQ(tok.syntactic_category, toks.front())
        << "Failed at " << tok.lexeme << std::endl;
    std::cout << "Passed" << std::endl;
    toks.pop_front();
  }
}

TEST(LexTests, StringMode) {
  std::deque<Tag> toks = {Tag::I32,  Tag::VOID,   Tag::BOOL,
                          Tag::CHAR, Tag::STRING, Tag::I64,
                          Tag::F32,  Tag::F64,    Tag::ENDFILE};
  auto scanned = lex("i32 void bool char string i64 f32 f64", true);

  EXPECT_NE(scanned, nullptr);
  for (LexerToken tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, toks.front())
        << "Failed at " << tok.lexeme << std::endl;
    toks.pop_front();
  }
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
