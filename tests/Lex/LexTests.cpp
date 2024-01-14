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

TEST(LexTests, LexBasicToken) {
  std::deque<Tok::Token> tok_queue{
      Tok::PLUS,   Tok::MINUS,  Tok::MULT,   Tok::DIV,    Tok::GTCMP,
      Tok::LTCMP,  Tok::EQ,     Tok::LPAREN, Tok::RPAREN, Tok::LCURLY,
      Tok::RCURLY, Tok::BANG,   Tok::COMMA,  Tok::COLON,  Tok::SEMI,
      Tok::LTECMP, Tok::GTECMP, Tok::EQCMP,  Tok::NECMP,  Tok::ENDFILE};
  std::unique_ptr<std::vector<TokValCat>> scanned =
      std::move(lex("lex_Basic.txt"));
  EXPECT_NE(scanned, nullptr);
  for (TokValCat tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, tok_queue.front());
    tok_queue.pop_front();
  }
}

TEST(LexTests, LexKeywords) {
  std::deque<Tok::Token> tok_queue{
      Tok::VOID, Tok::BOOL, Tok::CHAR,   Tok::STRING, Tok::I32,   Tok::I64,
      Tok::F32,  Tok::F64,  Tok::RETURN, Tok::TRUE,   Tok::FALSE, Tok::ENDFILE};

  std::unique_ptr<std::vector<TokValCat>> scanned =
      std::move(lex("lex_Keywords.txt"));
  EXPECT_NE(scanned, nullptr);
  for (TokValCat tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, tok_queue.front());
    tok_queue.pop_front();
  }
}

TEST(LexTests, IntNumLiterals) {
  std::unique_ptr<std::vector<TokValCat>> scanned =
      std::move(lex("lex_IntNumLits.txt"));
  EXPECT_NE(scanned, nullptr);
  scanned->pop_back();
  for (TokValCat tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, Tok::NUM);
  }
}

TEST(LexTests, RealNumLiterals) {
  TokValCat currentTok;
  std::unique_ptr<std::vector<TokValCat>> scanned =
      std::move(lex("lex_RealNumLits.txt"));
  EXPECT_NE(scanned, nullptr);
  scanned->pop_back();
  for (TokValCat tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, Tok::POINTNUM);
  }
}

TEST(LexTests, StringLits) {
  std::deque<Tok::Token> tok_queue{
      Tok::STRINGLIT, Tok::STRINGLIT, Tok::STRINGLIT, Tok::STRINGLIT,
      Tok::STRINGLIT, Tok::STRINGLIT, Tok::ENDFILE};
  std::unique_ptr<std::vector<TokValCat>> scanned =
      std::move(lex("lex_StringLits.txt"));
  EXPECT_NE(scanned, nullptr);
  for (TokValCat tok : *scanned) {
    std::cout << tok.lexeme << std::endl;
    EXPECT_EQ(tok.syntactic_category, tok_queue.front());
    tok_queue.pop_front();
  }
}

TEST(LexTests, ExampleFunc) {
  std::deque<Tok::Token> toks = {
      Tok::I32,        Tok::IDENTIFIER, Tok::LPAREN, Tok::I32,
      Tok::IDENTIFIER, Tok::COMMA,      Tok::I32,    Tok::IDENTIFIER,
      Tok::RPAREN,     Tok::LCURLY,     Tok::STRING, Tok::IDENTIFIER,
      Tok::EQ,         Tok::STRINGLIT,  Tok::RETURN, Tok::NUM,
      Tok::SEMI,       Tok::RCURLY,     Tok::ENDFILE};
  std::unique_ptr<std::vector<TokValCat>> scanned =
      lex("./lex_ExampleFunc.txt");
  EXPECT_NE(scanned, nullptr);
  for (TokValCat tok : *scanned) {
    EXPECT_EQ(tok.syntactic_category, toks.front())
        << "Failed at " << tok.lexeme << std::endl;
    std::cout << "Passed" << std::endl;
    toks.pop_front();
  }
}

TEST(LexTests, StringMode) {
  std::deque<Tok::Token> toks = {Tok::I32,  Tok::VOID,   Tok::BOOL,
                                 Tok::CHAR, Tok::STRING, Tok::I64,
                                 Tok::F32,  Tok::F64,    Tok::ENDFILE};
  auto scanned = lex("i32 void bool char string i64 f32 f64", true);

  EXPECT_NE(scanned, nullptr);
  for (TokValCat tok : *scanned) {
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
