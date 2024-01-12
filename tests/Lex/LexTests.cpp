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
  EXPECT_EQ(0, initInp("lex_Basic.txt"));
  std::deque<Tok::Token> tok_queue{
      Tok::PLUS,   Tok::MINUS,  Tok::MULT,   Tok::DIV,    Tok::GTCMP,
      Tok::LTCMP,  Tok::EQ,     Tok::LPAREN, Tok::RPAREN, Tok::LCURLY,
      Tok::RCURLY, Tok::BANG,   Tok::COMMA,  Tok::COLON,  Tok::SEMI,
      Tok::LTECMP, Tok::GTECMP, Tok::EQCMP,  Tok::NECMP};
  TokValCat currentTok;
  while ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    EXPECT_EQ(currentTok.syntactic_category, tok_queue.front());
    tok_queue.pop_front();
  }
  closeInp();
}

TEST(LexTests, LexKeywords) {
  std::deque<Tok::Token> toks_to_recognize{
      Tok::VOID, Tok::BOOL, Tok::CHAR,   Tok::STRING, Tok::I32,  Tok::I64,
      Tok::F32,  Tok::F64,  Tok::RETURN, Tok::TRUE,   Tok::FALSE};

  EXPECT_EQ(0, initInp("lex_Keywords.txt"));
  TokValCat currentTok;
  while ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    EXPECT_EQ(currentTok.syntactic_category, toks_to_recognize.front())
        << "Failed at " << currentTok.lexeme << std::endl;
    std::cout << "Pass" << std::endl;
    toks_to_recognize.pop_front();
  }
  closeInp();
}

TEST(LexTests, IntNumLiterals) {
  EXPECT_EQ(initInp("./lex_IntNumLits.txt"), 0);
  TokValCat currentTok;

  while ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    EXPECT_EQ(currentTok.syntactic_category, Tok::NUM)
        << "Did not recognize " << currentTok.lexeme << "as a number";
  }
  closeInp();
}

TEST(LexTests, RealNumLiterals) {
  EXPECT_EQ(initInp("lex_RealNumLits.txt"), 0);
  TokValCat currentTok;

  while ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    EXPECT_EQ(currentTok.syntactic_category, Tok::POINTNUM)
        << "Did not recognize " << currentTok.lexeme << "as a number";
  }
  closeInp();
}

TEST(LexTests, StringLits) {
  std::deque<Tok::Token> string_toks{Tok::STRINGLIT, Tok::STRINGLIT,
                                     Tok::STRINGLIT, Tok::STRINGLIT,
                                     Tok::STRINGLIT, Tok::ERR};

  EXPECT_EQ(initInp("lex_StringLits.txt"), 0);
  TokValCat currentTok;

  while ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    EXPECT_EQ(currentTok.syntactic_category, string_toks.front()) << "Fail";
    std::cout << currentTok.lexeme << std::endl;
    string_toks.pop_front();
  }
  closeInp();
}

TEST(LexTests, ExampleFunc) {
  std::deque<Tok::Token> toks = {
      Tok::I32,        Tok::IDENTIFIER, Tok::LPAREN, Tok::I32,
      Tok::IDENTIFIER, Tok::COMMA,      Tok::I32,    Tok::IDENTIFIER,
      Tok::RPAREN,     Tok::LCURLY,     Tok::STRING, Tok::IDENTIFIER,
      Tok::EQ,         Tok::STRINGLIT,  Tok::RETURN, Tok::NUM,
      Tok::SEMI,       Tok::RCURLY,     Tok::ENDFILE};
  TokValCat tok;
  std::unique_ptr<std::vector<TokValCat>> scanned =
      lex("./lex_ExampleFunc.txt");
  EXPECT_NE(scanned, nullptr);

  for (TokValCat tok : *scanned) {

    EXPECT_EQ(tok.syntactic_category, toks.front())
        << "Failed at " << tok.lexeme << std::endl;
    std::cout << "Passed" << std::endl;
    toks.pop_front();
  }
  closeInp();
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
