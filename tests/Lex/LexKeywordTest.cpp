#include "Lex.hpp"
#include <deque>
#include <iostream>
int test3() {
  // void bool char String i32 i64 f32 f64 hello world identifier another
  // identifier yay
  std::deque<Tok::Token> toks_to_recognize{
      Tok::VOID,       Tok::BOOL,       Tok::CHAR,       Tok::STRING,
      Tok::I32,        Tok::I64,        Tok::F32,        Tok::F64,
      Tok::IDENTIFIER, Tok::IDENTIFIER, Tok::IDENTIFIER, Tok::IDENTIFIER,
      Tok::IDENTIFIER, Tok::LPAREN,     Tok::RPAREN};

  if (initInp("lex_test3.txt") != 0) {
    return 1;
  }
  TokValCat currentTok;
  while ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    Tok::Token toRecognize = toks_to_recognize.front();
    if (toRecognize != currentTok.syntactic_category) {
      std::cout << "Failed at " << currentTok.lexeme << std::endl;
      return 1;
    }
    std::cout << "Pass" << std::endl;
    toks_to_recognize.pop_front();
  }
  return 0;
}

int main() { return test3(); }
