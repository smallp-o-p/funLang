#include "Lex.hpp"
#include <deque>
#include <filesystem>
#include <iostream>
#include <string>
// file should get copied over to executable directory
//+-*/>< =(){}!
//<=
//>=
//==
//!=
//
int test1() {
  std::cout << "Current path is " << std::filesystem::current_path()
            << std::endl;
  if (initInp("lex_test1.txt") != 0) {
    return 1;
  }
  std::deque<Tok::Token> tok_queue{
      Tok::PLUS,   Tok::MINUS,  Tok::MULT,   Tok::DIV,
      Tok::GTCMP,  Tok::LTCMP,  Tok::EQ,     Tok::LPAREN,
      Tok::RPAREN, Tok::LCURLY, Tok::RCURLY, Tok::BANG,
      Tok::LTECMP, Tok::GTECMP, Tok::EQCMP,  Tok::NECMP};
  TokValCat currentTok;

  while ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    if (tok_queue.front() != currentTok.syntactic_category) {
      std::cout << "Mismatch between " << currentTok.lexeme
                << " and current front of queue" << std::endl;
      return 1;
    }
    std::cout << "Recognized " << currentTok.lexeme << std::endl;
    tok_queue.pop_front();
  }
  closeInp();

  return 0;
}
int main() { return test1(); }
