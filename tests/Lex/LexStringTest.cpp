#include "Lex.hpp"
#include <deque>
#include <iostream>
int test4() {
  std::deque<Tok::Token> string_toks{Tok::STRINGLIT, Tok::STRINGLIT,
                                     Tok::STRINGLIT, Tok::STRINGLIT,
                                     Tok::STRINGLIT, Tok::ERR};

  if (initInp("lex_test4.txt") != 0) {
    return 1;
  }
  TokValCat currentTok;

  while ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    if (currentTok.syntactic_category != string_toks.front()) {
      std::cout << "Failed" << std::endl;
      return 1;
    }
    std::cout << "Passed" << std::endl;
    std::cout << currentTok.lexeme << std::endl;
    string_toks.pop_front();
  }
  return 0;
}

int main() { return test4(); }
