#include "Lex.hpp"
#include <iostream>
#include <ostream>

int test2() {

  if (initInp("lex_test2.txt") != 0) {
    return 1;
  }
  int iteration = 1;
  TokValCat currentTok;

  while ((currentTok = getNextTok()).syntactic_category != Tok::ENDFILE) {
    if (currentTok.syntactic_category != Tok::NUM) {
      std::cout << "Failed to recognize number " << iteration << " as number"
                << std::endl;
      return 1;
    }
    iteration++;
  }
  closeInp();
  return 0;
}

int main() { return test2(); }
