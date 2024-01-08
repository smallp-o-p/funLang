#include "Lex.hpp"
#include <deque>
int test5() {

  if (initInp("./lex_test5.txt") != 0) {
    return 1;
  };

  std::deque<Tok::Token> toks = {
      Tok::I32,        Tok::IDENTIFIER, Tok::LPAREN, Tok::I32,
      Tok::IDENTIFIER, Tok::COMMA,      Tok::I32,    Tok::IDENTIFIER,
      Tok::RPAREN,     Tok::LCURLY,     Tok::STRING, Tok::IDENTIFIER,
      Tok::EQ,         Tok::STRINGLIT,  Tok::RETURN, Tok::NUM,
      Tok::SEMI,       Tok::RCURLY,     Tok::ENDFILE};
  TokValCat tok;
  std::vector<TokValCat> scanned = lex();
  if (scanned.empty()) {
    std::cout << "Lex returned empty when it should not have." << std::endl;
    return 1;
  }
  for (TokValCat tok : scanned) {
    if (tok.syntactic_category != toks.front()) {
      std::cout << "Failed at " << tok.lexeme << std::endl;
      return 1;
    }
    std::cout << "Passed" << std::endl;
    toks.pop_front();
  }
  return 0;
}

int main() { return test5(); }
