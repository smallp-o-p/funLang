#include "Lex.hpp"
#include "Parse.hpp"

int test1() {
  initInp("./FnsParseTest.txt");

  if (initVec() == 1) {
    return 1;
  };

  if (!func()) {
    return 1;
  }

  return 0;
}

int test2() { return 0; };

int main() {
  if (test1() == 1) {
    return 1;
  }
  if (test2() == 1) {
    return 1;
  }

  return 0;
}
