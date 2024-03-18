#include "gtest/gtest.h"

TEST(ParseTesting, ExpFunc) {
  // auto parser_ptr = Parser::init("./FnsParseTest.txt");
  // auto parsed = parser_ptr->function();
  SUCCEED();
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
