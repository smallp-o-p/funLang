#include "Lex.hpp"
#include "Parse.hpp"
#include "gtest/gtest.h"

TEST(ParseTesting, ExpFunc) {
  EXPECT_EQ(initInstance("./FnsParseTest.txt"), 0);
  EXPECT_NE(func(), nullptr);
  closeInp();
}

TEST(ParseTesting, FuncWArgs) {
  EXPECT_EQ(initInstance("./FnsParseTest2.txt"), 0);
  EXPECT_NE(func(), nullptr);
  closeInp();
}

TEST(ParseTesting, FuncWLots) {
  EXPECT_EQ(initInstance("./FnsParseTest3.txt"), 0);
  EXPECT_NE(program(), nullptr);
}

int main() {

  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
