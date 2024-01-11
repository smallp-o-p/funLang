#include "Lex.hpp"
#include "Parse.hpp"
#include "gtest/gtest.h"

TEST(ParseTesting, ExpFunc) {
  EXPECT_EQ(initInp("./FnsParseTest.txt"), 0);

  EXPECT_EQ(initVec(), 0);

  EXPECT_NE(func(), nullptr);
  closeInp();
  cleanup();
}

TEST(ParseTesting, FuncWArgs) {
  EXPECT_EQ(initInp("./FnsParseTest2.txt"), 0);
  EXPECT_EQ(initVec(), 0);
  EXPECT_NE(func(), nullptr);
  closeInp();
}
int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
