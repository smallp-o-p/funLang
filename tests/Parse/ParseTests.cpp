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

TEST(ParseTesting, FuncWError) {
  EXPECT_EQ(initInstance("./FnsParseTest4.fun"), 0);
  EXPECT_EQ(program(), nullptr); // there is a syntax error in this file
  // call returnStringLit
  //                      ^^^^^ missing parentheses
}

TEST(ParseTesting, FuncWExprs) {
  EXPECT_EQ(initInstance("./FnsParseTest5.fun"), 0);
  EXPECT_NE(program(), nullptr); // same file as num 4 but syntactically correct
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
