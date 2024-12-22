//
// Created by will on 12/21/24.
//
import Sema;
#include <gtest/gtest.h>

using namespace funLang;

class ScopeTests : public testing::Test {
protected:
  Scope S{};

  ScopeTests() = default;
};

TEST_F(ScopeTests, TestIteratorBeginInGlobal) { EXPECT_TRUE(&*S.begin() == &S); }
TEST_F(ScopeTests, TestIteratorEndInGlobal) { EXPECT_TRUE(&*S.end() == S.getParent()); }
TEST_F(ScopeTests, TestLoopScopeInGlobal) { EXPECT_EQ(S.getClosestLoopScope(), nullptr); }
TEST_F(ScopeTests, TestFunctionTypePtrInGlobal) { EXPECT_EQ(S.getFunctionReturnType(), nullptr); }
