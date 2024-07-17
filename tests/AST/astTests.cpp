#include "gtest/gtest.h"
#include "AST/AST.hpp"
class ASTTests : public testing::Test {
protected:
  friend class BuiltInType;
  BuiltInType *makeI32Ty() {
	return new BuiltInType(Basic::Data::Type::i32, "i32", 32);
  }
  BuiltInType *makeIntegerLiteral() {
	return new BuiltInType(Basic::Data::int_literal, "integer literal", 64);
  }
  ASTTests() {}
};

TEST_F(ASTTests, BuiltInTypeIsEqualToItself) {
  auto I32 = makeI32Ty();
  ASSERT_TRUE(I32->eq(I32));
}

TEST_F(ASTTests, I32CompatibleWithIntegerLiteral) {
  auto I32 = makeI32Ty();
  auto IntLit = makeIntegerLiteral();
  ASSERT_TRUE(I32->isCompatible(IntLit));
}