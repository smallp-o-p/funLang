#include "gtest/gtest.h"
#include "Parse.hpp"
std::unique_ptr<Parser> makeParser(const std::string &filename) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileOrErr =
	  llvm::MemoryBuffer::getFile(filename);
  if (std::error_code bufferErr = fileOrErr.getError()) {
	std::cout << bufferErr.message() << " " << bufferErr.value();
	return nullptr;
  }
  auto srcMgr = std::make_shared<llvm::SourceMgr>();
  srcMgr->AddNewSourceBuffer(std::move(*fileOrErr), llvm::SMLoc());
  DiagEngine diag = DiagEngine(srcMgr);
  auto lexer = std::make_unique<Lexer>(srcMgr, diag);
  std::unique_ptr<Parser> parser = std::make_unique<Parser>(std::move(lexer), diag);
  return std::move(parser);
}

TEST(ParseTesting, ExpFunc) {
  auto parser_ptr = makeParser("./FnsParseTest.txt");
  EXPECT_NE(nullptr, parser_ptr);
  auto func = parser_ptr->function();
  EXPECT_NE(nullptr, func);
}

TEST(ParseTesting, FuncWStmts) {
  auto parser_ptr = makeParser("./FnsParseTest2.txt");
  EXPECT_NE(nullptr, parser_ptr);
  auto func = parser_ptr->function();
  EXPECT_NE(nullptr, func);
  auto &args = func->getProto()->getArgs()->getArgList();
  EXPECT_EQ(func->getProto()->getTypeNode()->getType(), TypeNode::i32);
  EXPECT_EQ(args.size(), 1);
}

TEST(ParseTesting, TwoFuncs) {
  auto parser_ptr = makeParser("./FnsParseTest3.txt");
  EXPECT_NE(nullptr, parser_ptr);
  std::unique_ptr<ProgramNode> program = parser_ptr->program();
  EXPECT_NE(nullptr, program);
  if (!program) {
	FAIL();
  } else {
	auto &fnmap = program->getFuncs();
	EXPECT_EQ(fnmap.size(), 2);
	EXPECT_NE(fnmap["fun"], nullptr) << "Could not find fn fun()\n";
	EXPECT_NE(fnmap["main"], nullptr) << "Could not find fn main()\n";
  }
}

TEST(ParseTesting, ManyFunc) {
  auto parser_ptr = makeParser("./FnsParseTest4.fun");
  EXPECT_NE(nullptr, parser_ptr);
  std::unique_ptr<ProgramNode> program = parser_ptr->program();
  EXPECT_NE(nullptr, program);
  if (!program) {
	FAIL();
  } else {
	auto &fnmap = program->getFuncs();
	EXPECT_EQ(fnmap.size(), 3);
	EXPECT_NE(fnmap["returnTrue"], nullptr) << "Could not find fn returnTrue()\n";
	EXPECT_NE(fnmap["returnStringLit"], nullptr) << "Could not find fn returnStringLit()\n";
	EXPECT_NE(fnmap["main"], nullptr) << "Could not find fn main()\n";
  }
}

TEST(ParseTesting, AssignExpr) {

  auto parser_ptr = makeParser("./FnsParseTest5.fun");
  EXPECT_NE(nullptr, parser_ptr);
  std::unique_ptr<CompoundStmt> program = parser_ptr->compoundStmt();
  EXPECT_NE(nullptr, program);
  if (!program) {
	FAIL();
  } else {
	auto &stmts = program->getStmts();
	EXPECT_EQ(stmts.size(), 5);
  }
}
int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
