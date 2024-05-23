#include "gtest/gtest.h"
#include "Parse.hpp"
#include "Sema.hpp"

std::unique_ptr<Parser> makeParser(const std::string &filename) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileOrErr =
	  llvm::MemoryBuffer::getFile(filename);
  if (std::error_code bufferErr = fileOrErr.getError()) {
	std::cout << bufferErr.message() << " " << bufferErr.value();
	return nullptr;
  }
  auto srcMgr = std::make_shared<llvm::SourceMgr>();
  srcMgr->AddNewSourceBuffer(std::move(*fileOrErr), llvm::SMLoc());

  std::shared_ptr<DiagEngine> diag = std::make_shared<DiagEngine>(srcMgr);
  auto lexer = std::make_unique<Lexer>(srcMgr, *diag);

  std::unique_ptr<SemaAnalyzer> semaAnalyzer = std::make_unique<SemaAnalyzer>(diag);

  std::unique_ptr<Parser> parser = std::make_unique<Parser>(std::move(lexer), *diag, std::move(semaAnalyzer));

  return std::move(parser);
}

TEST(ParseTesting, ExpFunc) {
  auto parser_ptr = makeParser("./FnsParseTest.txt");
  EXPECT_NE(nullptr, parser_ptr);
  auto func = parser_ptr->function();
  EXPECT_NE(nullptr, func);
}

TEST(ParseTesting, ExpStruct) {
  auto parser_ptr = makeParser("./structTest.fun");
  EXPECT_NE(nullptr, parser_ptr);
  auto struct_decl = parser_ptr->typeDecl();
  EXPECT_NE(nullptr, struct_decl);
}

TEST(ParseTesting, FuncWStmts) {
  auto parser_ptr = makeParser("./FnsParseTest2.txt");
  EXPECT_NE(nullptr, parser_ptr);
  auto func = parser_ptr->function();
  EXPECT_NE(nullptr, func);
}

TEST(ParseTesting, TwoFuncs) {
  auto parser_ptr = makeParser("./FnsParseTest3.txt");
  EXPECT_NE(nullptr, parser_ptr);
  std::unique_ptr<CompilationUnit> program = parser_ptr->program();
  EXPECT_NE(nullptr, program);
  if (!program) {
	FAIL();
  } else {
  }
}

TEST(ParseTesting, ManyFunc) {
  auto parser_ptr = makeParser("./FnsParseTest4.fun");
  EXPECT_NE(nullptr, parser_ptr);
  std::unique_ptr<CompilationUnit> program = parser_ptr->program();
  EXPECT_NE(nullptr, program);
  if (!program) {
	FAIL();
  } else {
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
