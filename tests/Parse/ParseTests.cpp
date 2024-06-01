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

TEST(ParseTesting, fnTest1) {
  auto parser_ptr = makeParser("./fnTest1.fun");
  ASSERT_NE(nullptr, parser_ptr);
  auto func = parser_ptr->function();
  EXPECT_NE(nullptr, func);
}

TEST(ParseTesting, structTest) {
  auto parser_ptr = makeParser("./structTest.fun");
  ASSERT_NE(nullptr, parser_ptr);
  auto struct_decl = parser_ptr->typeDecl();
  EXPECT_NE(nullptr, struct_decl);
}

TEST(ParseTesting, fnTest2) {
  auto parser_ptr = makeParser("./fnTest2.fun");
  ASSERT_NE(nullptr, parser_ptr);
  auto func = parser_ptr->function();
  EXPECT_NE(nullptr, func);
}

TEST(ParseTesting, compilationUnit) {
  auto parser_ptr = makeParser("./compilationUnit.fun");
  ASSERT_NE(nullptr, parser_ptr);
  std::unique_ptr<CompilationUnit> program = parser_ptr->program();
  EXPECT_NE(nullptr, program);
}

TEST(ParseTesting, assignExpr) {
  auto parser_ptr = makeParser("./assignExpr.fun");
  ASSERT_NE(nullptr, parser_ptr);
  std::unique_ptr<CompoundStmt> program = parser_ptr->compoundStmt();
  ASSERT_NE(nullptr, program);
  auto &stmts = program->getStmts();
  EXPECT_EQ(stmts.size(), 5);
}

TEST(ParseTesting, bigCompilationUnit) {
  auto parser_ptr = makeParser("./bigCompilationUnit.fun");
  ASSERT_NE(nullptr, parser_ptr);
  std::unique_ptr<CompilationUnit> program = parser_ptr->program();
  EXPECT_NE(nullptr, program);
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
