#include "gtest/gtest.h"
#include "Parse/Parse.hpp"
#include "Sema/Sema.hpp"

std::unique_ptr<Parser> makeParser(const std::string &Filename) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
	  llvm::MemoryBuffer::getFile(Filename);
  if (std::error_code BufferErr = FileOrErr.getError()) {
	std::cout << BufferErr.message() << " " << BufferErr.value();
	return nullptr;
  }
  auto SrcMgr = std::make_shared<llvm::SourceMgr>();
  SrcMgr->AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  std::shared_ptr<DiagEngine> Diag = std::make_shared<DiagEngine>(SrcMgr);
  auto LexerObj = std::make_unique<Lexer>(SrcMgr, *Diag);

  std::unique_ptr<SemaAnalyzer> Sema = std::make_unique<SemaAnalyzer>(Diag);

  std::unique_ptr<Parser> Parse = std::make_unique<Parser>(std::move(LexerObj), *Diag, std::move(Sema));

  return std::move(Parse);
}

TEST(ParseTesting, fnTest1) {
  auto ParserPtr = makeParser("./fnTest1.fun");
  ASSERT_NE(nullptr, ParserPtr);
  auto Func = ParserPtr->function();
  EXPECT_NE(nullptr, Func);
}

TEST(ParseTesting, structTest) {
  auto ParserPtr = makeParser("./structTest.fun");
  ASSERT_NE(nullptr, ParserPtr);
  auto StructDecl = ParserPtr->typeDecl();
  EXPECT_NE(nullptr, StructDecl);
}

TEST(ParseTesting, fnTest2) {
  auto ParserPtr = makeParser("./fnTest2.fun");
  ASSERT_NE(nullptr, ParserPtr);
  auto Func = ParserPtr->function();
  EXPECT_NE(nullptr, Func);
}

TEST(ParseTesting, compilationUnit) {
  auto ParserPtr = makeParser("./compilationUnit.fun");
  ASSERT_NE(nullptr, ParserPtr);
  std::unique_ptr<CompilationUnit> Program = ParserPtr->program();
  EXPECT_NE(nullptr, Program);
}

TEST(ParseTesting, assignExpr) {
  auto ParserPtr = makeParser("./assignExpr.fun");
  ASSERT_NE(nullptr, ParserPtr);
  std::unique_ptr<CompoundStmt> Program = ParserPtr->compoundStmt();
  ASSERT_NE(nullptr, Program);
  auto &Stmts = Program->getStmts();
  EXPECT_EQ(Stmts.size(), 5);
}
TEST(ParseTesting, cmpExpr) {

  auto ParserPtr = makeParser("./cmpExpr.fun");
  ASSERT_NE(nullptr, ParserPtr);
  std::unique_ptr<Expr> CmpExpr = ParserPtr->cmpExpr();
  ASSERT_NE(nullptr, CmpExpr);

}
TEST(ParseTesting, bigCompilationUnit) {
  auto ParserPtr = makeParser("./bigCompilationUnit.fun");
  ASSERT_NE(nullptr, ParserPtr);
  std::unique_ptr<CompilationUnit> Program = ParserPtr->program();
  EXPECT_NE(nullptr, Program);
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
