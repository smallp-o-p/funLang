#include "Parse/Parse.hpp"
#include "gtest/gtest.h"

class ParseTesting : public testing::Test {
protected:
  std::unique_ptr<Parser> ParserObj;
  std::shared_ptr<llvm::SourceMgr> SrcMgr;
  std::shared_ptr<DiagEngine> Diags;
  bool setBuffer(llvm::StringRef Buf, bool IsStr = false) {
	llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrError = nullptr;
	if (IsStr) {
	  FileOrError = llvm::MemoryBuffer::getMemBufferCopy(Buf);
	} else {
	  FileOrError = llvm::MemoryBuffer::getFile(Buf);
	}
	if (std::error_code BufferErr = FileOrError.getError()) {
	  std::cout << BufferErr.message() << " " << BufferErr.value();
	  return false;
	}
	SrcMgr->AddNewSourceBuffer(std::move(*FileOrError), llvm::SMLoc());
	auto LexerObj = std::make_unique<Lexer>(SrcMgr, *Diags);
	auto Sema = std::make_unique<SemaAnalyzer>(Diags);
	ParserObj = std::make_unique<Parser>(std::move(LexerObj), *Diags, std::move(Sema));
	return true;
  }

  ParseTesting() {
	SrcMgr = std::make_shared<llvm::SourceMgr>();
	Diags = std::make_shared<DiagEngine>(SrcMgr);
  }
};

TEST_F(ParseTesting, simpleIfStmtTest) {
  ASSERT_TRUE(setBuffer("if a<=b {} else {}", true));
  auto T = ParserObj->ifStmt();
  ASSERT_NE(nullptr, T);
  ASSERT_TRUE(llvm::isa<ifStmt>(T));
}

TEST_F(ParseTesting, forStmtTest) {
  ASSERT_TRUE(setBuffer("for(i32 i=0; i<5; ++i){}", true));
  auto For = ParserObj->forStmt();
  ASSERT_NE(nullptr, For);
  ASSERT_EQ(llvm::isa<forStmt>(For.get()), true);
}

TEST_F(ParseTesting, whileStmtTest) {
  ASSERT_TRUE(setBuffer("while(i < 2) {}", true));
  std::unique_ptr<Stmt> While = ParserObj->whileStmt();
  ASSERT_NE(nullptr, While);
  ASSERT_EQ(llvm::isa<whileStmt>(While.get()), true);
}

TEST_F(ParseTesting, cmpExpr) {
  ASSERT_TRUE(setBuffer("(100 == 1-2/3);", true));
  std::unique_ptr<Expr> CmpExpr = ParserObj->cmpExpr();
  ASSERT_NE(nullptr, CmpExpr);
}

TEST_F(ParseTesting, fnTest1) {
  ASSERT_TRUE(setBuffer("./fnTest1.fun"));
  auto Func = ParserObj->function();
  EXPECT_NE(nullptr, Func);
}

TEST_F(ParseTesting, structTest) {
  ASSERT_TRUE(setBuffer("./structTest.fun"));
  auto StructDecl = ParserObj->typeDecl();
  EXPECT_NE(nullptr, StructDecl);
}

TEST_F(ParseTesting, fnTest2) {
  ASSERT_TRUE(setBuffer("./fnTest2.fun"));
  auto Func = ParserObj->function();
  EXPECT_NE(nullptr, Func);
}

TEST_F(ParseTesting, compilationUnit) {
  ASSERT_TRUE(setBuffer("./compilationUnit.fun"));
  std::unique_ptr<CompilationUnit> Program = ParserObj->program();
  EXPECT_NE(nullptr, Program);
}

TEST_F(ParseTesting, bigCompilationUnit) {
  ASSERT_TRUE(setBuffer("./bigCompilationUnit.fun"));
  std::unique_ptr<CompilationUnit> Program = ParserObj->program();
  EXPECT_NE(nullptr, Program);
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
