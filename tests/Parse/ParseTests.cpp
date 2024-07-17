#include "Parse/Parse.hpp"
#include "gtest/gtest.h"

class ParseTesting : public testing::Test {
protected:
  std::unique_ptr<Parser> ParserObj;
  std::shared_ptr<llvm::SourceMgr> SrcMgr;
  std::shared_ptr<DiagEngine> Diags;
  bool setBuffer(llvm::StringRef Buf, bool Str = false) {
	llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrError = nullptr;
	if (Str) {
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

TEST_F(ParseTesting, PointerParsingTest) {
  ASSERT_TRUE(setBuffer("***i32", true));
  auto T = ParserObj->type();
  ASSERT_NE(nullptr, T);
  auto Next = llvm::dyn_cast<PointerType>(T->getTypeDecl()); // *
  ASSERT_NE(Next, nullptr);
  Next = llvm::dyn_cast<PointerType>(Next->getPointee());  // **
  ASSERT_NE(Next, nullptr);
  Next = llvm::dyn_cast<PointerType>(Next->getPointee()); // ***
  ASSERT_NE(Next, nullptr);
  auto BuiltIn = llvm::dyn_cast<BuiltInType>(Next->getPointee()); // i32
  ASSERT_NE(BuiltIn, nullptr);
}

TEST_F(ParseTesting, forStmtTest) {
  ASSERT_TRUE(setBuffer("for(i=0; i<5; ++i){}", true));
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

TEST_F(ParseTesting, assignExpr) {
  ASSERT_TRUE(setBuffer("./assignExpr.fun"));
  auto Compound = ParserObj->compoundStmt();
  ASSERT_NE(nullptr, Compound);
  EXPECT_EQ(Compound->getStmts().size(), 5);
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
