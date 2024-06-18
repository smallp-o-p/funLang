#include "Lex/Lex.hpp"
#include "../../include/Basic/Basic.hpp"
#include "gtest/gtest.h"
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/SMLoc.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>
#include <filesystem>
#include <cstdio>
#include <cstdlib>
#include <iostream>

std::unique_ptr<Lexer> makeLexer(std::string filename) {

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileOrErr =
	  llvm::MemoryBuffer::getFile(filename);
  if (std::error_code bufferErr = fileOrErr.getError()) {
	std::cout << bufferErr.message() << " " << bufferErr.value();
	return nullptr;
  }
  auto srcMgr = std::make_shared<llvm::SourceMgr>();
  srcMgr->AddNewSourceBuffer(std::move(*fileOrErr), llvm::SMLoc());
  DiagEngine diag = DiagEngine(srcMgr);
  return std::make_unique<Lexer>(srcMgr, diag);
}

TEST(LexTests, BuildLexerObject) {
  using namespace Basic;
  auto lexx = makeLexer("./lex_Basic.txt");

  EXPECT_NE(nullptr, lexx);
}

// file should get copied over to executable directory
//+-*/>< =(){}!,:;
//<=
//>=
//==
//!=
TEST(LexTests, LexBasicToken) {
  using namespace Basic;

  auto lexer = makeLexer("./lex_Basic.txt");
  EXPECT_NE(nullptr, lexer) << "Lexer returned nullptr";
  std::vector<Basic::tok::Tag> expected = {
	  tok::plus, tok::minus, tok::star, tok::slash,
	  tok::greater, tok::less, tok::equal, tok::l_paren,
	  tok::r_paren, tok::l_brace, tok::r_brace, tok::exclaim,
	  tok::comma, tok::colon, tok::semi, tok::lessequal,
	  tok::greaterequal, tok::equalequal, tok::exclaimequal, tok::plusequal,
	  tok::minusequal, tok::starequal, tok::slashequal};
  for (auto tok : expected) {
	Token lexed_token = lexer->advance();
	EXPECT_EQ(lexed_token.getTag(), tok)
			<< "Expected: " << tok::getTokenName(tok)
			<< " Received: " << tok::getTokenName(lexed_token.getTag());
  }
  EXPECT_EQ(lexer->advance().getTag(), tok::eof);
}

TEST(LexTests, LexKeywords) {
  using namespace Basic;
  auto lexer = makeLexer("./lex_Keywords.txt");
  std::vector<Basic::tok::Tag> expected = {
	  tok::kw_void, tok::kw_bool, tok::kw_char, tok::kw_string,
	  tok::kw_i32, tok::kw_i64, tok::kw_f32, tok::kw_f64,
	  tok::kw_return, tok::kw_true, tok::kw_false, tok::kw_for,
	  tok::kw_while, tok::kw_loop, tok::kw_struct, tok::kw_enum};
  for (auto expected_tok : expected) {
	Token lexed_token = lexer->advance();
	EXPECT_EQ(lexed_token.getTag(), expected_tok)
			<< "Expected: " << tok::getTokenName(expected_tok)
			<< " Received: " << tok::getTokenName(lexed_token.getTag());
  }
}

TEST(LexTests, IntNumLiterals) {
  using namespace Basic;
  auto lexer = makeLexer("./lex_IntNumLits.txt");
  Token tok = lexer->advance();
  while (tok.getTag() != tok::eof) {
	EXPECT_EQ(tok.getTag(), tok::numeric_constant)
			<< "\nExpected: " << tok::getTokenName(tok::numeric_constant)
			<< " Received: " << tok::getTokenName(tok.getTag());
	tok = lexer->advance();
  }
  EXPECT_EQ(tok.getTag(), tok::eof);
}

TEST(LexTests, RealNumLiterals) {
  using namespace Basic;
  auto lexer = makeLexer("./lex_RealNumLits.txt");
  Token tok = lexer->advance();
  while (tok.getTag() != tok::eof) {
	EXPECT_EQ(tok.getTag(), tok::floating_constant)
			<< "\nExpected: " << tok::getTokenName(tok::floating_constant)
			<< " Received: " << tok::getTokenName(tok.getTag());
	tok = lexer->advance();
  }
  EXPECT_EQ(tok.getTag(), tok::eof);
}

TEST(LexTests, StringLits) {
  using namespace Basic;
  std::unique_ptr<Lexer> lex = std::move(makeLexer("./lex_StringLits.txt"));
  EXPECT_NE(nullptr, lex);

  Token tok = lex->advance();
  while (tok.getTag() != tok::eof) {
	EXPECT_EQ(tok::string_literal, tok.getTag())
			<< "\nExpected: " << tok::getTokenName(tok::string_literal)
			<< " Received: " << tok::getTokenName(tok.getTag());
	tok = lex->advance();
  }
  EXPECT_EQ(tok::eof, lex->previous().getTag());
}

TEST(LexTests, ExampleFunc) {
  using namespace Basic;
  auto lex = makeLexer("./lex_ExampleFunc.txt");
  EXPECT_NE(lex, nullptr) << "Lexer init returned nullptr.";
  std::vector<Basic::tok::Tag> expected = {
	  tok::kw_i32, tok::identifier, tok::l_paren,
	  tok::kw_i32, tok::identifier, tok::comma,
	  tok::kw_i32, tok::identifier, tok::r_paren,
	  tok::l_brace, tok::kw_string, tok::identifier,
	  tok::equal, tok::string_literal, tok::semi,
	  tok::kw_return, tok::numeric_constant, tok::semi,
	  tok::r_brace};
  for (auto expected_tok : expected) {
	EXPECT_EQ(expected_tok, lex->advance().getTag())
			<< "Mismatch; Expected: " << tok::getTokenName(expected_tok)
			<< "\nReceived: " << tok::getTokenName(lex->previous().getTag());
  }
}

TEST(LexTests, WithErrs) {
  using namespace Basic;
  auto lexer = makeLexer("./lex_withErrors.txt");
  EXPECT_NE(lexer, nullptr) << "Lexer init returned nullptr.\n";
  std::vector<Basic::tok::Tag> expected = {
	  tok::err, tok::err, tok::err,
	  tok::err, tok::err, tok::kw_void,
	  tok::kw_bool, tok::kw_i32, tok::kw_i64,
	  tok::err, tok::err, tok::err, tok::err
  };

  for (auto expected_tok : expected) {
	EXPECT_EQ(expected_tok, lexer->advance().getTag())
			<< "Mismatch; Expected: " << tok::getTokenName(expected_tok)
			<< "\nReceived: " << tok::getTokenName(lexer->previous().getTag());
  }
}

int main() {
  testing::InitGoogleTest();
  return RUN_ALL_TESTS();
}
