#include "Parse/Parse.hpp"

std::unique_ptr<ifStmt> Parser::ifStmt() {
  assert(advance().is(Basic::tok::kw_if) && "if stmt did not begin with if");
  llvm::SMLoc IfLoc = previous().getLoc();
  std::unique_ptr<Expr> Condition = expr();
  if (!Condition) {
	return nullptr;
  }

  std::unique_ptr<CompoundStmt> Compound = compoundStmt();
  if (!Compound) {
	return nullptr;
  }
  std::unique_ptr<CompoundStmt> ElseBlock = nullptr;
  if (peek().is(Basic::tok::kw_else)) {
	advance();
	ElseBlock = compoundStmt();
  }

  return semantics->actOnIfStmt(IfLoc, std::move(Condition), std::move(Compound), nullptr, std::move(ElseBlock));
}

std::unique_ptr<loopStmt> Parser::loopStmt() {
  Token LoopTok = advance();
  std::unique_ptr<CompoundStmt> Compound = compoundStmt();

  if (Compound) {
	return nullptr;
  }
  return semantics->actOnLoopStmt(std::move(Compound), LoopTok.getLoc());
}

std::unique_ptr<forStmt> Parser::forStmt() {
  Token ForTok = advance();
  assert(ForTok.is(Basic::tok::kw_for) &&
	  "first token in forStmt should be for");

  llvm::SMLoc ForLoc = ForTok.getLoc();
  if (!expect(Basic::tok::Tag::l_paren)) {
	return nullptr;
  }

  std::unique_ptr<Stmt> Init = nullptr;
  if (!peek().is(Basic::tok::semi)) {
	if (peek().isBaseType()) {
	  Init = declStmt();
	} else if (peek().is(Basic::tok::identifier)) {
	  if (lookahead(2).is(Basic::tok::identifier)) {
		Init = declStmt(); // id id
	  } else {
		Init = expr();
	  }
	} else {
	  return nullptr;
	}
  }
  if (!expect(Basic::tok::semi)) {
	return nullptr;
  }

  std::unique_ptr<Expr> Cond = expr();
  if (!Cond) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::semi)) {
	return nullptr;
  }
  std::unique_ptr<Expr> Inc = expr();

  if (!Inc) {
	return nullptr;
  }

  if (!expect(Basic::tok::Tag::r_paren)) {
	return nullptr;
  }

  llvm::SMLoc RParenLoc = previous().getLoc();

  std::unique_ptr<CompoundStmt> Compound = compoundStmt();

  if (!Compound) {
	return nullptr;
  }

  return semantics->actOnForStmt(std::move(Init), std::move(Cond),
								 std::move(Inc), std::move(Compound), ForLoc,
								 RParenLoc);
}

std::unique_ptr<whileStmt> Parser::whileStmt() {
  Token &WhileTok = advance();
  assert(WhileTok.is(Basic::tok::kw_while) && "not a while token in while");
  if (!expect(Basic::tok::l_paren)) {
	return nullptr;
  }

  std::unique_ptr<Expr> Condition = expr();

  if (!Condition) {
	return nullptr;
  }

  if (!expect(Basic::tok::r_paren)) {
	return nullptr;
  }
  llvm::SMLoc RParenLoc = previous().getLoc();
  std::unique_ptr<CompoundStmt> Compound = compoundStmt();

  if (!Compound) {
	return nullptr;
  }

  return semantics->actOnWhileStmt(std::move(Condition), std::move(Compound),
								   WhileTok.getLoc(), RParenLoc);
}

std::unique_ptr<VarDeclStmt> Parser::declStmt() {

  std::unique_ptr<VarDecl> D = nameDecl();
  if (!D) {
	return nullptr;
  }

  if (check(Basic::tok::Tag::semi)) {
	return semantics->actOnVarDeclStmt(std::move(D), nullptr,
									   previous().getLoc());
  }
  if (!expect(Basic::tok::Tag::equal)) {
	return nullptr;
  }
  std::unique_ptr<Expr> ExprNode = expr();

  if (!ExprNode) {
	return nullptr;
  }

  if (!expect(Basic::tok::semi)) {
	return nullptr;
  }

  return semantics->actOnVarDeclStmt(std::move(D), std::move(ExprNode),
									 previous().getLoc());
}

std::unique_ptr<ReturnStmt> Parser::returnStmt() {
  Token ReturnToken = advance();
  std::unique_ptr<Expr> ExprNode = nullptr;
  if (!check(Basic::tok::semi)) {
	ExprNode = expr();
	if (!ExprNode) {
	  return nullptr;
	}
  }
  if (!expect(Basic::tok::semi)) {
	return nullptr;
  }
  return semantics->actOnReturnStmt(ReturnToken.getLoc(), std::move(ExprNode));
}
