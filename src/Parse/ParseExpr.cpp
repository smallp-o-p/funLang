#include "Parse/Parse.hpp"

std::unique_ptr<Expr> Parser::expr() {
  if (nextTokIs(Basic::tok::kw_match)) { ;
	return match();
  }
  return assign();
}

std::unique_ptr<Expr> Parser::assign() {
  std::unique_ptr<Expr> EqNode = eqExpr();
  Basic::Op::Binary Opcode;
  if (nextIsOneOf(Basic::tok::Tag::equal, Basic::tok::Tag::plusequal,
				  Basic::tok::Tag::minusequal, Basic::tok::Tag::starequal,
				  Basic::tok::Tag::slashequal)) {
	switch (advance().getTag()) {
	case Basic::tok::Tag::equal: Opcode = Basic::Op::Binary::BO_assign;
	  break;
	case Basic::tok::Tag::plusequal: Opcode = Basic::Op::Binary::BO_plusassign;
	  break;
	case Basic::tok::Tag::minusequal: Opcode = Basic::Op::Binary::BO_minusassign;
	  break;
	case Basic::tok::Tag::starequal: Opcode = Basic::Op::Binary::BO_multassign;
	  break;
	case Basic::tok::Tag::slashequal: Opcode = Basic::Op::Binary::BO_divassign;
	  break;
	default: return nullptr;
	}
	std::unique_ptr<Expr> EqNode2 = eqExpr();
	if (!EqNode2) {
	  return nullptr;
	}
	return Semantics->actOnBinaryOp(std::move(EqNode), Opcode,
									std::move(EqNode2));
  } else {
	return EqNode;
  }
}

std::unique_ptr<Expr> Parser::eqExpr() {
  std::unique_ptr<Expr> CmpNode = cmpExpr();
  if (!CmpNode) {
	return nullptr;
  }
  if (nextIsOneOf(Basic::tok::Tag::equalequal, Basic::tok::Tag::exclaimequal)) {
	Basic::Op::Binary Opcode = advance().getTag() == Basic::tok::Tag::equalequal
							   ? Basic::Op::Binary::BO_equals
							   : Basic::Op::Binary::BO_notequals;
	std::unique_ptr<Expr> CmpNode2 = cmpExpr();

	if (!CmpNode2) {
	  return nullptr;
	}
	return Semantics->actOnBinaryOp(std::move(CmpNode), Opcode,
									std::move(CmpNode2));
  }
  return CmpNode;
}

std::unique_ptr<Expr> Parser::cmpExpr() {
  std::unique_ptr<Expr> AddNode = addExpr();
  if (!AddNode) {
	return nullptr;
  }
  if (nextIsOneOf(Basic::tok::Tag::less, Basic::tok::Tag::lessequal,
				  Basic::tok::Tag::greater, Basic::tok::Tag::greaterequal)) {
	Basic::Op::Binary Opcode;
	switch (advance().getTag()) {
	case Basic::tok::Tag::less: Opcode = Basic::Op::BO_lt;
	  break;
	case Basic::tok::Tag::lessequal: Opcode = Basic::Op::BO_ltequals;
	  break;
	case Basic::tok::Tag::greater: Opcode = Basic::Op::BO_gt;
	  break;
	case Basic::tok::Tag::greaterequal: Opcode = Basic::Op::BO_gtequals;
	  break;
	default: return nullptr;
	}
	std::unique_ptr<Expr> AddNode2 = addExpr();
	if (!AddNode2) {
	  return nullptr;
	}

	return Semantics->actOnBinaryOp(std::move(AddNode), Opcode,
									std::move(AddNode2));
  }
  return AddNode;
}

std::unique_ptr<Expr> Parser::addExpr() {
  std::unique_ptr<Expr> MultdivNode = multdiv();
  if (!MultdivNode) {
	return nullptr;
  }
  if (nextIsOneOf(Basic::tok::Tag::plus, Basic::tok::Tag::minus)) {
	Basic::Op::Binary Opcode = advance().getTag() == Basic::tok::Tag::plus
							   ? Basic::Op::BO_plus
							   : Basic::Op::BO_minus;
	std::unique_ptr<Expr> MultdivNode2 = multdiv();
	if (!MultdivNode2) {
	  return nullptr;
	}
	return Semantics->actOnBinaryOp(std::move(MultdivNode), Opcode,
									std::move(MultdivNode2));
  }
  return MultdivNode;
}

std::unique_ptr<Expr> Parser::multdiv() {
  std::unique_ptr<Expr> UnaryNode = unary();

  if (!UnaryNode) {
	return nullptr;
  }
  if (nextIsOneOf(Basic::tok::Tag::star, Basic::tok::Tag::slash)) {
	Basic::Op::Binary Opcode = advance().getTag() == Basic::tok::Tag::star
							   ? Basic::Op::BO_multiply
							   : Basic::Op::BO_divide;
	std::unique_ptr<Expr> UnaryNode2 = unary();
	if (!UnaryNode2) {
	  return nullptr;
	}
	return Semantics->actOnBinaryOp(std::move(UnaryNode), Opcode,
									std::move(UnaryNode2));
  }
  return UnaryNode;
}

std::unique_ptr<Expr> Parser::unary() {
  using namespace Basic;
  Op::Unary Opcode = Basic::Op::Unary::NUM_UNARY;
  if (nextIsOneOf(
	  tok::plusplus, tok::minusminus, tok::exclaim, tok::minus, tok::at)) {
	switch (advance().getTag()) {
	case tok::plusplus: Opcode = Basic::Op::Unary::UO_preInc;
	  break;
	case tok::exclaim: Opcode = Basic::Op::Unary::UO_lNot;
	  break;
	case tok::minusminus: Opcode = Basic::Op::Unary::UO_preDec;
	  break;
	case tok::minus: Opcode = Basic::Op::Unary::UO_unaryMinus;
	  break;
	case tok::at : return deref();
	default:;
	}
  }
  std::unique_ptr<Expr> Postfix = postfix();
  if (!Postfix) {
	return nullptr;
  }
  if (Opcode != Basic::Op::Unary::NUM_UNARY) {
	return Semantics->actOnUnaryOp(Opcode, std::move(Postfix), llvm::SMLoc());
  }
  return Postfix;
}

std::unique_ptr<Expr> Parser::deref() {
  assert(previous().is(Basic::tok::at) && "entered deref() without having the deref token before it");
  size_t derefCount = 1;
  while (nextTokIs(Basic::tok::at)) {
	derefCount++;
	advance();
  }

  std::unique_ptr<Expr> Postfix = postfix();
  if (!Postfix) {
	return nullptr;
  }

  return Semantics->actOnDereference(std::move(Postfix), derefCount);

}

std::unique_ptr<Expr> Parser::postfix() {
  std::unique_ptr<Expr> Primary = primary();
  if (!Primary) {
	return nullptr;
  }
  using namespace Basic;
  if (nextTokIs(tok::dot)) {
	advance();
	u_ptr<Expr> MemberUsage = primary();
	if (!MemberUsage) {
	  return nullptr;
	}
	return Semantics->actOnMemberExpr(std::move(Primary), std::move(MemberUsage));
  } else if (nextTokIs(tok::l_square)) {
	return arrayIndex(std::move(Primary));
  }
  return Primary;
}

std::unique_ptr<Expr> Parser::arrayIndex(std::unique_ptr<Expr> Input) {
  assert(advance().is(Basic::tok::l_square) && "Peek() should've been a l_square");
  llvm::SMLoc LSquareLoc = previous().getLoc();
  std::unique_ptr<Expr> Indexed = expr();

  if (!Indexed) {
	return nullptr;
  }
  if (!expect(Basic::tok::l_brace)) {
	return nullptr;
  }

  llvm::SMLoc RSquareLoc = previous().getLoc();

  return Semantics->actOnIndexOperation(std::move(Input), std::move(Input), LSquareLoc, RSquareLoc);
}

std::unique_ptr<Expr> Parser::primary() {
  if (nextTokIs(Basic::tok::l_paren)) {
	advance();
	std::unique_ptr<Expr> ExprNode = expr();
	if (!ExprNode) {
	  return nullptr;
	}
	if (!expect(Basic::tok::r_paren)) {
	  return nullptr;
	}
	return ExprNode;
  } else if (nextTokIs(Basic::tok::identifier)) {
	if (lookahead(2).getTag() == Basic::tok::Tag::l_paren) {
	  std::unique_ptr<Expr> FunctionCall = fnCall();
	  if (!FunctionCall) {
		return nullptr;
	  }

	  return FunctionCall;
	} else { /* identifier only */
	  Token VarName = advance();
	  return Semantics->actOnNameUsage(VarName);
	}
  } else if (nextIsOneOf(Basic::tok::floating_constant,
						 Basic::tok::numeric_constant, Basic::tok::string_literal,
						 Basic::tok::kw_true, Basic::tok::kw_false)) {
	auto Token = advance();
	switch (Token.getTag()) {
	case Basic::tok::numeric_constant: {
	  return Semantics->actOnIntegerLiteral(Token);
	}
	case Basic::tok::floating_constant: {
	  return Semantics->actOnFloatingLiteral(Token);
	}
	case Basic::tok::string_literal: {
	  u_ptr<funLang::StrLiteral> StrLit = Semantics->actOnStrLiteral(Token);
	}
	case Basic::tok::kw_true:
	case Basic::tok::kw_false: {
	  return Semantics->actOnBooleanLiteral(Token);
	}
	default: return nullptr;
	}
  }
  return nullptr;
}

std::unique_ptr<Expr> Parser::fnCall() {
  if (!expect(Basic::tok::identifier)) {
	return nullptr;
  }
  Token Id = previous();
  if (!expect(Basic::tok::l_paren)) {
	return nullptr;
  }
  u_ptr<llvm::SmallVector<u_ptr<Expr>>> Arguments = callArgs();
  if (!Arguments) {
	return nullptr;
  }
  if (!expect(Basic::tok::r_paren)) {
	return nullptr;
  }
  return Semantics->actOnFunctionCall(Id, llvm::SMLoc(), std::move(Arguments));
}

u_ptr<llvm::SmallVector<u_ptr<Expr>>> Parser::callArgs() {
  auto Args = std::make_unique<llvm::SmallVector<u_ptr<Expr>>>();
  while (true) {
	if (nextTokIs(Basic::tok::r_paren)) {
	  break;
	}
	if (!Args->empty() && !expect(Basic::tok::Tag::comma)) {
	  return nullptr;
	}
	std::unique_ptr<Expr> ExprPtr = expr();
	if (!ExprPtr) {
	  return nullptr;
	}
	Args->push_back(std::move(ExprPtr));
  }

  return Args;
}

std::unique_ptr<Expr> Parser::match() {
  return std::unique_ptr<MatchExpr>();
}
