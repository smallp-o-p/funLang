#include "Parse/Parse.hpp"

std::unique_ptr<Expr> Parser::expr() { return assign(); }

std::unique_ptr<Expr> Parser::assign() {
  std::unique_ptr<Expr> EqNode = eqExpr();
  Basic::Op::Binary Opcode = Basic::Op::Binary::BO_equals;
  if (isOneOf({Basic::tok::Tag::equal, Basic::tok::Tag::plusequal,
			   Basic::tok::Tag::minusequal, Basic::tok::Tag::starequal,
			   Basic::tok::Tag::slashequal})) {
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
	return semantics->actOnBinaryOp(std::move(EqNode), Opcode,
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
  if (isOneOf({Basic::tok::Tag::equalequal, Basic::tok::Tag::exclaimequal})) {
	Basic::Op::Binary Opcode = advance().getTag() == Basic::tok::Tag::equalequal
							   ? Basic::Op::Binary::BO_equals
							   : Basic::Op::Binary::BO_notequals;
	std::unique_ptr<Expr> CmpNode2 = cmpExpr();

	if (!CmpNode2) {
	  return nullptr;
	}
	return semantics->actOnBinaryOp(std::move(CmpNode), Opcode,
									std::move(CmpNode2));
  }
  return CmpNode;
}

std::unique_ptr<Expr> Parser::cmpExpr() {
  std::unique_ptr<Expr> AddNode = addExpr();
  if (!AddNode) {
	return nullptr;
  }
  if (isOneOf({Basic::tok::Tag::less, Basic::tok::Tag::lessequal,
			   Basic::tok::Tag::greater, Basic::tok::Tag::greaterequal})) {
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

	return semantics->actOnBinaryOp(std::move(AddNode), Opcode,
									std::move(AddNode2));
  }
  return AddNode;
}

std::unique_ptr<Expr> Parser::addExpr() {
  std::unique_ptr<Expr> MultdivNode = multdiv();
  if (!MultdivNode) {
	return nullptr;
  }
  if (isOneOf({Basic::tok::Tag::plus, Basic::tok::Tag::minus})) {
	Basic::Op::Binary Opcode = advance().getTag() == Basic::tok::Tag::plus
							   ? Basic::Op::BO_plus
							   : Basic::Op::BO_minus;
	std::unique_ptr<Expr> MultdivNode2 = multdiv();
	if (!MultdivNode2) {
	  return nullptr;
	}
	return semantics->actOnBinaryOp(std::move(MultdivNode), Opcode,
									std::move(MultdivNode2));
  }
  return MultdivNode;
}

std::unique_ptr<Expr> Parser::multdiv() {
  std::unique_ptr<Expr> UnaryNode = unary();

  if (!UnaryNode) {
	return nullptr;
  }
  if (isOneOf({Basic::tok::Tag::star, Basic::tok::Tag::slash})) {
	Basic::Op::Binary Opcode = advance().getTag() == Basic::tok::Tag::star
							   ? Basic::Op::BO_multiply
							   : Basic::Op::BO_divide;
	std::unique_ptr<Expr> UnaryNode2 = unary();
	if (!UnaryNode2) {
	  return nullptr;
	}
	return semantics->actOnBinaryOp(std::move(UnaryNode), Opcode,
									std::move(UnaryNode2));
  }
  return UnaryNode;
}

std::unique_ptr<Expr> Parser::unary() {
  using namespace Basic;
  Op::Unary Opcode = Basic::Op::Unary::NUM_UNARY;
  if (isOneOf(
	  {tok::plusplus, tok::minusminus, tok::exclaim, tok::Tag::minus})) {
	switch (advance().getTag()) {
	case tok::plusplus: Opcode = Basic::Op::Unary::UO_preInc;
	  break;
	case tok::exclaim: Opcode = Basic::Op::Unary::UO_lNot;
	  break;
	case tok::minusminus: Opcode = Basic::Op::Unary::UO_preDec;
	  break;
	case tok::minus: Opcode = Basic::Op::Unary::UO_unaryMinus;
	  break;
	default:;
	}
  }
  std::unique_ptr<Expr> PrimaryNode = primary();
  if (!PrimaryNode) {
	return nullptr;
  }
  if (Opcode != Basic::Op::Unary::NUM_UNARY) {
	return semantics->actOnUnaryOp(Opcode, std::move(PrimaryNode));
  }
  return PrimaryNode;
}

std::unique_ptr<Expr> Parser::primary() {
  if (check(Basic::tok::l_paren)) {
	advance();
	std::unique_ptr<Expr> ExprNode = expr();
	if (!ExprNode) {
	  return nullptr;
	}
	if (!expect(Basic::tok::r_paren)) {
	  return nullptr;
	}
	return ExprNode;
  } else if (check(Basic::tok::identifier)) {
	if (lookahead(2).getTag() == Basic::tok::Tag::l_paren) {
	  std::unique_ptr<Expr> FunctionCall = fnCall();
	  if (!FunctionCall) {
		return nullptr;
	  }

	  return FunctionCall;
	} else { /* identifier only */
	  Token VarName = advance();
	  return semantics->actOnNameUsage(VarName);
	}
  } else if (isOneOf({Basic::tok::floating_constant,
					  Basic::tok::numeric_constant, Basic::tok::string_literal,
					  Basic::tok::kw_true, Basic::tok::kw_false})) {
	auto Token = advance();
	switch (Token.getTag()) {
	default: return nullptr;
	case Basic::tok::numeric_constant: {
	  return semantics->actOnIntegerLiteral(Token);
	}
	case Basic::tok::floating_constant: {
	  return semantics->actOnFloatingLiteral(Token);
	}
	case Basic::tok::string_literal: {
	  return semantics->actOnStrLiteral(Token);
	}
	case Basic::tok::kw_true:
	case Basic::tok::kw_false: {
	  return semantics->actOnBooleanLiteral(Token);
	}
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
  std::unique_ptr<CallArgList> CallargsNode = callArgs();
  if (!CallargsNode) {
	return nullptr;
  }
  if (!expect(Basic::tok::r_paren)) {
	return nullptr;
  }
  return semantics->actOnFnCall(Id, std::move(CallargsNode));
}
