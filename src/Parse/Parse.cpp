#include "Parse.hpp"
#include "AST.hpp"
#include "Lex.hpp"
#include "TokenTags.hpp"
#include <initializer_list>
#include <memory>

Token Parser::peek() { return lexer->peek(); }
bool Parser::check(Basic::tok::Tag tok) { return peek().getTag()==tok; }
Token &Parser::previous() { return lexer->previous(); }
Token &Parser::advance() { return lexer->advance(); }
bool Parser::isOneOf(std::initializer_list<Basic::tok::Tag> toExpect,
					 bool peeking = true) {
  Token consumed = peeking ? lexer->peek() : lexer->advance();

  return std::any_of(toExpect.begin(),
					 toExpect.end(),
					 [consumed](Basic::tok::Tag tok) { return tok==consumed.getTag(); });
}
bool Parser::expect(Basic::tok::Tag tok) {
  return lexer->advance().getTag()==tok;
}

void Parser::reportExpect(Basic::tok::Tag expected, Token received) {
  diags.emitDiagMsg(received.getFromPtr(),
					diag::err_expected,
					Basic::tok::getTokenName(expected),
					received.getLexeme());
}

void Parser::emitWarning(unsigned int diagID, llvm::SMLoc loc, llvm::StringRef name) {
  diags.emitDiagMsg(loc, diagID, name);
}
Token Parser::lookahead(uint32_t howMuch) { return lexer->lookahead(howMuch); }

std::unique_ptr<TypeNode> Parser::type() {
  std::unique_ptr<TypeNode> type_ptr = std::make_unique<TypeNode>(advance());
  if (type_ptr->getType()==TypeNode::DataTypes::INVALID) {
	return nullptr;
  } else {
	return type_ptr;
  }
}

bool Parser::recoverFromError(currentNT whereWeFailed) {
  error = true;
  switch (whereWeFailed) {
  case STMT: {
	do {
	  advance(); // discard symbols until we find a semicolon and eat the
	  // semicolon
	} while (!check(Basic::tok::Tag::semi) && !check(Basic::tok::Tag::eof));
	break;
  }
  case FUNCTION: {
	do {
	  advance();
	} while (!check(Basic::tok::Tag::r_brace) && !check(Basic::tok::Tag::eof));
	break;
  }
  }
  if (peek().getTag()==Basic::tok::Tag::eof) {
	return false;
  }
  return true;
}

std::unique_ptr<CompilationUnit> Parser::program() {
  // TODO: globals
  std::unique_ptr<FunctionsNode> fns = functions();
  if (Parser::error) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::eof)) {
	return nullptr;
  }
  return std::make_unique<CompilationUnit>(std::move(fns));
}

std::unique_ptr<FunctionsNode> Parser::functions() {
  std::unordered_map<std::string, std::shared_ptr<FunctionNode>> funcList;
  while (true) {
	std::shared_ptr<FunctionNode> fn = function();
	if (!fn) {
	  return nullptr;
	}
	if (funcList.find(fn->getName().str())!=funcList.cend()) {
	  emitWarning(diag::warn_function_redefinition,
				  llvm::SMLoc::getFromPointer(fn->getName().data()),
				  fn->getName());
	} else {
	  funcList.insert(std::make_pair(fn->getName(), std::move(fn)));
	}
	if (check(Basic::tok::Tag::eof)) {
	  break;
	}
  }
  return std::make_unique<FunctionsNode>(std::move(funcList));
}

std::shared_ptr<FunctionNode> Parser::function() {
  std::unique_ptr<PrototypeNode> prototype = proto();
  if (!prototype) {
	return nullptr;
  }

  std::unique_ptr<CompoundStmt> compound = compoundStmt();
  if (!compound) {
	return nullptr;
  }
  return std::make_unique<FunctionNode>(std::move(prototype),
										std::move(compound));
}

std::unique_ptr<PrototypeNode> Parser::proto() {
  std::unique_ptr<TypeNode> type_node = type();

  if (!type_node) {
	return nullptr;
  }
  if (!expect(Basic::tok::identifier)) {
	reportExpect(Basic::tok::identifier, previous());
	return nullptr;
  }
  Token &id = previous();

  if (!expect(Basic::tok::Tag::l_paren)) {
	reportExpect(Basic::tok::l_paren, previous());
	return nullptr;
  }
  std::unique_ptr<ArgumentsNode> args = arguments();

  if (!args) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::r_paren)) {
	reportExpect(Basic::tok::r_paren, previous());
	return nullptr;
  }

  return std::make_unique<PrototypeNode>(std::move(type_node), id,
										 std::move(args));
}

std::unique_ptr<ArgumentsNode> Parser::arguments() {
  std::vector<std::unique_ptr<ArgNode>> argList;
  while (true) {
	if (check(Basic::tok::Tag::r_paren)) {
	  break;
	}
	if (!argList.empty() && !expect(Basic::tok::Tag::comma)) {
	  reportExpect(Basic::tok::Tag::comma, previous());
	  return nullptr;
	}
	std::unique_ptr<ArgNode> argument = arg();
	if (!argument) {
	  return nullptr;
	}
	argList.push_back(std::move(argument));
  }
  return std::make_unique<ArgumentsNode>(argList);
}

std::unique_ptr<ArgNode> Parser::arg() {
  std::unique_ptr<TypeNode> t_node = type();
  if (!t_node) {
	return nullptr;
  }

  if (!expect(Basic::tok::Tag::identifier)) {
	return nullptr;
  }
  Token &id = previous();

  return std::make_unique<ArgNode>(std::move(t_node), id);
}

std::unique_ptr<CompoundStmt> Parser::compoundStmt() {
  if (!expect(Basic::tok::Tag::l_brace)) {
	return nullptr;
  }
  std::vector<std::unique_ptr<Stmt>> stmts;
  while (true) {
	if (check(Basic::tok::Tag::r_brace)) {
	  advance();
	  break;
	}
	std::unique_ptr<Stmt> s = simpleStmt();
	if (!s) {
	  if (!recoverFromError(currentNT::STMT)) {
		return nullptr;
	  }
	} else {
	  stmts.push_back(std::move(s));
	}
  }
  return std::make_unique<CompoundStmt>(std::move(stmts));
}

std::unique_ptr<Stmt> Parser::simpleStmt() {
  using namespace Basic;
  std::unique_ptr<Stmt> stmt_inq;
  switch (peek().getTag()) {
  case Basic::tok::Tag::kw_void:
  case tok::Tag::kw_bool:
  case tok::Tag::kw_i32:
  case tok::Tag::kw_i64:
  case tok::Tag::kw_f32:
  case tok::Tag::kw_f64:
  case tok::Tag::kw_string:stmt_inq = declStmt();
	break;
  case tok::Tag::identifier: {
	if (check(tok::Tag::equal)) { // unsure how we'll handle struct
	  // members but we'll find out
	  stmt_inq = declStmt();
	} else {
	  stmt_inq = expr();
	}
	break;
  }
  case tok::Tag::kw_return: {
	stmt_inq = returnStmt();
	break;
  }
  default:return nullptr;
  }
  if (!stmt_inq) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::semi)) {
	reportExpect(Basic::tok::Tag::semi, previous());
	return nullptr;
  }
  return stmt_inq;
}
std::unique_ptr<VarDeclStmt> Parser::declStmt() {
  std::unique_ptr<TypeNode> type_node = type();
  if (!type_node) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::identifier)) {
	return nullptr;
  }
  Token &id = previous();

  if (!expect(Basic::tok::Tag::equal)) {
	return nullptr;
  }
  std::unique_ptr<Expr> expr_node = expr();
  if (!expr_node) {
	return nullptr;
  }
  return std::make_unique<VarDeclStmt>(std::move(type_node), id,
									   std::move(expr_node));
}

std::unique_ptr<returnNode> Parser::returnStmt() {

  if (!expect(Basic::tok::Tag::kw_return)) {
	return nullptr;
  }
  std::unique_ptr<Expr> expr_node = expr();

  if (!expr_node) {
	return nullptr;
  }
  return std::make_unique<returnNode>(std::move(expr_node));
}

std::unique_ptr<Expr> Parser::expr() { return assign(); }

std::unique_ptr<Expr> Parser::assign() {
  std::unique_ptr<Expr> eq_node = eqExpr();
  Basic::BinaryOperations opc = Basic::BinaryOperations::equals;
  if (isOneOf({Basic::tok::Tag::equal, Basic::tok::Tag::plusequal, Basic::tok::Tag::minusequal,
			   Basic::tok::Tag::starequal, Basic::tok::Tag::slashequal})) {
	switch (advance().getTag()) {
	case Basic::tok::Tag::equal: opc = Basic::BinaryOperations::assign;
	  break;
	case Basic::tok::Tag::plusequal: opc = Basic::BinaryOperations::plusassign;
	  break;
	case Basic::tok::Tag::minusequal: opc = Basic::BinaryOperations::minusassign;
	  break;
	case Basic::tok::Tag::starequal: opc = Basic::BinaryOperations::multassign;
	  break;
	case Basic::tok::Tag::slashequal: opc = Basic::BinaryOperations::divassign;
	  break;
	default: return nullptr;
	}
	std::unique_ptr<Expr> expr_node = eqExpr();
	if (!expr_node) {
	  return nullptr;
	}
	return std::make_unique<BinaryOp>(std::move(eq_node), std::move(expr_node),
									  opc);
  } else {
	return eq_node;
  }
}

std::unique_ptr<Expr> Parser::eqExpr() {
  std::unique_ptr<Expr> cmp_node = cmpExpr();
  if (!cmp_node) {
	return nullptr;
  }
  if (isOneOf({Basic::tok::Tag::equalequal, Basic::tok::Tag::exclaimequal})) {
	Basic::BinaryOperations opcode =
		advance().getTag()==Basic::tok::Tag::equalequal
		? Basic::BinaryOperations::equals
		: Basic::BinaryOperations::notequals;
	std::unique_ptr<Expr> cmp_node2 = cmpExpr();

	if (!cmp_node2) {
	  return nullptr;
	}
	return std::make_unique<BinaryOp>(std::move(cmp_node), std::move(cmp_node2),
									  opcode);
  }
  return cmp_node;
}

std::unique_ptr<Expr> Parser::cmpExpr() {
  std::unique_ptr<Expr> add_node = addExpr();
  if (!add_node) {
	return nullptr;
  }
  if (isOneOf({Basic::tok::Tag::less, Basic::tok::Tag::lessequal,
			   Basic::tok::Tag::greater, Basic::tok::Tag::greaterequal})) {
	Basic::BinaryOperations opcode;
	switch (advance().getTag()) {
	case Basic::tok::Tag::less:opcode = Basic::BinaryOperations::lt;
	  break;
	case Basic::tok::Tag::lessequal:opcode = Basic::BinaryOperations::ltequals;
	  break;
	case Basic::tok::Tag::greater:opcode = Basic::BinaryOperations::gt;
	  break;
	case Basic::tok::Tag::greaterequal:opcode = Basic::BinaryOperations::gtequals;
	  break;
	default: return nullptr;
	}
	std::unique_ptr<Expr> add_node2 = addExpr();
	if (!add_node2) {
	  return nullptr;
	}
	return std::make_unique<BinaryOp>(std::move(add_node), std::move(add_node2),
									  opcode);
  }
  return add_node;
}

std::unique_ptr<Expr> Parser::addExpr() {
  std::unique_ptr<Expr> multdiv_node = multdiv();
  if (!multdiv_node) {
	return nullptr;
  }
  if (isOneOf({Basic::tok::Tag::plus, Basic::tok::Tag::minus})) {
	Basic::BinaryOperations opcode =
		advance().getTag()==Basic::tok::Tag::plus
		? Basic::BinaryOperations::plus
		: Basic::BinaryOperations::minus;
	std::unique_ptr<Expr> multdiv_node2 = multdiv();
	if (!multdiv_node2) {
	  return nullptr;
	}
	return std::make_unique<BinaryOp>(std::move(multdiv_node),
									  std::move(multdiv_node2), opcode);
  }
  return multdiv_node;
}

std::unique_ptr<Expr> Parser::multdiv() {
  std::unique_ptr<Expr> unary_node = unary();

  if (!unary_node) {
	return nullptr;
  }
  if (isOneOf({Basic::tok::Tag::star, Basic::tok::Tag::slash})) {
	Basic::BinaryOperations opcode =
		advance().getTag()==Basic::tok::Tag::star
		? Basic::BinaryOperations::multiply
		: Basic::BinaryOperations::divide;
	std::unique_ptr<Expr> unary_node2 = unary();
	if (!unary_node2) {
	  return nullptr;
	}
	return std::make_unique<BinaryOp>(std::move(unary_node),
									  std::move(unary_node2), opcode);
  }
  return unary_node;
}

std::unique_ptr<Expr> Parser::unary() {
  using namespace Basic;
  UnaryOperations opcode = UnaryOperations::NUM_UNARY;
  if (isOneOf(
	  {tok::plusplus, tok::minusminus, tok::exclaim, tok::Tag::minus})) {
	switch (advance().getTag()) {
	case tok::plusplus: opcode = UnaryOperations::preInc;
	  break;
	case tok::exclaim: opcode = UnaryOperations::lNot;
	  break;
	case tok::minusminus: opcode = UnaryOperations::preDec;
	  break;
	case tok::minus: opcode = UnaryOperations::unaryMinus;
	  break;
	default:;
	}
  }
  std::unique_ptr<Expr> primary_node = primary();
  if (!primary_node) {
	return nullptr;
  }
  if (opcode!=UnaryOperations::NUM_UNARY) {
	return std::make_unique<UnaryOp>(std::move(primary_node), opcode);
  } else {
	return primary_node;
  }
}

std::unique_ptr<Expr> Parser::primary() {
  if (check(Basic::tok::l_paren)) {
	advance();
	std::unique_ptr<Expr> expr_node = expr();
	if (!expr_node) {
	  return nullptr;
	}
	if (!expect(Basic::tok::r_paren)) {
	  return nullptr;
	}
	return expr_node;
  } else if (check(Basic::tok::identifier)) {
	if (lookahead(2).getTag()==Basic::tok::Tag::l_paren) {
	  std::unique_ptr<Expr> fncall_node = fnCall();
	  if (!fncall_node) {
		return nullptr;
	  }
	  return fncall_node;
	} else { /* identifier only */
	  std::unique_ptr<leafNode> leaf = std::make_unique<leafNode>(advance());
	  return leaf;
	}
  } else if (isOneOf({Basic::tok::floating_constant,
					  Basic::tok::numeric_constant, Basic::tok::string_literal,
					  Basic::tok::kw_true, Basic::tok::kw_false})) {
	return std::make_unique<leafNode>(advance());
  }
  return nullptr;
}

std::unique_ptr<Expr> Parser::fnCall() {
  if (!expect(Basic::tok::identifier)) {
	return nullptr;
  }
  Token &id = previous();
  if (!expect(Basic::tok::l_paren)) {
	return nullptr;
  }
  std::unique_ptr<callArgList> callargs_node = callArgs();

  if (!callargs_node) {
	return nullptr;
  }

  if (!expect(Basic::tok::r_paren)) {
	return nullptr;
  }
  return std::make_unique<fnCallNode>(id, std::move(callargs_node));
}

std::unique_ptr<callArgList> Parser::callArgs() {
  std::vector<std::unique_ptr<Expr>> args;
  while (true) {
	if (check(Basic::tok::r_paren)) {
	  break;
	}
	if (!args.empty() && !expect(Basic::tok::Tag::comma)) {
	  reportExpect(Basic::tok::Tag::comma, previous());
	  return nullptr;
	}
	std::unique_ptr<Expr> expr_ptr = expr();
	if (!expr_ptr) {
	  return nullptr;
	}
	args.push_back(std::move(expr_ptr));
  }
  return std::make_unique<callArgList>(std::move(args));
}
