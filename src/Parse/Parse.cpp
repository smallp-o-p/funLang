#include "Parse.hpp"
#include "AST.hpp"
#include "Lex.hpp"
#include "Basic.hpp"
#include <initializer_list>
#include <memory>

Token Parser::peek() { return lexer->peek(); }
bool Parser::check(Basic::tok::Tag tok) { return peek().getTag()==tok; }
Token &Parser::previous() { return lexer->previous(); }
Token &Parser::advance() { return lexer->advance(); }
bool Parser::isOneOf(std::initializer_list<Basic::tok::Tag> toExpect,
					 bool peeking = true) {
  Token consumed = peeking ? lexer->peek() : lexer->advance();

  return std::any_of(
	  toExpect.begin(), toExpect.end(),
	  [consumed](Basic::tok::Tag tok) { return tok==consumed.getTag(); });
}
bool Parser::expect(Basic::tok::Tag tok) {
  if (lexer->advance().getTag()!=tok) {
	reportExpect(tok, previous());
	return false;
  }
  return true;
}

void Parser::reportExpect(Basic::tok::Tag expected, Token received) {
  diags.emitDiagMsg(received.getLoc(), diag::err_expected,
					Basic::tok::getTokenName(expected), received.getLexeme());
}

void Parser::emitWarning(unsigned int diagID, llvm::SMLoc loc,
						 llvm::StringRef name) {
  diags.emitDiagMsg(loc, diagID, name);
}

Token Parser::lookahead(uint32_t howMuch) { return lexer->lookahead(howMuch); }

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

std::unique_ptr<TypeUse> Parser::type() {
  Token &type_name = advance();
  if (!type_name.isBaseType() && type_name.isIdentifier()) {
	diags.emitDiagMsg(type_name.getLoc(),
					  diag::err_expected,
					  llvm::StringRef("identifier or base type"),
					  type_name.getLexeme());
	return nullptr;
  }

  Decl *foundType = semantics->lookup(type_name.getLexeme());
  if (!foundType) {
	return std::make_unique<TypeUse>(type_name.getLoc()); // allow types to be used anywhere so long as they're declared
  }
  return std::make_unique<TypeUse>((TypeDecl *)foundType, type_name.getLoc());
}

std::unique_ptr<CompilationUnit> Parser::program() {
  // TODO: globals
  std::unique_ptr<TopLevelDecls> tops = topLevels();
  if (Parser::error) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::eof)) {
	return nullptr;
  }
  return std::make_unique<CompilationUnit>(std::move(tops));
}

std::unique_ptr<TopLevelDecls> Parser::topLevels() {
  std::unordered_map<std::string, std::unique_ptr<Decl>> topLevelList;
  while (true) {
	std::unique_ptr<Decl> topLevel;

	if (check(Basic::tok::kw_struct)) {
	  topLevel = typeDecl();
	} else if (isOneOf({Basic::tok::Tag::identifier}) || peek().isBaseType()) {
	  topLevel = function();
	}
	if (!topLevel) {
	  return nullptr;
	}
	if (semantics->actOnTopLevelDecl(*topLevel)) {
	  topLevelList.insert(std::pair<llvm::StringRef, std::unique_ptr<Decl>>(topLevel->getName(), std::move(topLevel)));
	}
	if (check(Basic::tok::Tag::eof)) {
	  break;
	}
  }
  return std::make_unique<TopLevelDecls>(std::move(topLevelList));
}

std::unique_ptr<TypeDecl> Parser::typeDecl() {
  advance();
  if (!expect(Basic::tok::identifier)) {
	return nullptr;
  }
  Token &id = previous();
  if (!expect(Basic::tok::l_brace)) {
	return nullptr;
  }
  std::unique_ptr<TypeProperties> members = typeProperties();

  if (!members) {
	return nullptr;
  }

  return std::make_unique<TypeDecl>(id.getLexeme(), std::move(members), id.getLoc());
}

std::unique_ptr<TypeProperties> Parser::typeProperties() {

}

std::unique_ptr<FunctionNode> Parser::function() {
  std::unique_ptr<TypeUse> type_node = type();
  if (!type_node) {
	return nullptr;
  }
  if (!expect(Basic::tok::identifier)) {
	return nullptr;
  }
  Token &id = previous();

  if (!expect(Basic::tok::Tag::l_paren)) {
	return nullptr;
  }
  std::unique_ptr<ArgsList> args = arguments();

  if (!args) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::r_paren)) {
	return nullptr;
  }
  semantics->enterFunction(std::move(type_node)); // borrow
  semantics->enterScope();
  std::unique_ptr<CompoundStmt> compound = compoundStmt();
  if (!compound) {
	return nullptr;
  }
  semantics->exitScope();
  type_node = semantics->exitFunction(); // give back
  return std::make_unique<FunctionNode>(std::move(type_node), id.getLexeme(),
										std::move(compound), id.getLoc());
}

std::unique_ptr<ArgsList> Parser::arguments() {
  std::vector<std::unique_ptr<ArgDecl>> argList;
  while (true) {
	if (check(Basic::tok::Tag::r_paren)) {
	  break;
	}
	if (!argList.empty() && !expect(Basic::tok::Tag::comma)) {
	  return nullptr;
	}
	std::unique_ptr<ArgDecl> argument = arg();
	if (!argument) {
	  return nullptr;
	}
	argList.push_back(std::move(argument));
  }
  return std::make_unique<ArgsList>(argList);
}

std::unique_ptr<ArgDecl> Parser::arg() {
  std::unique_ptr<TypeUse> t_node = type();
  if (!t_node) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::identifier)) {
	return nullptr;
  }
  Token &id = previous();

  return std::make_unique<ArgDecl>(std::move(t_node), id.getLexeme(), id.getLoc());
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
	std::unique_ptr<Stmt> s;
	if (check(Basic::tok::Tag::l_brace)) {
	  advance();
	  semantics->enterScope();
	  s = compoundStmt();
	} else {
	  s = simpleStmt();
	}
	if (!s) {
	  if (!recoverFromError(currentNT::STMT)) {
		return nullptr;
	  }
	} else {
	  if (s->getKind()==Stmt::SK_COMPOUND) {
		semantics->exitScope();
	  }
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
	if (check(tok::Tag::equal)) {
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
	return nullptr;
  }
  return stmt_inq;
}

std::unique_ptr<VarDeclStmt> Parser::declStmt() {
  std::unique_ptr<TypeUse> type_node = type();
  if (!type_node) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::identifier)) {
	return nullptr;
  }

  Token &id = previous();

  if (check(Basic::tok::Tag::semi)) {
	return std::make_unique<VarDeclStmt>(std::move(type_node), id.getLexeme(), id.getLoc());
  }
  if (!expect(Basic::tok::Tag::equal)) {
	return nullptr;
  }
  std::unique_ptr<Expr> expr_node = expr();

  if (!expr_node) {
	return nullptr;
  }
  auto decl = std::make_unique<VarDeclStmt>(std::move(type_node), id.getLexeme(),
											std::move(expr_node), id.getLoc());
  semantics->actOnVarDeclStmt(*decl);

  return decl;
}

std::unique_ptr<ReturnStmt> Parser::returnStmt() {
  if (!expect(Basic::tok::Tag::kw_return)) {
	return nullptr;
  }
  std::unique_ptr<Expr> expr_node = expr();
  if (!expr_node) {
	return nullptr;
  }
  semantics->actOnReturnStmt(*expr_node);

  return std::make_unique<ReturnStmt>(std::move(expr_node));
}

std::unique_ptr<Expr> Parser::expr() { return assign(); }

std::unique_ptr<Expr> Parser::assign() {
  std::unique_ptr<Expr> eq_node = eqExpr();
  Basic::Op::Binary opc = Basic::Op::Binary::equals;
  if (isOneOf({Basic::tok::Tag::equal, Basic::tok::Tag::plusequal,
			   Basic::tok::Tag::minusequal, Basic::tok::Tag::starequal,
			   Basic::tok::Tag::slashequal})) {
	switch (advance().getTag()) {
	case Basic::tok::Tag::equal:opc = Basic::Op::Binary::assign;
	  break;
	case Basic::tok::Tag::plusequal:opc = Basic::Op::Binary::plusassign;
	  break;
	case Basic::tok::Tag::minusequal:opc = Basic::Op::Binary::minusassign;
	  break;
	case Basic::tok::Tag::starequal:opc = Basic::Op::Binary::multassign;
	  break;
	case Basic::tok::Tag::slashequal:opc = Basic::Op::Binary::divassign;
	  break;
	default:return nullptr;
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
	Basic::Op::Binary opcode =
		advance().getTag()==Basic::tok::Tag::equalequal
		? Basic::Op::Binary::equals
		: Basic::Op::Binary::notequals;
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
	Basic::Op::Binary opcode;
	switch (advance().getTag()) {
	case Basic::tok::Tag::less:opcode = Basic::Op::lt;
	  break;
	case Basic::tok::Tag::lessequal:opcode = Basic::Op::ltequals;
	  break;
	case Basic::tok::Tag::greater:opcode = Basic::Op::gt;
	  break;
	case Basic::tok::Tag::greaterequal:opcode = Basic::Op::gtequals;
	  break;
	default:return nullptr;
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
	Basic::Op::Binary opcode = advance().getTag()==Basic::tok::Tag::plus
							   ? Basic::Op::plus
							   : Basic::Op::minus;
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
	Basic::Op::Binary opcode = advance().getTag()==Basic::tok::Tag::star
							   ? Basic::Op::multiply
							   : Basic::Op::divide;
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
  Op::Unary opcode = Basic::Op::Unary::NUM_UNARY;
  if (isOneOf(
	  {tok::plusplus, tok::minusminus, tok::exclaim, tok::Tag::minus})) {
	switch (advance().getTag()) {
	case tok::plusplus:opcode = Basic::Op::Unary::preInc;
	  break;
	case tok::exclaim:opcode = Basic::Op::Unary::lNot;
	  break;
	case tok::minusminus:opcode = Basic::Op::Unary::preDec;
	  break;
	case tok::minus:opcode = Basic::Op::Unary::unaryMinus;
	  break;
	default:;
	}
  }
  std::unique_ptr<Expr> primary_node = primary();
  if (!primary_node) {
	return nullptr;
  }
  if (opcode!=Basic::Op::Unary::NUM_UNARY) {
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
	  std::unique_ptr<FunctionCall> fncall_node = fnCall();
	  if (!fncall_node) {
		return nullptr;
	  }
	  semantics->actOnFnCall(*fncall_node);
	  return fncall_node;
	} else { /* identifier only */
	  Token &var_name = advance();
	  semantics->actOnNameUsage(var_name);
	  std::unique_ptr<NameUsage> leaf = std::make_unique<NameUsage>(var_name.getLexeme());
	  return leaf;
	}
  } else if (isOneOf({Basic::tok::floating_constant,
					  Basic::tok::numeric_constant, Basic::tok::string_literal,
					  Basic::tok::kw_true, Basic::tok::kw_false})) {
	auto token = advance();
	switch (token.getTag()) {
	case Basic::tok::numeric_constant: {
	}
	case Basic::tok::string_literal:
	case Basic::tok::floating_constant:
	case Basic::tok::kw_true: return std::make_unique<BooleanLiteral>(true, token.getLoc());
	case Basic::tok::kw_false: return std::make_unique<BooleanLiteral>(false, token.getLoc());
	default: return nullptr;
	}
  }
  return nullptr;
}

std::unique_ptr<FunctionCall> Parser::fnCall() {
  if (!expect(Basic::tok::identifier)) {
	return nullptr;
  }
  Token &id = previous();
  if (!expect(Basic::tok::l_paren)) {
	return nullptr;
  }
  std::unique_ptr<CallArgList> callargs_node = callArgs();
  if (!callargs_node) {
	return nullptr;
  }
  if (!expect(Basic::tok::r_paren)) {
	return nullptr;
  }

  return std::make_unique<FunctionCall>(id.getLexeme(), std::move(callargs_node), id.getLoc());
}

std::unique_ptr<CallArgList> Parser::callArgs() {
  std::vector<std::unique_ptr<Expr>> args;
  while (true) {
	if (check(Basic::tok::r_paren)) {
	  break;
	}
	if (!args.empty() && !expect(Basic::tok::Tag::comma)) {
	  return nullptr;
	}
	std::unique_ptr<Expr> expr_ptr = expr();
	if (!expr_ptr) {
	  return nullptr;
	}
	args.push_back(std::move(expr_ptr));
  }
  return std::make_unique<CallArgList>(std::move(args));
}
