#include "Parse/Parse.hpp"
#include <initializer_list>
#include <llvm/ADT/APFloat.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/SMLoc.h>
#include <memory>
#include <utility>

Token Parser::peek() { return lexer->peek(); }
bool Parser::check(Basic::tok::Tag Tok) { return peek().getTag() == Tok; }
Token Parser::previous() { return lexer->previous(); }
Token &Parser::advance() { return lexer->advance(); }
bool Parser::isOneOf(std::initializer_list<Basic::tok::Tag> ToExpect,
					 bool Peeking) {
  Token Consumed = Peeking ? lexer->peek() : lexer->advance();

  return std::any_of(
	  ToExpect.begin(), ToExpect.end(),
	  [Consumed](Basic::tok::Tag Tok) { return Tok == Consumed.getTag(); });
}
bool Parser::expect(Basic::tok::Tag Tok) {
  if (lexer->advance().getTag() != Tok) {
	reportExpect(Tok, previous());
	return false;
  }
  return true;
}

void Parser::reportExpect(Basic::tok::Tag Expected, Token Received) {
  diags.emitDiagMsg(Received.getLoc(), diag::err_expected,
					Basic::tok::getTokenName(Expected), Received.getLexeme());
}

void Parser::emitWarning(unsigned int DiagId, llvm::SMLoc Loc,
						 llvm::StringRef Name) {
  diags.emitDiagMsg(Loc, DiagId, Name);
}

Token Parser::lookahead(uint32_t HowMuch) { return lexer->lookahead(HowMuch); }

bool Parser::recoverFromError(CurrentNonTerminal WhereWeFailed) {
  error = true;
  switch (WhereWeFailed) {
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
  if (peek().getTag() == Basic::tok::Tag::eof) {
	return false;
  }
  return true;
}

std::unique_ptr<TypeUse> Parser::type() {
  Token &TypeName = advance();
  if (!TypeName.isBaseType() && !TypeName.isIdentifier()) {
	diags.emitDiagMsg(TypeName.getLoc(), diag::err_expected,
					  llvm::StringRef("identifier or base type"),
					  TypeName.getLexeme());
	return nullptr;
  }
  return semantics->actOnTypeUse(TypeName);
}

std::unique_ptr<CompilationUnit> Parser::program() {
  // TODO: globals
  std::unique_ptr<TopLevelDecls> Tops = topLevels();
  if (Parser::error) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::eof)) {
	return nullptr;
  }
  return std::make_unique<CompilationUnit>(std::move(Tops));
}

std::unique_ptr<TopLevelDecls> Parser::topLevels() {
  std::unordered_map<std::string, std::unique_ptr<Decl>> TopLevelList;
  while (true) {
	std::unique_ptr<Decl> TopLevel;

	if (check(Basic::tok::kw_struct)) {
	  TopLevel = typeDecl();
	} else if (isOneOf({Basic::tok::Tag::identifier}) || peek().isBaseType()) {
	  TopLevel = function();
	}
	if (!TopLevel) {
	  return nullptr;
	}
	if (semantics->actOnTopLevelDecl(
		TopLevel
			.get())) { // check if top level name has been defined before
	  TopLevelList.insert(std::pair<llvm::StringRef, std::unique_ptr<Decl>>(
		  TopLevel->getName(), std::move(TopLevel)));
	}
	if (check(Basic::tok::Tag::eof)) {
	  break;
	}
  }
  return std::make_unique<TopLevelDecls>(std::move(TopLevelList));
}

std::unique_ptr<TypeDecl> Parser::typeDecl() {
  advance();
  if (!expect(Basic::tok::identifier)) {
	return nullptr;
  }
  Token Id = previous();
  if (!expect(Basic::tok::l_brace)) {
	return nullptr;
  }
  semantics->enterScope();

  std::unique_ptr<TypeProperties> Members = typeProperties();
  if (!Members) {
	return nullptr;
  }
  semantics->exitScope();
  if (!expect(Basic::tok::r_brace)) {
	return nullptr;
  }

  llvm::SMLoc RBraceLoc = previous().getLoc();
  return semantics->actOnStructDecl(Id, std::move(Members), RBraceLoc);
}

std::unique_ptr<TypeProperties> Parser::typeProperties() {
  auto Properties =
	  std::make_unique<llvm::StringMap<std::unique_ptr<VarDecl>>>();
  while (true) {
	if (check(Basic::tok::Tag::r_brace)) {
	  break;
	}

	std::unique_ptr<VarDecl> Var = nameDecl();
	if (!Var) {
	  return nullptr;
	}
	if (!expect(Basic::tok::semi)) {
	  reportExpect(Basic::tok::semi, previous());
	  return nullptr;
	}
	Properties->insert(std::pair<llvm::StringRef, std::unique_ptr<VarDecl>>(
		Var->getName(), std::move(Var)));
  }

  return std::make_unique<TypeProperties>(std::move(Properties));
}

std::unique_ptr<VarDecl> Parser::nameDecl() {
  std::unique_ptr<TypeUse> T = type();
  if (!T) {
	return nullptr;
  }

  if (!expect(Basic::tok::identifier)) {
	return nullptr;
  }
  Token ID = previous();
  return semantics->actOnNameDecl(std::move(T), ID);
}

std::unique_ptr<FunctionNode> Parser::function() {
  std::unique_ptr<TypeUse> TypeNode = type();
  if (!TypeNode) {
	return nullptr;
  }
  if (!expect(Basic::tok::identifier)) {
	return nullptr;
  }
  Token Id = previous();

  if (!expect(Basic::tok::Tag::l_paren)) {
	return nullptr;
  }
  std::unique_ptr<ArgsList> Args = arguments();

  if (!Args) {
	return nullptr;
  }
  if (!expect(Basic::tok::Tag::r_paren)) {
	return nullptr;
  }
  semantics->enterFunction(std::move(TypeNode), *Args); // borrow
  std::unique_ptr<CompoundStmt> Compound = compoundStmt();
  if (!Compound) {
	return nullptr;
  }
  TypeNode = semantics->exitFunction(); // give back
  return semantics->actOnFnDecl(std::move(TypeNode), Id, std::move(Args),
								std::move(Compound));
}

std::unique_ptr<ArgsList> Parser::arguments() {
  std::vector<std::unique_ptr<VarDecl>> ArgList;
  while (true) {
	if (check(Basic::tok::Tag::r_paren)) {
	  break;
	}
	if (!ArgList.empty() && !expect(Basic::tok::Tag::comma)) {
	  return nullptr;
	}
	std::unique_ptr<VarDecl> Argument = nameDecl();
	if (!Argument) {
	  return nullptr;
	}
	ArgList.push_back(std::move(Argument));
  }
  return std::make_unique<ArgsList>(ArgList);
}

std::unique_ptr<CompoundStmt> Parser::compoundStmt() {
  if (!expect(Basic::tok::Tag::l_brace)) {
	return nullptr;
  }
  std::vector<std::unique_ptr<Stmt>> Stmts;
  while (true) {
	if (check(Basic::tok::Tag::r_brace)) {
	  advance();
	  break;
	}
	std::unique_ptr<Stmt> S;
	if (check(Basic::tok::Tag::l_brace)) {
	  advance();
	  semantics->enterScope();
	  S = compoundStmt();
	} else {
	  S = simpleStmt();
	  if (!expect(Basic::tok::semi)) {
		return nullptr;
	  }
	}
	if (!S) {
	  if (!recoverFromError(CurrentNonTerminal::STMT)) {
		return nullptr;
	  }
	} else {
	  if (S->getKind() == Stmt::SK_COMPOUND) {
		semantics->exitScope();
	  }
	  Stmts.push_back(std::move(S));
	}
  }
  return std::make_unique<CompoundStmt>(std::move(Stmts));
}

std::unique_ptr<Stmt> Parser::simpleStmt() {
  using namespace Basic;
  std::unique_ptr<Stmt> StmtInq;
  switch (peek().getTag()) {
  case Basic::tok::Tag::kw_void:
  case tok::Tag::kw_bool:
  case tok::Tag::kw_i32:
  case tok::Tag::kw_i64:
  case tok::Tag::kw_f32:
  case tok::Tag::kw_f64:
  case tok::Tag::kw_string: {
	StmtInq = declStmt();
	break;
  }
  case tok::Tag::identifier: { // TODO: this seems wrong
	if (lookahead(2).is(Basic::tok::identifier)) {
	  StmtInq = declStmt();
	} else {
	  StmtInq = expr();
	}
	break;
  }
  case tok::Tag::kw_return: {
	StmtInq = returnStmt();
	break;
  }
  case tok::Tag::kw_for: {
	StmtInq = forStmt();
	break;
  }
  case tok::Tag::kw_while:StmtInq = whileStmt();
	break;
  case tok::Tag::kw_loop:StmtInq = loopStmt();
  default:return nullptr;
  }

  if (!StmtInq) {
	return nullptr;
  }
  return StmtInq;
}

std::unique_ptr<CallArgList> Parser::callArgs() {
  std::vector<std::unique_ptr<Expr>> Args;
  while (true) {
	if (check(Basic::tok::r_paren)) {
	  break;
	}
	if (!Args.empty() && !expect(Basic::tok::Tag::comma)) {
	  return nullptr;
	}
	std::unique_ptr<Expr> ExprPtr = expr();
	if (!ExprPtr) {
	  return nullptr;
	}
	Args.push_back(std::move(ExprPtr));
  }
  return std::make_unique<CallArgList>(std::move(Args));
}
