#include "Parse/Parse.hpp"
#include "AST/AST.hpp"
#include "Basic/Basic.hpp"
#include <initializer_list>
#include <llvm/ADT/APFloat.h>
#include <llvm/Support/Error.h>
#include <memory>
#include <utility>

Token Parser::peek() { return lexer->peek(); }
bool Parser::check(Basic::tok::Tag Tok) { return peek().getTag() == Tok; }
Token Parser::previous() { return lexer->previous(); }
Token &Parser::advance() { return lexer->advance(); }
bool Parser::isOneOf(std::initializer_list<Basic::tok::Tag> ToExpect,
                     bool Peeking = true) {
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
      semantics->actOnStructDecl(*llvm::dyn_cast<TypeDecl>(TopLevel.get()));
    } else if (isOneOf({Basic::tok::Tag::identifier}) || peek().isBaseType()) {
      TopLevel = function();
    }
    if (!TopLevel) {
      return nullptr;
    }
    if (semantics->actOnTopLevelDecl(
            *TopLevel)) { // check if top level name has been defined before
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
  semantics->exitScope();
  if (!Members) {
    return nullptr;
  }

  return std::make_unique<TypeDecl>(Id.getLexeme(), std::move(Members),
                                    Id.getLoc());
}

std::unique_ptr<TypeProperties> Parser::typeProperties() {
  llvm::StringMap<std::unique_ptr<VarDeclStmt>> Properties;
  while (true) {
    if (check(Basic::tok::Tag::r_brace)) {
      break;
    }
    std::unique_ptr<VarDeclStmt> VarDecl = declStmt();
    if (!VarDecl) {
      return nullptr;
    }
    if (semantics->actOnStructVarDecl(*VarDecl)) {
      if (!expect(Basic::tok::semi)) {
        reportExpect(Basic::tok::semi, previous());
        return nullptr;
      }
      Properties.insert(
          std::pair<llvm::StringRef, std::unique_ptr<VarDeclStmt>>(
              VarDecl->getName(), std::move(VarDecl)));
    }
  }

  return std::make_unique<TypeProperties>(std::move(Properties));
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
  std::vector<std::unique_ptr<ArgDecl>> ArgList;
  while (true) {
    if (check(Basic::tok::Tag::r_paren)) {
      break;
    }
    if (!ArgList.empty() && !expect(Basic::tok::Tag::comma)) {
      return nullptr;
    }
    std::unique_ptr<ArgDecl> Argument = arg();
    if (!Argument) {
      return nullptr;
    }
    ArgList.push_back(std::move(Argument));
  }
  return std::make_unique<ArgsList>(ArgList);
}

std::unique_ptr<ArgDecl> Parser::arg() {
  std::unique_ptr<TypeUse> TNode = type();
  if (!TNode) {
    return nullptr;
  }
  if (!expect(Basic::tok::Tag::identifier)) {
    return nullptr;
  }
  Token Id = previous();

  return std::make_unique<ArgDecl>(std::move(TNode), Id.getLexeme(),
                                   Id.getLoc());
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
    if (lookahead(2).is(Basic::tok::equal) ||
        lookahead(2).is(Basic::tok::semi)) {
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
  case tok::Tag::kw_while:
    StmtInq = whileStmt();
    break;
  case tok::Tag::kw_loop:
    StmtInq = loopStmt();
  default:
    return nullptr;
  }

  if (!StmtInq) {
    return nullptr;
  }
  return StmtInq;
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
  std::unique_ptr<TypeUse> TypeNode = type();
  if (!TypeNode) {
    return nullptr;
  }
  if (!expect(Basic::tok::Tag::identifier)) {
    return nullptr;
  }

  Token Id = previous();

  if (check(Basic::tok::Tag::semi)) {
    return semantics->actOnVarDeclStmt(std::move(TypeNode), Id, nullptr);
  }
  if (!expect(Basic::tok::Tag::equal)) {
    return nullptr;
  }
  std::unique_ptr<Expr> ExprNode = expr();

  if (!ExprNode) {
    return nullptr;
  }

  return semantics->actOnVarDeclStmt(std::move(TypeNode), Id,
                                     std::move(ExprNode));
}

std::unique_ptr<ReturnStmt> Parser::returnStmt() {
  Token ReturnToken = advance();
  if (!ReturnToken.is(Basic::tok::kw_return)) {
    return nullptr;
  }
  std::unique_ptr<Expr> ExprNode = nullptr;
  if (!check(Basic::tok::semi)) {
    ExprNode = expr();
    if (!ExprNode) {
      return nullptr;
    }
  }
  return semantics->actOnReturnStmt(ReturnToken.getLoc(), std::move(ExprNode));
}

std::unique_ptr<Expr> Parser::expr() { return assign(); }

std::unique_ptr<Expr> Parser::assign() {
  std::unique_ptr<Expr> EqNode = eqExpr();
  Basic::Op::Binary Opcode = Basic::Op::Binary::BO_equals;
  if (isOneOf({Basic::tok::Tag::equal, Basic::tok::Tag::plusequal,
               Basic::tok::Tag::minusequal, Basic::tok::Tag::starequal,
               Basic::tok::Tag::slashequal})) {
    switch (advance().getTag()) {
    case Basic::tok::Tag::equal:
      Opcode = Basic::Op::Binary::BO_assign;
      break;
    case Basic::tok::Tag::plusequal:
      Opcode = Basic::Op::Binary::BO_plusassign;
      break;
    case Basic::tok::Tag::minusequal:
      Opcode = Basic::Op::Binary::BO_minusassign;
      break;
    case Basic::tok::Tag::starequal:
      Opcode = Basic::Op::Binary::BO_multassign;
      break;
    case Basic::tok::Tag::slashequal:
      Opcode = Basic::Op::Binary::BO_divassign;
      break;
    default:
      return nullptr;
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
    case Basic::tok::Tag::less:
      Opcode = Basic::Op::BO_lt;
      break;
    case Basic::tok::Tag::lessequal:
      Opcode = Basic::Op::BO_ltequals;
      break;
    case Basic::tok::Tag::greater:
      Opcode = Basic::Op::BO_gt;
      break;
    case Basic::tok::Tag::greaterequal:
      Opcode = Basic::Op::BO_gtequals;
      break;
    default:
      return nullptr;
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
    case tok::plusplus:
      Opcode = Basic::Op::Unary::UO_preInc;
      break;
    case tok::exclaim:
      Opcode = Basic::Op::Unary::UO_lNot;
      break;
    case tok::minusminus:
      Opcode = Basic::Op::Unary::UO_preDec;
      break;
    case tok::minus:
      Opcode = Basic::Op::Unary::UO_unaryMinus;
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
    default:
      return nullptr;
    case Basic::tok::numeric_constant: {
      return semantics->actOnIntegerLiteral(Token);
    }
    case Basic::tok::floating_constant: {
      return semantics->actOnFloatingLiteral(Token);
    }
    case Basic::tok::string_literal: {
      auto StrLit = std::make_unique<StringLiteral>(
          Token.getLexeme().size(), Token.getLexeme(), Token.getLoc());
      return StrLit;
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
