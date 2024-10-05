#include "Parse/Parse.hpp"

std::unique_ptr<CompoundStmt> Parser::compoundStmt() {
  if (!expect(Basic::tok::Tag::l_brace)) {
    return nullptr;
  }
  llvm::SmallVector<std::unique_ptr<Stmt>> Stmts;
  while (true) {
    if (nextTokIs(Basic::tok::Tag::r_brace)) {
      advance();
      break;
    }
    std::unique_ptr<Stmt> S;
    if (nextTokIs(Basic::tok::Tag::l_brace)) {
      Semantics->enterScope();
      S = compoundStmt();
    } else {
      S = simpleStmt();
      if (llvm::isa<Expr>(S.get())) {
        if (!expect(Basic::tok::semi)) {
          return nullptr;
        }
      }
    }
    if (!S) {
      return nullptr;
    } else {
      if (llvm::isa<CompoundStmt>(S.get())) {
        Semantics->exitScope();
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
  case tok::Tag::identifier: {// TODO: this seems wrong
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
  case tok::Tag::kw_if: {
    StmtInq = ifStmt();
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
    break;
  case tok::Tag::kw_break:
    Semantics->actOnBreakStmt(advance());
    break;
  case tok::Tag::kw_next:
    Semantics->actOnNextStmt(advance());
    break;
  default: return nullptr;
  }

  if (!StmtInq) {
    return nullptr;
  }
  return StmtInq;
}

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
  if (nextTokIs(Basic::tok::kw_else)) {
    advance();
    ElseBlock = compoundStmt();
  }

  return Semantics->actOnIfStmt(IfLoc,
                                Condition->getEndLoc(),
                                std::move(Condition),
                                std::move(Compound),
                                nullptr,
                                std::move(ElseBlock));
}

std::unique_ptr<loopStmt> Parser::loopStmt() {
  Token LoopTok = advance();
  std::unique_ptr<CompoundStmt> Compound = compoundStmt();
  if (!Compound) {
    return nullptr;
  }
  return Semantics->actOnLoopStmt(std::move(Compound), LoopTok.getLoc());
}

std::unique_ptr<forStmt> Parser::forStmt() {
  Token ForTok = advance();
  assert(ForTok.is(Basic::tok::kw_for) && "first token in forStmt should be for");

  llvm::SMLoc ForLoc = ForTok.getLoc();
  if (!expect(Basic::tok::Tag::l_paren)) {
    return nullptr;
  }

  std::unique_ptr<Stmt> Init = nullptr;
  Semantics->enterLoopScope();
  if (!nextTokIs(Basic::tok::semi)) {
    if (peek().isBaseType()) {
      Init = declStmt();
    } else if (nextTokIs(Basic::tok::identifier)) {
      if (lookahead(2).is(Basic::tok::identifier)) {
        Init = declStmt();// id id
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

  return Semantics->actOnForStmt(std::move(Init), std::move(Cond),
                                 std::move(Inc), std::move(Compound), ForLoc,
                                 RParenLoc);
}

std::unique_ptr<whileStmt> Parser::whileStmt() {
  Token WhileTok = advance();
  assert(WhileTok.is(Basic::tok::kw_while) && "Not a while token in whileStmt()!");

  std::unique_ptr<Expr> Condition = expr();

  if (!Condition) {
    return nullptr;
  }

  std::unique_ptr<CompoundStmt> Compound = compoundStmt();

  if (!Compound) {
    return nullptr;
  }

  return Semantics->actOnWhileStmt(std::move(Condition), std::move(Compound),
                                   WhileTok.getLoc(), Condition->getEndLoc());
}

std::unique_ptr<DeclStmt> Parser::declStmt() {
  std::unique_ptr<VarDecl> D = nameDecl();
  if (!D) {
    return nullptr;
  }

  if (nextTokIs(Basic::tok::Tag::semi)) {
    return Semantics->actOnVarDeclStmt(std::move(D), nullptr,
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

  return Semantics->actOnVarDeclStmt(std::move(D), std::move(ExprNode),
                                     previous().getLoc());
}

std::unique_ptr<ReturnStmt> Parser::returnStmt() {
  Token ReturnToken = advance();
  std::unique_ptr<Expr> ExprNode = nullptr;
  if (!nextTokIs(Basic::tok::semi)) {
    ExprNode = expr();
    if (!ExprNode) {
      return nullptr;
    }
  }

  if (!expect(Basic::tok::semi)) {
    return nullptr;
  }

  llvm::SMLoc SemicolonLoc = previous().getLoc();
  return Semantics->actOnReturnStmt(ReturnToken.getLoc(), SemicolonLoc, std::move(ExprNode));
}
