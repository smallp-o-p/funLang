module Parse;
namespace funLang {

Parser::StmtResult Parser::compoundStmt() {
  if (!expect(tok::Tag::l_brace)) {
    return StmtResult::InvalidRes();
  }

  Semantics.enterBlockScope();
  llvm::SmallVector<u_ptr<Stmt>> Stmts{};
  while (true) {
    if (nextTokIs(tok::Tag::r_brace)) {
      std::ignore = advance();
      break;
    }

    auto S = [&] {
      if (nextTokIs(tok::Tag::l_brace)) {
        return compoundStmt();
      }
      return stmt();
    }();

    if (!S) {
      return StmtResult::InvalidRes();
    }
    Stmts.push_back(S.move());
  }
  Semantics.popScope();

  return StmtResult<CompoundStmt>(std::move(Stmts));
}

Parser::StmtResult Parser::stmt() {
  using namespace Basic;
  auto S = [&] {
    switch (peek().getTag()) {
    case tok::Tag::kw_void:
    case tok::Tag::kw_bool:
    case tok::Tag::kw_i32:
    case tok::Tag::kw_i64:
    case tok::Tag::kw_f32:
    case tok::Tag::kw_f64:
    case tok::Tag::kw_string: {
      return declStmt();
    }
    case tok::Tag::identifier: {
      if (lookahead(2).is(tok::identifier)) {
        return declStmt();
      }
      return exprStmt();
    }
    case tok::Tag::kw_return: {
      return returnStmt();
    }
    case tok::Tag::kw_if: {
      return ifStmt();
    }
    case tok::Tag::kw_for: {
      return forStmt();
    }
    case tok::Tag::kw_while: return whileStmt();
    case tok::Tag::kw_loop: return parseLoop();
    case tok::Tag::kw_next:
    case tok::Tag::kw_break:
      const auto T = advance();
      Semantics.checkBreakOrNextUse(T);
      if (T.is(tok::Tag::kw_break)) {
        return StmtResult(BreakStmt::Create(T.getLoc(), T.getRightmostLoc()));
      }
      return StmtResult(NextStmt::Create(T.getLoc(), T.getRightmostLoc()));
    case tok::Tag::semi:
      return StmtResult(EmptyStmt::Create(advance().getLoc()));
    default: return StmtError();
    }
  }();
  if (!S) {
    return StmtError();
  }

  if (isa<DeclStmt>(S.get())) {
    if (!expect(tok::Tag::semi)) {
      return StmtError();
    }
  }

  return S;
}

Parser::StmtResult Parser::ifStmt() {
  if (!expect(tok::Tag::kw_if)) {
    return StmtError();
  }
  const SourceLoc IfLoc = previous().getLoc();

  ExprResult Condition = expr();
  if (!Condition) {
    return StmtError();
  }

  StmtResult Compound = compoundStmt();
  if (!Compound) {
    return StmtError();
  }

  StmtResult ElseBlock = StmtResult::ValidEmptyRes();
  if (nextTokIs(tok::kw_else)) {
    std::ignore = advance();
    ElseBlock = compoundStmt();
  }

  if (!ElseBlock) {
    return StmtError();
  }

  Semantics.checkIfStmt(Condition.get());// don't worry about elif yet
  return StmtResult(ifStmt::Create(IfLoc, Condition.move(),
                                   Compound.moveAs<CompoundStmt>(), nullptr,
                                   ElseBlock.moveAs<CompoundStmt>()));
}

Parser::StmtResult Parser::exprStmt() {
  ExprResult E = expr();
  if (!E) {
    return StmtError();
  }
  if (!expect(tok::Tag::semi)) {
    return StmtError();
  }

  return StmtResult(E.moveAs<Stmt>());
}

Parser::StmtResult Parser::parseLoop() {
  if (!expect(tok::Tag::kw_loop)) {
    return StmtError();
  }

  const SourceLoc LoopLoc = previous().getLoc();
  StmtResult Compound = compoundStmt();

  if (!Compound) {
    return StmtError();
  }
  const SourceLoc End = Compound.get()->getEndLoc();

  return StmtResult<loopStmt>(Compound.move(), LoopLoc, End);
}

Parser::StmtResult Parser::forStmt() {
  if (!expect(tok::Tag::kw_for)) {
    return StmtError();
  }

  SourceLoc ForLoc = previous().getLoc();

  if (!expect(tok::Tag::l_paren)) {
    return StmtError();
  }
  Semantics.enterLoopScope();
  StmtResult Init = [&] {
    if (!nextTokIs(tok::semi)) {
      if (peek().isBaseType()) {
        return declStmt();
      }
      if (nextTokIs(tok::identifier)) {
        if (lookahead(2).is(tok::identifier)) {
          return declStmt();// id id
        }
      }
      return StmtResult(expr());
    }
    return StmtResult::ValidEmptyRes();
  }();

  if (!Init) {
    return StmtError();
  }

  if (!expect(tok::semi)) {
    return StmtError();
  }

  ExprResult Cond = expr();

  if (!Cond) {
    return StmtError();
  }
  if (!expect(tok::Tag::semi)) {
    return StmtError();
  }

  ExprResult Inc = expr();

  if (!Inc) {
    return StmtError();
  }

  if (!expect(tok::Tag::r_paren)) {
    return StmtError();
  }

  const SourceLoc RParenLoc = previous().getLoc();

  StmtResult Compound = compoundStmt();
  if (!Compound) {
    return StmtError();
  }

  Semantics.checkForStmt(Cond.get());

  return StmtResult(forStmt::Create(ForLoc, RParenLoc, Init.move(), Cond.move(),
                                    Inc.move(),
                                    Compound.moveAs<CompoundStmt>()));
}

Parser::StmtResult Parser::whileStmt() {
  if (!expect(tok::Tag::kw_while)) {
    return StmtError();
  }

  SourceLoc WhileLoc = previous().getLoc();

  ExprResult Condition = expr();

  if (!Condition) {
    return StmtError();
  }

  StmtResult Compound = compoundStmt();

  if (!Compound) {
    return StmtError();
  }

  Semantics.checkWhileStmt(Condition.get());

  return StmtResult(whileStmt::Create(WhileLoc, WhileLoc, Condition.move(),
                                      Compound.moveAs<CompoundStmt>()));
}

Parser::StmtResult Parser::declStmt() {
  DeclResult D = nameDecl();
  if (!D) {
    return StmtError();
  }

  VarDecl *Var = D.getAs<VarDecl>();

  if (nextTokIs(tok::Tag::semi)) {
    Semantics.handleVarDeclStmt(D.moveAs<VarDecl>(), nullptr);
    return StmtResult<DeclStmt>(Var, nullptr);
  }

  if (!expect(tok::Tag::equal)) {
    return StmtError();
  }

  ExprResult ExprNode = expr();
  if (!ExprNode) {
    return StmtError();
  }

  Semantics.handleVarDeclStmt(D.moveAs<VarDecl>(), ExprNode.get());
  return StmtResult<DeclStmt>(Var, std::move(ExprNode));
}

Parser::StmtResult Parser::returnStmt() {
  if (!expect(tok::Tag::kw_return)) {
    return StmtError();
  }
  const Token ReturnToken = previous();

  auto ExprNode = ExprResult::ValidEmptyRes();
  if (!nextTokIs(tok::semi)) {
    ExprNode = expr();
    if (!ExprNode) {
      return StmtError();
    }
  }

  if (!expect(tok::semi)) {
    return StmtError();
  }

  const auto SemicolonLoc = previous().getLoc();
  Semantics.checkReturnStmt(ExprNode.get());

  return StmtResult(
      ReturnStmt::Create(ExprNode.move(), ReturnToken.getLoc(), SemicolonLoc));
}
}// namespace funLang
