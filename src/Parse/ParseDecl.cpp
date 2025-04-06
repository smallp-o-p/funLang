#include <llvm/Support/Error.h>
module Parse;
import Basic;
namespace funLang {

Parser::CompileResult Parser::program() {
  auto Compilation = CompilationUnit::Init();
  Semantics.initSemaAnalyzer(Compilation.get());
  while (!Lexer_.atEnd()) {
    DeclResult Toplevel;
    if (nextTokIs(tok::kw_struct))
      Toplevel = typeDecl();
    else if (nextTokIs(tok::identifier) || peek().isBaseType()) {
      Toplevel = functionDecl();
    } else {
      Toplevel = DeclResult::InvalidRes();
      // todo: make error message
    }
    if (!Toplevel) {
      return CompileResult::InvalidRes();
    }
    Semantics.actOnTopLevelDecl(Toplevel.move());
  }
  return CompileResult(std::move(Compilation));
}

Parser::DeclResult Parser::typeDecl() {
  if (!expect(tok::identifier)) {
    return DeclResult::InvalidRes();
  }
  Token StructName = previous();

  if (!expect(tok::l_brace)) {
    return DeclResult::InvalidRes();
  }
  auto NewType = Semantics.enterStructScope(StructName);
  while (true) {
    if (nextTokIs(tok::r_brace)) {
      break;
    }
    DeclResult Field = nameDecl();
    if (!Field) {
      skipUntil(tok::semi, tok::r_brace);
      return DeclResult::InvalidRes();
    }
    Semantics.actOnStructMemberDecl(Field.move());
    if (!expect(tok::semi)) {
      skipUntil(tok::semi, tok::r_brace);
      return DeclResult::InvalidRes();
    }
  }
  return DeclResult(std::move(NewType));
}

Parser::DeclResult Parser::functionDecl() {
  auto TypeUseRes = typeUse();
  if (!TypeUseRes) {
    goto invalid_res;
  }

  auto NameAndParams = fnNameAndParams();
  if (!NameAndParams) {
    goto invalid_res;
  }

  auto FnDecl =
      Semantics.beginFunctionDecl(TypeUseRes.move(), NameAndParams.get()->first,
                                  std::move(NameAndParams.get()->second));
  auto Compound = compoundStmt();

  if (!Compound) {
    goto invalid_res;
  }

  Semantics.endFunctionDecl(FnDecl.get(), Compound.move());
  return DeclResult(std::move(FnDecl));

invalid_res:
  return DeclResult::InvalidRes();
}

ActionRes<std::pair<Symbol *, u_ptr<ParamDecl>>> Parser::fnNameAndParams() {
  using Res = ActionRes<std::pair<Symbol *, u_ptr<ParamDecl>>>;
  if (!expect(tok::identifier) && !expect(tok::Tag::l_paren)) {
    return Res::InvalidRes();
  }
  const Token Id = previous();

  auto Args = parameters();
  if (!Args && !expect(tok::Tag::r_paren)) {
    return Res::InvalidRes();
  }

  return Res(Id.getIdentifierTableEntry(), Args.move());
}

Parser::DeclResult Parser::nameDecl() {
  auto TyUse = typeUse();

  if (!TyUse) {
    return DeclError();
  }
  const auto Name = advance();
  if (!Name.isBaseType() && !Name.isIdentifier()) {
    return DeclError();
  }

  if (!expect(tok::Tag::identifier)) {
    return DeclError();
  }

  return DeclResult<VarDecl>(Decl::DK_VAR, TyUse.move(), Name.getIdentifier());
}

Parser::DeclResult Parser::paramDecl() {
  auto namedDecl = nameDecl();
  if (!namedDecl) {
    return DeclResult::InvalidRes();
  }

  if (!peek().is(tok::Tag::equal)) {
    return DeclResult<ParamDecl>(std::move(namedDecl), nullptr);
  }

  std::ignore = advance();
  auto Exp = expr();

  if (!Exp) {
    return DeclResult::InvalidRes();
  }

  return DeclResult<ParamDecl>(namedDecl, Exp.move());
}

Parser::DeclResult Parser::parameters() {
  llvm::SmallDenseMap<const Symbol *, Decl *> ParamMap{};
  u_ptr<Decl> DeclChainBegin = nullptr;
  Decl *DeclLast = nullptr;
  while (!nextTokIs(tok::Tag::r_paren)) {
    if (ParamMap.size() > 0 and not previous().is(tok::Tag::comma)) {
      return DeclResult::InvalidRes();
    }
    auto Argument = paramDecl();
    if (!Argument) {
      return DeclResult::InvalidRes();
    }

    Semantics.actOnParamDecl(ParamMap, Argument.get());
    if (!DeclChainBegin) [[unlikely]] {// only happens once
      DeclChainBegin = Argument.move();
      DeclLast = DeclChainBegin.get();
    } else {
      DeclLast->setNext(Argument.move());
      DeclLast = DeclLast->getNext();
    }

    if (nextTokIs(tok::Tag::comma)) {// allow trailing comma
      std::ignore = advance();
    }
  }

  return DeclResult(std::move(DeclChainBegin));
}
}// namespace funLang
