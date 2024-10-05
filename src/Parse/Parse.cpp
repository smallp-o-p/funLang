#include "Parse/Parse.hpp"
#include <llvm/ADT/APFloat.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/SMLoc.h>
#include <memory>
#include <utility>

Token Parser::peek() { return lexer->peek(); }
bool Parser::nextTokIs(Basic::tok::Tag Tok) { return lexer->peek().is(Tok); }
Token Parser::previous() { return lexer->previous(); }
Token &Parser::advance() { return lexer->advance(); }
bool Parser::expect(Basic::tok::Tag Tok) {
  if (lexer->advance().getTag() != Tok) {
    reportExpect(Tok, previous());
    return false;
  }
  return true;
}

void Parser::reportExpect(Basic::tok::Tag Expected, Token Received) {
  diags.emitDiagMsg(Received.getLoc(), Diag::err_expected,
                    Basic::tok::getTokenName(Expected), Received.getLexeme());
}

void Parser::emitWarning(unsigned int DiagId, llvm::SMLoc Loc,
                         llvm::StringRef Name) {
  diags.emitDiagMsg(Loc, DiagId, Name);
}

Token Parser::lookahead(uint32_t HowMuch) { return lexer->lookahead(HowMuch); }

std::unique_ptr<TypeUse> Parser::type() {
  size_t IndirectionCount = 0;
  while (nextTokIs(Basic::tok::star)) {
    IndirectionCount++;
    advance();
  }

  Token &TypeName = advance();
  if (!TypeName.isBaseType() && !TypeName.isIdentifier()) {
    diags.emitDiagMsg(TypeName.getLoc(), Diag::err_expected,
                      llvm::StringRef("identifier or base type"),
                      TypeName.getLexeme());
    return nullptr;
  }
  return Semantics->actOnTypeUse(TypeName, IndirectionCount);
}

std::unique_ptr<CompilationUnit> Parser::program() {
  llvm::DenseMap<IDTableEntry *, std::unique_ptr<Decl>> GlobalDecls;
  while (true) {
    std::unique_ptr<Decl> TopLevel = nullptr;
    if (nextTokIs(Basic::tok::kw_struct)) {
      TopLevel = typeDecl();
    } else if (nextTokIs(Basic::tok::Tag::identifier) || peek().isBaseType()) {
      TopLevel = function();
    }
    if (!TopLevel) {
      return nullptr;
    }
    Semantics->actOnTopLevelDecl(std::move(TopLevel));
    if (nextTokIs(Basic::tok::Tag::eof)) {
      break;
    }
  }
  Semantics->exitScope();

  return std::make_unique<CompilationUnit>(std::move(GlobalDecls));
}

std::unique_ptr<RecordDecl> Parser::typeDecl() {
  llvm::SMLoc StructLoc = advance().getLoc(), RBraceLoc;
  if (!expect(Basic::tok::identifier)) {
    return nullptr;
  }
  Token Id = previous();
  if (!expect(Basic::tok::l_brace)) {
    return nullptr;
  }
  std::unique_ptr<RecordDecl> NewDecl = Semantics->enterStructScope(Id);
  while (true) {
    if (nextTokIs(Basic::tok::r_brace)) {
      RBraceLoc = advance().getLoc();
      break;
    }
    std::unique_ptr<VarDecl> Var = nameDecl();
    if (!Var) {
      return nullptr;
    }
    Semantics->actOnStructMemberDecl(std::move(Var));
    if (!expect(Basic::tok::semi)) {
      reportExpect(Basic::tok::semi, previous());
      return nullptr;
    }
  }
  NewDecl->setRange(StructLoc, RBraceLoc);
  return std::move(NewDecl);
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
  return Semantics->actOnNameDecl(std::move(T), ID);
}

std::unique_ptr<FunctionDecl> Parser::function() {
  llvm::SMLoc RParenLoc;
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
  u_ptr<llvm::SmallVector<u_ptr<ParamDecl>>> Args = parameters();
  if (!Args) {
    return nullptr;
  }

  if (!expect(Basic::tok::Tag::r_paren)) {
    return nullptr;
  }

  RParenLoc = previous().getLoc();

  u_ptr<FunctionDecl>
      FnDecl = Semantics->enterFunctionScope(std::move(TypeNode), std::move(Args), Id, RParenLoc);
  std::unique_ptr<CompoundStmt> Compound = compoundStmt();
  if (!Compound) {
    return nullptr;
  }
  Semantics->actOnFunctionDecl(FnDecl.get(), std::move(Compound));
  return std::move(FnDecl);
}

std::unique_ptr<ParamDecl> Parser::paramDecl() {
  return nullptr;
}

u_ptr<llvm::SmallVector<u_ptr<ParamDecl>>> Parser::parameters() {
  u_ptr<llvm::SmallVector<u_ptr<ParamDecl>>> ParamsVec = std::make_unique<llvm::SmallVector<u_ptr<ParamDecl>>>();
  auto ParamsMap = llvm::DenseMap<IDTableEntry *, ParamDecl *>();
  while (true) {
    if (nextTokIs(Basic::tok::Tag::r_paren)) {
      break;
    }
    if (!ParamsVec->empty() && !expect(Basic::tok::Tag::comma)) {
      return nullptr;
    }
    std::unique_ptr<ParamDecl> Argument = paramDecl();
    if (!Argument) {
      return nullptr;
    }
    Semantics->actOnParamDecl(*ParamsVec, std::move(Argument), ParamsMap);
  }

  return std::move(ParamsVec);
}
