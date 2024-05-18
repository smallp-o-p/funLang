#include "Sema.hpp"

bool funLang::SemaAnalyzer::actOnReturnStmt(Expr &retExpr) {
  if (!currentFnRetType) {
	return true;
  }
  if (!retExpr.getType()) { // poison type
	return true;
  }
  if (retExpr.getType()->getName()!=currentFnRetType->getType().getName()) {
	diags->emitDiagMsg(retExpr.getLoc(),
					   diag::err_incompatible_ret,
					   retExpr.getType()->getName(),
					   currentFnRetType->getType().getName());
	return false;
  }
  return false;
}

bool funLang::SemaAnalyzer::actOnUnaryOp(UnaryOp &unary) {
}

bool funLang::SemaAnalyzer::actOnBinaryOp(BinaryOp &bin) {
}

bool funLang::SemaAnalyzer::actOnFnCall(FunctionCall &fnCall) {
}

void funLang::SemaAnalyzer::actOnVarDeclStmt(VarDeclStmt &declStmt) {
  if (!currentScope->insert(declStmt.toDecl())) {
	diags->emitDiagMsg(declStmt.getLoc(), diag::err_var_redefinition, declStmt.getName());
	Decl *look = lookup(declStmt.getName());
	diags->emitDiagMsg(look->getLoc(), diag::note_var_redefinition, look->getName());
  }
}

void funLang::SemaAnalyzer::exitScope() {
  assert(currentScope->getParent() && "Attempted to exit global scope.\n");
  currentScope = currentScope->getParent();
}

void funLang::SemaAnalyzer::enterScope() {
  std::shared_ptr<Scope> innerScope = std::make_shared<Scope>(currentScope);
  currentScope = innerScope;
}

bool funLang::SemaAnalyzer::actOnNameUsage(Token &identifier) {
  assert(identifier.getTag()==Basic::tok::Tag::identifier && "Trying to lookup a tag that isn't an identifier");
  if (!lookup(identifier.getLexeme())) {
	diags->emitDiagMsg(identifier.getLoc(), diag::err_var_not_found, identifier.getLexeme());
	return false;
  }
  return true;
}

bool funLang::SemaAnalyzer::actOnFnDecl(FunctionNode &fn) {
  if (Decl *same = lookup(fn.getName())) {
	diags->emitDiagMsg(fn.getLoc(), diag::err_var_redefinition);
	diags->emitDiagMsg(same->getLoc(), diag::note_var_redefinition, same->getName());
	return false;
  }
  return true;
}
void funLang::SemaAnalyzer::enterFunction(std::unique_ptr<TypeUse> retType) {
  currentFnRetType = std::move(retType);
}

std::unique_ptr<TypeUse> funLang::SemaAnalyzer::exitFunction() {
  return std::move(currentFnRetType);
}

bool funLang::SemaAnalyzer::actOnTopLevelDecl(Decl &decl) {
  if (Decl *search = lookup(decl.getName())) {
	diags->emitDiagMsg(decl.getLoc(), diag::err_toplevel_redefinition, search->getName());
	diags->emitDiagMsg(search->getLoc(), diag::note_var_redefinition, search->getName());
	return false;
  }
  currentScope->insert(&decl);
  return true;
}
