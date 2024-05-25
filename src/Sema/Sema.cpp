#include "Sema.hpp"

bool funLang::SemaAnalyzer::actOnReturnStmt(Expr &retExpr) {
  if (!currentFnRetType || !retExpr.getType()) { // poison
	return true;
  }
  if (retExpr.getType()->getName()!=currentFnRetType->getType().getName()) {
	diags->emitDiagMsg(retExpr.getLoc(),
					   diag::err_incompatible_ret,
					   retExpr.getType()->getName(),
					   currentFnRetType->getType().getName());
	return false;
  }
  return true;
}

bool funLang::SemaAnalyzer::actOnUnaryOp(UnaryOp &unary) {
}

bool funLang::SemaAnalyzer::actOnBinaryOp(BinaryOp &bin) {
}

bool funLang::SemaAnalyzer::actOnFnCall(FunctionCall &fnCall) {
  return true;
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
	diags->emitDiagMsg(same->getLoc(),
					   diag::note_var_redefinition,
					   same->getName());
	return false;
  }
  return true;
}

/// Make return type available to parser and insert declared arguments into function scope.
void funLang::SemaAnalyzer::enterFunction(std::unique_ptr<TypeUse> retType, ArgsList &args) {
  currentFnRetType = std::move(retType);
  enterScope();

  for (auto &arg : args.getArgList()) {
	currentScope->insert(arg.get());
  }
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
bool funLang::SemaAnalyzer::actOnStructVarDecl(VarDeclStmt &declStmt) {
  if (Expr *expr = declStmt.getExpr()) {
	if (expr->getExprKind() < Expr::EXPR_INT) {
	  diags->emitDiagMsg(expr->getLoc(), diag::err_struct_var_initialization_err);
	  return false;
	} else if (!(expr->getType()->eq(declStmt.getTypeUse().getType()))) {
	  return false;
	}
	return true;
  }
  return true;
}
void funLang::SemaAnalyzer::init() {
  auto boolType = new TypeDecl("bool");
  auto i32Type = new TypeDecl("i32");
  auto i64Type = new TypeDecl("i64");
  auto f32Type = new TypeDecl("f32");
  auto f64Type = new TypeDecl("f64");
  auto stringType = new TypeDecl("string");
  auto voidType = new TypeDecl("void");

  currentScope->insert(boolType);
  currentScope->insert(i32Type);
  currentScope->insert(i64Type);
  currentScope->insert(f32Type);
  currentScope->insert(f64Type);
  currentScope->insert(stringType);
  currentScope->insert(voidType);
}
void funLang::SemaAnalyzer::actOnFnArgsList(ArgsList &args) {
  for (auto &arg : args.getArgList()) {
	currentScope->insert(arg.get());
  }
}
