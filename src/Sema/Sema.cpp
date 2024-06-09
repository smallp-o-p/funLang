#include "Sema.hpp"

bool funLang::SemaAnalyzer::actOnReturnStmt(Expr &retExpr) {
  if (!currentFnRetType || !retExpr.getType()) { // poison
	return true;
  }
  if (retExpr.getType()->getName() != currentFnRetType->getType().getName()) {
	diags->emitDiagMsg(retExpr.getLoc(),
					   diag::err_incompatible_ret,
					   retExpr.getType()->getName(),
					   currentFnRetType->getType().getName());
	return false;
  }
  return true;
}

bool funLang::SemaAnalyzer::actOnUnaryOp(UnaryOp &unary) {
  switch (unary.getOp()) {
//  case Basic::Op::access: {
//	if (!unary.getExprInput().getType()) {
//	  return false;
//	}
//	if (!unary.getExprInput().getType()->getProperties()) {
//	  diags->emitDiagMsg(unary.getLoc(), diag::err_access_member_of_type_without_members, unary.getType()->getName());
//	  return false;
//	}
//	NameUsage *Name = llvm::dyn_cast<NameUsage>(&unary.getExprInput());
//	assert(Name && "Null pointer in Name in actOnUnaryOp");
//	if (auto *StructMember = unary.getType()->getMember(Name->getLexeme())) {
//	  unary.setType(&StructMember->getTypeUse().getType());
//	  return true;
//	} else {
//
//	  diags->emitDiagMsg(unary.getExprInput().getLoc(),
//						 diag::err_member_does_not_exist,
//						 unary.getType()->getName(),
//						 Name->getLexeme());
//	  return false;
//	}
//  }
  case Basic::Op::unaryMinus:
  case Basic::Op::preInc:;
  case Basic::Op::preDec: {
	if (!unary.getExprInput().getType()->isTypeOfMany({"i32", "i64", "f32", "f64"})) {
	  diags->emitDiagMsg(unary.getLoc(),
						 diag::err_unary_op_incompatible,
						 Basic::Op::getUnaryOpSpelling(unary.getOp()),
						 unary.getExprInput().getType()->getName());

	  return false;
	}
	unary.setType(unary.getExprInput().getType());
	return true;
  }
  case Basic::Op::lNot: {
	if (!unary.getType()->eq(*getBaseType("bool"))) {
	  diags->emitDiagMsg(unary.getLoc(),
						 diag::err_unary_op_incompatible,
						 Basic::Op::getUnaryOpSpelling(unary.getOp()),
						 unary.getType()->getName());
	  return false;
	}
	return true;
  };
  default: return true; // don't report error
  }
}

bool funLang::SemaAnalyzer::actOnBinaryOp(BinaryOp &bin) {
}

bool funLang::SemaAnalyzer::actOnFnCall(FunctionCall &fnCall) {
  Decl *FunctionLookup = lookup(fnCall.getName());
  if (!FunctionLookup) {
	diags->emitDiagMsg(fnCall.getLoc(), diag::err_fn_not_found, fnCall.getName());
	return false;
  }
  auto *FunctionCasted = llvm::dyn_cast<FunctionNode>(FunctionLookup);
  assert(FunctionCasted && "Function cast from Decl lookup did not work");
  if (FunctionCasted->getArgDecls().size() != fnCall.getArgs()->getSize()) {
	diags->emitDiagMsgRange(fnCall.getLoc(), fnCall.getArgs()->getArgsVec().back()->getLoc(),
							diag::err_wrong_number_of_parameters,
							FunctionCasted->getName(),
							std::to_string(FunctionCasted->getArgDecls().size()),
							std::to_string(fnCall.getArgs()->getSize()));
	return false;
  }
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
  assert(identifier.getTag() == Basic::tok::Tag::identifier && "Trying to lookup a tag that isn't an identifier");
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

bool funLang::SemaAnalyzer::actOnTopLevelDecl(Decl &TopLDecl) {
  if (Decl *search = lookup(TopLDecl.getName())) {
	diags->emitDiagMsg(TopLDecl.getLoc(), diag::err_toplevel_redefinition, search->getName());
	diags->emitDiagMsg(search->getLoc(), diag::note_var_redefinition, search->getName());
	return false;
  }
  currentScope->insert(&TopLDecl);
  return true;
}

// check for the expr is done in actOnStructDecl
bool funLang::SemaAnalyzer::actOnStructVarDecl(VarDeclStmt &DeclStmt) {
  if (Decl *Found = lookupOneScope(DeclStmt.getName())) {
	diags->emitDiagMsg(DeclStmt.getLoc(), diag::err_struct_var_redefinition, DeclStmt.getName());
	diags->emitDiagMsg(Found->getLoc(), diag::note_var_redefinition, Found->getName());
	return false;
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

  auto IntLiteral = new TypeDecl("integer literal");
  auto FloatLiteral = new TypeDecl("floating literal");

  baseTypeTable->insert(boolType);
  baseTypeTable->insert(i32Type);
  baseTypeTable->insert(i64Type);
  baseTypeTable->insert(f32Type);
  baseTypeTable->insert(f64Type);
  baseTypeTable->insert(stringType);
  baseTypeTable->insert(voidType);
  baseTypeTable->insert(IntLiteral);
  baseTypeTable->insert(FloatLiteral);
}

void funLang::SemaAnalyzer::actOnFnArgsList(ArgsList &args) {
  for (auto &arg : args.getArgList()) {
	currentScope->insert(arg.get());
  }
}

Decl *funLang::SemaAnalyzer::lookup(llvm::StringRef var) {
  std::shared_ptr<Scope> s = currentScope;
  while (s) {
	Decl *found = s->find(var);
	if (found) {
	  return found;
	} else {
	  s = s->getParent();
	}
  }
  return nullptr;
}

bool funLang::SemaAnalyzer::actOnStructDecl(TypeDecl &StructDecl) {
  auto *Properties = StructDecl.getProperties();
  bool Success = true;
  for (auto &StructMember : Properties->getDecls()) {
	auto &Val = StructMember.getValue();
	if (Val->getExpr()) {
	  diags->emitDiagMsg(Val->getLoc(),
						 diag::err_struct_var_initialization,
						 Val->getName(),
						 StructDecl.getName());
	  Success = false;
	}
  }
  return Success;
}
