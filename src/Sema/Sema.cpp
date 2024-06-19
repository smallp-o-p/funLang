#include "Sema/Sema.hpp"
#include <ranges>

bool funLang::SemaAnalyzer::actOnReturnStmt(Expr &RetExpr) {
  if (!currentFnRetType || !RetExpr.getType()) { // poison
	return true;
  }
  auto *RetExprType = RetExpr.getType();
  auto *FnRetTypeDecl = currentFnRetType->getTypeDecl();
  bool Mismatch = false;
  if (isEqualToBaseType({Basic::Data::i32, Basic::Data::i64}, FnRetTypeDecl)) {
	if (!RetExprType->eq(lookupBaseType(Basic::Data::int_literal))
		&& !RetExprType->eq(FnRetTypeDecl)) {
	  Mismatch = true;
	}
  } else if (isEqualToBaseType({Basic::Data::f32, Basic::Data::f64}, FnRetTypeDecl)) {
	if (!RetExprType->eq(lookupBaseType(Basic::Data::floating_literal)) // arg decl is f32 or f64, but passed arg is not an int literal and not equal to arg decl
		&& !RetExprType->eq(FnRetTypeDecl)) {
	  Mismatch = true;
	}
  } else {
	if (!RetExprType->eq(FnRetTypeDecl)) {
	  Mismatch = true;
	}
  }
  if (Mismatch) {
	diags->emitDiagMsg(RetExpr.getLoc(),
					   diag::err_incompatible_ret,
					   FnRetTypeDecl->getName(),
					   RetExprType->getName());
  }
  return !Mismatch;
}

bool funLang::SemaAnalyzer::actOnUnaryOp(UnaryOp &unary) {
  switch (unary.getOp()) {
//  case Basic::Op::access: {
//	if (!unary.getExprInput().getUnderlyingTypeDecl()) {
//	  return false;
//	}
//	if (!unary.getExprInput().getUnderlyingTypeDecl()->getProperties()) {
//	  diags->emitDiagMsg(unary.getLoc(), diag::err_access_member_of_type_without_members, unary.getUnderlyingTypeDecl()->getName());
//	  return false;
//	}
//	NameUsage *Name = llvm::dyn_cast<NameUsage>(&unary.getExprInput());
//	assert(Name && "Null pointer in Name in actOnUnaryOp");
//	if (auto *StructMember = unary.getUnderlyingTypeDecl()->getMember(Name->getLexeme())) {
//	  unary.setType(&StructMember->getTypeUse().getUnderlyingTypeDecl());
//	  return true;
//	} else {
//
//	  diags->emitDiagMsg(unary.getExprInput().getLoc(),
//						 diag::err_member_does_not_exist,
//						 unary.getUnderlyingTypeDecl()->getName(),
//						 Name->getLexeme());
//	  return false;
//	}
//  }
  case Basic::Op::UO_unaryMinus:
  case Basic::Op::UO_preInc:;
  case Basic::Op::UO_preDec: {
	TypeDecl *IsEq = isEqualToBaseType({Basic::Data::i32, Basic::Data::i64}, unary.getExprInput().getType());
	if (!IsEq) {
	  diags->emitDiagMsg(unary.getLoc(),
						 diag::err_unary_op_incompatible,
						 Basic::Op::getUnaryOpSpelling(unary.getOp()),
						 unary.getExprInput().getType()->getName());
	  unary.setType(nullptr);
	  return false;
	}
	unary.setType(unary.getExprInput().getType());
	return true;
  }
  case Basic::Op::UO_lNot: {
	if (!unary.getType()->eq(lookupBaseType(Basic::Data::Type::bool_))) {
	  diags->emitDiagMsg(unary.getLoc(),
						 diag::err_unary_op_incompatible,
						 Basic::Op::getUnaryOpSpelling(unary.getOp()),
						 unary.getType()->getName());
	  unary.setType(nullptr);
	  return false;
	}
	unary.setType(llvm::dyn_cast<TypeDecl>(lookupBaseType(Basic::Data::Type::bool_)));
	return true;
  };
  default: return true; // don't report error
  }
}

bool funLang::SemaAnalyzer::actOnBinaryOp(BinaryOp &bin) {}

bool funLang::SemaAnalyzer::actOnFnCall(FunctionCall &fnCall) {
  Decl *FunctionLookup = lookup(fnCall.getName());
  if (!FunctionLookup) {
	diags->emitDiagMsg(fnCall.getLoc(), diag::err_fn_not_found, fnCall.getName());
	return false;
  }
  auto *FunctionCasted = llvm::dyn_cast<FunctionNode>(FunctionLookup);
  assert(FunctionCasted && "Function cast from Decl lookup did not work");
  if (FunctionCasted->getArgDecls().size() != fnCall.getArgs()->getSize()) {
	diags->emitDiagMsg(fnCall.getLoc(),
					   diag::err_wrong_number_of_parameters,
					   FunctionCasted->getName(),
					   std::to_string(FunctionCasted->getArgDecls().size()),
					   std::to_string(fnCall.getArgs()->getSize()));
	return false;
  }
  bool Success = false;
  for (size_t i = 0; i < FunctionCasted->getArgDecls().size(); i++) {
	bool Mismatch = false;
	auto *FnCallArgType = fnCall.getArgs()->getArgsVec()[i]->getType();
	auto *FunctionArgDeclType = FunctionCasted->getArgDecls()[i]->getUnderlyingTypeDecl();

	if (isEqualToBaseType({Basic::Data::i32, Basic::Data::i64}, FunctionArgDeclType)) {
	  if (!FnCallArgType->eq(lookupBaseType(Basic::Data::int_literal)) // arg decl is i32 or i64, but passed arg is not an int literal and not equal to arg decl
		  && !FnCallArgType->eq(FunctionArgDeclType)) {
		Mismatch = true;
		Success = false;
	  }
	} else if (isEqualToBaseType({Basic::Data::f32, Basic::Data::f64}, FunctionArgDeclType)) {
	  if (!FnCallArgType->eq(lookupBaseType(Basic::Data::floating_literal)) // arg decl is f32 or f64, but passed arg is not an int literal and not equal to arg decl
		  && !FnCallArgType->eq(FunctionArgDeclType)) {
		Mismatch = true;
		Success = false;
	  }
	} else {
	  if (!FnCallArgType->eq(FunctionArgDeclType)) {
		Mismatch = true;
	  }
	}
	if (Mismatch) {
	  diags->emitDiagMsg(FnCallArgType->getLoc(),
						 diag::err_incompatible_type_passed,
						 FunctionArgDeclType->getName(),
						 FnCallArgType->getName());
	}
  }
  return Success;
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

bool funLang::SemaAnalyzer::actOnFnDecl(FunctionNode &Fn) {
  if (Decl *Same = lookup(Fn.getName())) {
	diags->emitDiagMsg(Fn.getLoc(), diag::err_var_redefinition);
	diags->emitDiagMsg(Same->getLoc(),
					   diag::note_var_redefinition,
					   Same->getName());
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

  currentScope->insert(boolType);
  currentScope->insert(i32Type);
  currentScope->insert(i64Type);
  currentScope->insert(f32Type);
  currentScope->insert(f64Type);
  currentScope->insert(stringType);
  currentScope->insert(voidType);
  currentScope->insert(IntLiteral);
  currentScope->insert(FloatLiteral);
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
bool funLang::SemaAnalyzer::actOnMultDivOp(BinaryOp &MultiplyOrDivide) {
}

bool funLang::SemaAnalyzer::actOnAddSubOp(BinaryOp &AddOrSubtract) {
  return false;
}

bool funLang::SemaAnalyzer::actOnComparisonOp(BinaryOp &CmpOp) {
  if (!CmpOp.getLhs().getType()->eq(CmpOp.getRhs().getType())) {
	diags->emitDiagMsgRange(
		CmpOp.getLhs().getLoc(),
		CmpOp.getRhs().getLoc(),
		diag::err_incompatible_binary_operands,
		CmpOp.getLhs().getType()->getName(),
		CmpOp.getRhs().getType()->getName(),
		Basic::Op::getBinaryOpSpelling(CmpOp.getOp()));
	CmpOp.setType(nullptr);
	return false;
  }
  CmpOp.setType(lookupType(Basic::Data::getBasicTypeSpelling(Basic::Data::bool_)));
  return true;
}

TypeDecl *funLang::SemaAnalyzer::lookupType(llvm::StringRef Type) {
  if (Decl *Found = lookup(Type)) {
	if (Found->getKind() != Decl::DK_TYPE) {
	  return nullptr;
	}
	return llvm::dyn_cast<TypeDecl>(Found);
  }
  return nullptr;
}

TypeDecl *funLang::SemaAnalyzer::lookupBaseType(Basic::Data::Type Type) { // wrapper function to type less
  return lookupType(Basic::Data::getBasicTypeSpelling(Type));
}

TypeDecl *funLang::SemaAnalyzer::isEqualToBaseType(std::initializer_list<Basic::Data::Type> Types, TypeDecl *Type) {
  for (Basic::Data::Type T : Types) {
	TypeDecl *Lookup = lookupType(Basic::Data::getBasicTypeSpelling(T));
	if (Lookup == Type) {
	  return Lookup;
	}
  }
  return nullptr;
}

Decl *funLang::SemaAnalyzer::lookupOneScope(llvm::StringRef varName) {
  return currentScope->find(varName);
}
