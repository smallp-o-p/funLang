#include "Sema/Sema.hpp"
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/SMLoc.h>
#include <memory>
using namespace funLang;

bool funLang::SemaAnalyzer::TypesAreEqual(TypeDecl *LHS, TypeDecl *RHS) {

  assert(LHS && RHS && "LHS or RHS are nullptrs");

  if (llvm::isa<PointerType>(LHS) && llvm::isa<PointerType>(RHS)) {
	auto RHSPtr = llvm::dyn_cast<PointerType>(RHS);
	auto LHSPtr = llvm::dyn_cast<PointerType>(LHS);

	return LHSPtr->getPointee()->eq(RHSPtr->getPointee());
  }

  return LHS->eq(RHS);
}

bool funLang::SemaAnalyzer::TypesAreEqualOrCompatible(TypeDecl *LHS,
													  TypeDecl *RHS) {
  if (TypesAreEqual(LHS, RHS)) {
	return true;

  } else {
	if (!llvm::isa<BuiltInType>(LHS) && !llvm::isa<BuiltInType>(RHS)) {
	  return false;
	}
	BuiltInType *Left = llvm::dyn_cast<BuiltInType>(LHS);
	BuiltInType *Right = llvm::dyn_cast<BuiltInType>(RHS);

	return Left->isCompatible(Right);
  }
}

std::unique_ptr<forStmt> SemaAnalyzer::actOnForStmt(
	std::unique_ptr<Stmt> Init, std::unique_ptr<Expr> Cond,
	std::unique_ptr<Expr> Inc, std::unique_ptr<CompoundStmt> Body,
	llvm::SMLoc Left, llvm::SMLoc Right) {}

std::unique_ptr<whileStmt>
SemaAnalyzer::actOnWhileStmt(std::unique_ptr<Expr> Condition,
							 std::unique_ptr<CompoundStmt> Compound,
							 llvm::SMLoc Left, llvm::SMLoc Right) {
  if (!Condition->getType()) {
	return std::make_unique<whileStmt>(std::move(Condition),
									   std::move(Compound), Left, Right);
  }

  if (BuiltInType *T = llvm::dyn_cast<BuiltInType>(Condition->getType())) {
	if (!T->isBoolType()) {
	  diags->emitDiagMsgRange(Left, Right, diag::err_while_condition_not_bool);
	}
  } else {
	diags->emitDiagMsgRange(Left, Right, diag::err_while_condition_not_bool);
  }
  return std::make_unique<whileStmt>(std::move(Condition), std::move(Compound),
									 Left, Right);
}
std::unique_ptr<TypeUse> SemaAnalyzer::actOnTypeUse(Token &TypeName) {
  if (TypeDecl *FoundType = lookupType(TypeName.getLexeme())) {
	return std::make_unique<TypeUse>(FoundType, TypeName.getLoc());

  } else {
	diags->emitDiagMsg(TypeName.getLoc(), diag::err_var_not_found,
					   TypeName.getLexeme());

	return std::make_unique<TypeUse>(nullptr, TypeName.getLoc());
  }
}

std::unique_ptr<ReturnStmt>
funLang::SemaAnalyzer::actOnReturnStmt(llvm::SMLoc ReturnLoc,
									   std::unique_ptr<Expr> ReturnExpr) {
  if (!currentFnRetType || !ReturnExpr->getType()) { // poison
	return std::make_unique<ReturnStmt>(std::move(ReturnExpr));
  }
  if (!ReturnExpr) {
	BuiltInType *ShouldBeVoid =
		llvm::dyn_cast<BuiltInType>(currentFnRetType->getTypeDecl());
	if (!ShouldBeVoid) {
	  diags->emitDiagMsg(ReturnLoc, diag::err_naked_return_on_non_void_fn);
	  return std::make_unique<ReturnStmt>(std::make_unique<ErrorExpr>());
	} else {
	  if (!ShouldBeVoid->isVoidType()) {
		diags->emitDiagMsg(ReturnLoc, diag::err_return_on_void_fn);
		return std::make_unique<ReturnStmt>(std::make_unique<ErrorExpr>());
	  }
	  return std::make_unique<ReturnStmt>(nullptr);
	}
  }
  if (!TypesAreEqualOrCompatible(currentFnRetType->getTypeDecl(),
								 ReturnExpr->getType())) {
	diags->emitDiagMsg(ReturnExpr->getLeftLoc(), diag::err_incompatible_ret,
					   currentFnRetType->getTypeDecl()->getName(),
					   ReturnExpr->getType()->getName());
	return std::make_unique<ReturnStmt>(
		std::make_unique<ErrorExpr>(std::move(ReturnExpr)));
  }
  return std::make_unique<ReturnStmt>(std::move(ReturnExpr));
}

std::unique_ptr<Expr>
funLang::SemaAnalyzer::actOnUnaryOp(Basic::Op::Unary Op,
									std::unique_ptr<Expr> ExprInput) {
  if (!llvm::isa<BuiltInType>(ExprInput->getType())) {
	diags->emitDiagMsg(ExprInput->getLeftLoc(), diag::err_unary_op_incompatible,
					   Basic::Op::getUnaryOpSpelling(Op),
					   ExprInput->getType()->getName());

	return std::make_unique<UnaryOp>(
		std::make_unique<ErrorExpr>(std::move(ExprInput)), Op, nullptr);
  }

  if (!ExprInput->getType()) {
	return std::make_unique<UnaryOp>(std::move(ExprInput), Op, nullptr);
  }

  bool Mismatch = false;
  TypeDecl *resultType = nullptr;
  BuiltInType *BuiltIn = llvm::dyn_cast<BuiltInType>(ExprInput->getType());
  switch (Op) {
  case Basic::Op::UO_unaryMinus: {
	if (!BuiltIn->isNumericType()) {
	  Mismatch = true;
	}
	break;
  }
  case Basic::Op::UO_preInc:
  case Basic::Op::UO_preDec: {
	if (!BuiltIn->isIntType()) {
	  Mismatch = true;
	}
	break;
  }
  case Basic::Op::UO_lNot: {
	if (!BuiltIn->isBoolType()) {
	  Mismatch = true;
	}
	break;
  }
  case Basic::Op::NUM_UNARY:llvm_unreachable("Invalid unary operator");
	break;
  }
  if (Mismatch) {
	diags->emitDiagMsg(ExprInput->getLeftLoc(), diag::err_unary_op_incompatible,
					   Basic::Op::getUnaryOpSpelling(Op),
					   ExprInput->getType()->getName());

	return std::make_unique<UnaryOp>(
		std::make_unique<ErrorExpr>(std::move(ExprInput)), Op, nullptr);
  }

  resultType = ExprInput->getType();
  return std::make_unique<UnaryOp>(std::move(ExprInput), Op, resultType);
}

std::unique_ptr<Expr> SemaAnalyzer::actOnBinaryOp(std::unique_ptr<Expr> LHS,
												  Basic::Op::Binary Op,
												  std::unique_ptr<Expr> RHS) {
  if (!LHS->getType() || !LHS->getType()) {
	return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
		std::move(LHS), std::move(RHS), Op, nullptr));
  }

  if (!llvm::isa<BuiltInType>(LHS.get()->getType()) ||
	  !llvm::isa<BuiltInType>(RHS.get()->getType())) {
	diags->emitDiagMsgRange(
		LHS->getLeftLoc(), RHS->getLeftLoc(),
		diag::err_incompatible_binary_operands, LHS->getType()->getName(),
		RHS->getType()->getName(), Basic::Op::getBinaryOpSpelling(Op));
	return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
		std::move(LHS), std::move(RHS), Op, nullptr));
  }

  if (!TypesAreEqualOrCompatible(LHS->getType(), RHS->getType())) {
	diags->emitDiagMsgRange(
		LHS->getLeftLoc(), RHS->getLeftLoc(),
		diag::err_incompatible_binary_operands, LHS->getType()->getName(),
		RHS->getType()->getName(), Basic::Op::getBinaryOpSpelling(Op));
	return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
		std::move(LHS), std::move(RHS), Op, nullptr));
  }

  TypeDecl *resultType = nullptr;
  switch (Op) {
  case Basic::Op::BO_plus:
  case Basic::Op::BO_minus:
  case Basic::Op::BO_multiply:
  case Basic::Op::BO_divide:resultType = LHS->getType();
	break;
  case Basic::Op::BO_assign:
  case Basic::Op::BO_plusassign:
  case Basic::Op::BO_minusassign:
  case Basic::Op::BO_multassign:
  case Basic::Op::BO_divassign:
	if (llvm::isa<FloatingLiteral>(LHS.get()) ||
		llvm::isa<IntegerLiteral>(LHS.get())) {
	  diags->emitDiagMsg(LHS->getLeftLoc(), diag::err_expr_not_assignable);
	  return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
		  std::move(LHS), std::move(RHS), Op, nullptr));
	}
	break;
  case Basic::Op::BO_equals:
  case Basic::Op::BO_notequals:
  case Basic::Op::BO_lt:
  case Basic::Op::BO_gt:
  case Basic::Op::BO_ltequals:
  case Basic::Op::BO_gtequals:
	resultType =
		lookupType(Basic::Data::getBasicTypeSpelling(Basic::Data::bool_));
	break;
  case Basic::Op::BO_range:
  case Basic::Op::NUM_BINARY:break;
  }

  auto BinOp = std::make_unique<BinaryOp>(std::move(LHS), std::move(RHS), Op,
										  resultType);
  return BinOp;
}

std::unique_ptr<Expr>
funLang::SemaAnalyzer::actOnFnCall(Token &ID,
								   std::unique_ptr<CallArgList> PassedArgs) {
  if (Decl *Lookup = lookup(ID.getIdentifier())) {
	if (FunctionNode *CalledFunction = llvm::dyn_cast<FunctionNode>(Lookup)) {
	  if (PassedArgs->getSize() != CalledFunction->getArgDecls().size()) {
		diags->emitDiagMsg(ID.getLoc(), diag::err_wrong_number_of_parameters,
						   ID.getIdentifier(),
						   CalledFunction->getArgDecls().size(),
						   PassedArgs->getSize());
		auto ErrorFN = std::make_unique<FunctionCall>(
			ID.getIdentifier(), std::move(PassedArgs), ID.getLoc());

		return std::make_unique<ErrorExpr>(std::move(ErrorFN));
	  }
	  bool Mismatch = false;
	  auto &CalledArgsVec = CalledFunction->getArgDecls();

	  for (size_t idx = 0; idx < CalledArgsVec.size(); idx++) {
		if (!TypesAreEqualOrCompatible(
			PassedArgs->getArgsVec()[idx]->getType(),
			CalledArgsVec[idx]->getUnderlyingTypeDecl())) {
		  diags->emitDiagMsg(
			  PassedArgs->getArgsVec()[idx]->getLeftLoc(),
			  diag::err_incompatible_type_passed, CalledArgsVec[idx]->getName(),
			  CalledArgsVec[idx]->getUnderlyingTypeDecl()->getName(),
			  PassedArgs->getArgsVec()[idx]->getType()->getName());
		  Mismatch = true;
		}
	  }
	  auto FNCall = std::make_unique<FunctionCall>(
		  ID.getIdentifier(), std::move(PassedArgs), ID.getLoc(),
		  CalledFunction->getTypeDecl());
	  if (Mismatch) {
		return std::make_unique<ErrorExpr>(std::move(FNCall));
	  }
	  return FNCall;
	} else {
	  diags->emitDiagMsg(ID.getLoc(), diag::err_called_fn_on_var,
						 Lookup->getName());
	  return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(
		  ID.getIdentifier(), std::move(PassedArgs), ID.getLoc()));
	}
  } else {
	diags->emitDiagMsg(ID.getLoc(), diag::err_fn_not_found, ID.getIdentifier());
	return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(
		ID.getIdentifier(), std::move(PassedArgs), ID.getLoc()));
  }
}

std::unique_ptr<VarDeclStmt>
funLang::SemaAnalyzer::actOnVarDeclStmt(std::unique_ptr<VarDecl> NameDecl,
										std::unique_ptr<Expr> ExprInput,
										llvm::SMLoc SemicolonLoc) {
  if (!ExprInput) {
	return std::make_unique<VarDeclStmt>(std::move(NameDecl), nullptr,
										 NameDecl->getLeftLoc(), SemicolonLoc);
  }

  if (!ExprInput->getType()) {
	return std::make_unique<VarDeclStmt>(std::move(NameDecl), nullptr,
										 NameDecl->getLeftLoc(), SemicolonLoc);
  }

  if (!TypesAreEqualOrCompatible(NameDecl->getUnderlyingTypeDecl(),
								 ExprInput->getType())) {
	diags->emitDiagMsg(
		ExprInput->getLeftLoc(), diag::err_incompatible_type_var_decl,
		NameDecl->getName(), NameDecl->getUnderlyingTypeDecl()->getName(),
		ExprInput->getType()->getName());
  }
  return std::make_unique<VarDeclStmt>(std::move(NameDecl),
									   std::move(ExprInput),
									   NameDecl->getLeftLoc(), SemicolonLoc);
}

void funLang::SemaAnalyzer::exitScope() {
  assert(currentScope->getParent() && "Attempted to exit global scope.\n");
  currentScope = currentScope->getParent();
}

// TODO: Add some kind of DeclContext
void funLang::SemaAnalyzer::enterScope() {
  std::shared_ptr<Scope> innerScope = std::make_shared<Scope>(currentScope);
  currentScope = innerScope;
}

std::unique_ptr<NameUsage>
funLang::SemaAnalyzer::actOnNameUsage(Token &Identifier) {
  assert(Identifier.getTag() == Basic::tok::Tag::identifier &&
	  "Trying to lookup a tag that isn't an identifier");
  Decl *Found = lookup(Identifier.getIdentifier());
  if (!Found) {
	diags->emitDiagMsg(Identifier.getLoc(), diag::err_var_not_found,
					   Identifier.getIdentifier());
	return std::make_unique<NameUsage>(Identifier.getIdentifier(),
									   Identifier.getLoc(), nullptr);
  }
  VarDecl *Variable = llvm::dyn_cast<VarDecl>(Found);
  if (!Variable) {
	diags->emitDiagMsg(Identifier.getLoc(), diag::err_var_not_found,
					   Identifier.getIdentifier());
	return nullptr;
  }
  return std::make_unique<NameUsage>(Identifier.getIdentifier(),
									 Identifier.getLoc(),
									 Variable->getUnderlyingTypeDecl());
}

std::unique_ptr<FunctionNode>
funLang::SemaAnalyzer::actOnFnDecl(std::unique_ptr<TypeUse> Type, Token ID,
								   std::unique_ptr<ArgsList> Args,
								   std::unique_ptr<CompoundStmt> Compound) {

  return std::make_unique<FunctionNode>(std::move(Type), ID.getIdentifier(),
										std::move(Args), std::move(Compound),
										ID.getLoc());
}

/// Make return type available to parser and insert declared arguments into
/// function scope.
void funLang::SemaAnalyzer::enterFunction(std::unique_ptr<TypeUse> retType,
										  ArgsList &args) {
  currentFnRetType = std::move(retType);
  enterScope();

  for (auto &arg : args.getArgList()) {
	currentScope->insert(arg.get());
  }
}

std::unique_ptr<TypeUse> funLang::SemaAnalyzer::exitFunction() {
  exitScope();
  return std::move(currentFnRetType);
}

bool funLang::SemaAnalyzer::actOnTopLevelDecl(Decl *TopLDecl) {
  if (Decl *search = lookup(TopLDecl->getName())) {
	if (search != TopLDecl) {
	  diags->emitDiagMsg(TopLDecl->getLeftLoc(),
						 diag::err_toplevel_redefinition, search->getName());
	  diags->emitDiagMsg(search->getLeftLoc(), diag::note_var_redefinition,
						 search->getName());
	}
	return false;
  }

  currentScope->insert(TopLDecl);
  return true;
}

void funLang::SemaAnalyzer::init() {
  auto BoolTy = new BuiltInType(Basic::Data::Type::bool_, "bool", 1);
  auto I32Ty = new BuiltInType(Basic::Data::Type::i32, "i32", 32);
  auto I64Ty = new BuiltInType(Basic::Data::Type::i64, "i64", 64);
  auto F32Ty = new BuiltInType(Basic::Data::f32, "f32", 32);
  auto F64Ty = new BuiltInType(Basic::Data::f64, "f64", 64);
  auto StringTy = new BuiltInType(Basic::Data::string, "string", 64);
  auto VoidTy = new BuiltInType(Basic::Data::void_, "void", 0);

  auto IntLiteral =
	  new BuiltInType(Basic::Data::int_literal, "integer literal", 64);
  auto FloatLiteral = new BuiltInType(Basic::Data::floating_literal,
									  "floating point literal", 64);

  currentScope->insert(BoolTy);
  currentScope->insert(I32Ty);
  currentScope->insert(I64Ty);
  currentScope->insert(F32Ty);
  currentScope->insert(F64Ty);
  currentScope->insert(StringTy);
  currentScope->insert(VoidTy);
  currentScope->insert(IntLiteral);
  currentScope->insert(FloatLiteral);
}

std::unique_ptr<IntegerLiteral>
SemaAnalyzer::actOnIntegerLiteral(Token &Literal) {

  uint32_t NumBits =
	  llvm::APInt::getSufficientBitsNeeded(Literal.getLexeme(), 10);

  llvm::APInt ApInt = llvm::APInt(NumBits, Literal.getLexeme(), 10);
  auto IntLiteral = std::make_unique<IntegerLiteral>(ApInt, Literal.getLoc());
  IntLiteral->setType(
	  lookupType(Basic::Data::getBasicTypeSpelling(Basic::Data::int_literal)));
  return IntLiteral;
}

std::unique_ptr<FloatingLiteral>
SemaAnalyzer::actOnFloatingLiteral(Token &Literal) {
  llvm::APFloat ApFloatSingle =
	  llvm::APFloat(llvm::APFloat::IEEEdouble(),
					Literal.getLexeme()); // TODO: find out how to use APFloat
  std::unique_ptr<FloatingLiteral> FloatLit =
	  std::make_unique<FloatingLiteral>(ApFloatSingle, Literal.getLoc());
  FloatLit->setType(lookupType(
	  Basic::Data::getBasicTypeSpelling(Basic::Data::floating_literal)));

  return FloatLit;
}

std::unique_ptr<BooleanLiteral>
SemaAnalyzer::actOnBooleanLiteral(Token &Literal) {
  assert((Literal.getTag() == Basic::tok::kw_true ||
	  Literal.getTag() == Basic::tok::kw_false) &&
	  "Token tag is not true or false in actOnBooleanLiteral");
  auto BoolLit = std::make_unique<BooleanLiteral>(
	  Literal.getTag() == Basic::tok::kw_true, Literal.getLoc());
  BoolLit->setType(
	  (lookupType(Basic::Data::getBasicTypeSpelling(Basic::Data::bool_))));

  return BoolLit;
}

std::unique_ptr<StringLiteral> SemaAnalyzer::actOnStrLiteral(Token &Literal) {
  assert(Literal.is(Basic::tok::string_literal) &&
	  "Tag is not a string in string literal");

  return std::make_unique<StringLiteral>(
	  Literal.getLexeme().size() - 2, Literal.getLexeme(), Literal.getLoc(),
	  lookupType(Basic::Data::getBasicTypeSpelling(Basic::Data::string)));
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

std::unique_ptr<VarDecl>
SemaAnalyzer::actOnNameDecl(std::unique_ptr<TypeUse> Type, Token &Name) {

  auto TypeLoc = Type->getLeftLoc();
  if (Decl *LookedUp = lookupCurrentScope(Name.getIdentifier())) {
	diags->emitDiagMsg(Name.getLoc(), diag::err_var_redefinition,
					   Name.getIdentifier());
	diags->emitDiagMsg(LookedUp->getLeftLoc(), diag::note_var_redefinition,
					   LookedUp->getName());

	return std::make_unique<VarDecl>(std::move(Type), Name.getIdentifier(),
									 TypeLoc, Name.getRightmostLoc());
  }
  auto Dec = std::make_unique<VarDecl>(std::move(Type), Name.getIdentifier(),
									   TypeLoc, Name.getRightmostLoc());

  currentScope->insert(Dec.get());
  return Dec;
}

std::unique_ptr<TypeDecl>
SemaAnalyzer::actOnStructDecl(Token &TypeName,
							  std::unique_ptr<TypeProperties> Properties,
							  llvm::SMLoc RBraceLoc) {
  size_t Size = 0;
  for (auto &T : Properties->getDecls()) {
	Size += T.getValue()->getUnderlyingTypeDecl()->getSize();
  }
  return std::make_unique<TypeDecl>(TypeName.getIdentifier(),
									std::move(Properties), TypeName.getLoc(),
									RBraceLoc, Size);
}

TypeDecl *funLang::SemaAnalyzer::lookupType(llvm::StringRef Type) {
  if (Decl *Found = lookup(Type)) {
	if (!(Found->getKind() == Decl::DK_TYPE ||
		Found->getKind() == Decl::DK_TYPEBUILTIN)) {
	  return nullptr;
	}
	return llvm::dyn_cast<TypeDecl>(Found);
  }
  return nullptr;
}

Decl *funLang::SemaAnalyzer::lookupCurrentScope(llvm::StringRef varName) {
  return currentScope->find(varName);
}
