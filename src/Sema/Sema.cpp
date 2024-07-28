#include "Sema/Sema.hpp"
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/SMLoc.h>
#include <memory>
using namespace funLang;

void SemaAnalyzer::init() {
  auto I32Ty = new BuiltInType(BuiltInType::i32);
  auto I64Ty = new BuiltInType(BuiltInType::i64);
  auto F32Ty = new BuiltInType(BuiltInType::f32);
  auto F64Ty = new BuiltInType(BuiltInType::f64);
  auto boolTy = new BuiltInType(BuiltInType::bool_);
  auto stringTy = new BuiltInType(BuiltInType::string);

  Types.insert({"i32", I32Ty});
  Types.insert({"i64", I64Ty});
  Types.insert({"f32", F32Ty});
  Types.insert({"f64", F64Ty});
  Types.insert({"bool", boolTy});
  Types.insert({"string", stringTy});
}

bool SemaAnalyzer::actOnTopLevelDecl(std::unique_ptr<Decl> TopLDecl) {
  auto Ref = TopLDecl.get();
  if (lookup(TopLDecl->getEntry())) {
	Diags->emitDiagMsg(Ref->getStart(), diag::err_toplevel_redefinition, TopLDecl->getName());
	return false;
  }

  insert(std::move(TopLDecl));
  return true;
}

void SemaAnalyzer::actOnStructMemberDecl(std::unique_ptr<VarDecl> Var) {
  assert(DeclScope->isStructScope() && "Not in a struct!");
  if (lookupOne(Var->getEntry())) {
	auto Record = llvm::dyn_cast<RecordDecl>(DeclScope->getContext());
	Diags->emitDiagMsg(Var->getStart(), diag::err_struct_var_redefinition, Var->getName(), Record->getName());
	return;
  }
  insert(std::move(Var));
}

std::unique_ptr<forStmt> SemaAnalyzer::actOnForStmt(
	std::unique_ptr<Stmt> Init, std::unique_ptr<Expr> Cond,
	std::unique_ptr<Expr> Inc, std::unique_ptr<CompoundStmt> Body,
	llvm::SMLoc Left, llvm::SMLoc Right) {
  if (!Cond->getType()->isBoolType()) {
	Diags->emitDiagMsgRange(Inc->getStartLoc(), Inc->getEndLoc(), diag::err_if_condition_does_not_evaluate_to_bool);
	return nullptr;
  }

  return std::make_unique<forStmt>(Left, Right, std::move(Init), std::move(Cond), std::move(Inc), std::move(Body));
}

std::unique_ptr<whileStmt>
SemaAnalyzer::actOnWhileStmt(std::unique_ptr<Expr> Condition,
							 std::unique_ptr<CompoundStmt> Compound,
							 llvm::SMLoc Left, llvm::SMLoc Right) {
  if (!Condition->getType()) {
	return std::make_unique<whileStmt>(std::move(Condition),
									   std::move(Compound), Left, Right);
  }
  if (!Condition->getType()->isBoolType()) {
	Diags->emitDiagMsgRange(Left, Right, diag::err_while_condition_not_bool);
  }

  return std::make_unique<whileStmt>(std::move(Condition), std::move(Compound),
									 Left, Right);
}
std::unique_ptr<TypeUse> SemaAnalyzer::actOnTypeUse(Token &TypeName, size_t IndirectionCount) {
  if (Type *FoundType = getType(TypeName.getLexeme())) {
	return std::make_unique<TypeUse>(FoundType, TypeName.getLoc(), TypeName.getRightmostLoc());
  } else {
	Diags->emitDiagMsg(TypeName.getLoc(), diag::err_var_not_found,
					   TypeName.getLexeme());
	return std::make_unique<TypeUse>(nullptr, TypeName.getLoc(), TypeName.getRightmostLoc());
  }
}

std::unique_ptr<ReturnStmt>
funLang::SemaAnalyzer::actOnReturnStmt(llvm::SMLoc ReturnLoc,
									   llvm::SMLoc SemicolonLoc,
									   std::unique_ptr<Expr> ReturnExpr) {

  if (!CurrentFunctionReturnType || !ReturnExpr->getType()) { // poison
	return std::make_unique<ReturnStmt>(std::move(ReturnExpr));
  }
  if (!ReturnExpr) {
	if (!CurrentFunctionReturnType->isVoidType()) {
	  Diags->emitDiagMsg(ReturnLoc, diag::err_naked_return_on_non_void_fn);
	}
	return std::make_unique<ReturnStmt>(nullptr);
  }

  if (!CurrentFunctionReturnType->eqTo(ReturnExpr->getType())) {
	Diags->emitDiagMsgRange(ReturnExpr->getStartLoc(), ReturnExpr->getEndLoc(), diag::err_incompatible_ret);
	return std::make_unique<ReturnStmt>(
		std::make_unique<ErrorExpr>(std::move(ReturnExpr)));
  }
  return std::make_unique<ReturnStmt>(std::move(ReturnExpr));
}

std::unique_ptr<Expr>
funLang::SemaAnalyzer::actOnUnaryOp(Basic::Op::Unary Op,
									std::unique_ptr<Expr> ExprInput,
									llvm::SMLoc OpLoc) {
  if (!ExprInput->getType()) {
	return std::make_unique<ErrorExpr>(std::make_unique<UnaryOp>(std::move(ExprInput),
																 Op,
																 nullptr,
																 OpLoc,
																 ExprInput->getStartLoc()));
  }

  if (!llvm::isa<BuiltInType>(ExprInput->getType())) {
	Diags->emitDiagMsg(ExprInput->getStartLoc(), diag::err_unary_op_incompatible,
					   Basic::Op::getUnaryOpSpelling(Op),
					   ExprInput->getType()->getName());

	return std::make_unique<ErrorExpr>(std::make_unique<UnaryOp>(std::move(ExprInput),
																 Op,
																 nullptr,
																 OpLoc,
																 ExprInput->getStartLoc()));
  }

  bool Mismatch = false;
  Type *ExprType = ExprInput->getType();
  switch (Op) {
  case Basic::Op::UO_unaryMinus:
  case Basic::Op::UO_preInc:
  case Basic::Op::UO_preDec: {
	if (llvm::isa<IntegerLiteral>(ExprInput.get())) {
	  Diags->emitDiagMsg(ExprInput->getStartLoc(), diag::err_invalid_lhs, Basic::Op::getUnaryOpSpelling(Op));
	  return std::make_unique<ErrorExpr>(std::make_unique<UnaryOp>(std::move(ExprInput),
																   Op,
																   nullptr,
																   OpLoc,
																   ExprInput->getStartLoc()));
	} else if (!ExprType->isIntType()) {
	  Mismatch = true;
	}
	break;
  }
  case Basic::Op::UO_lNot: {
	if (!ExprType->isBoolType()) {
	  Mismatch = true;
	}
	break;
  }
  default: llvm_unreachable("Invalid unary operator");
  }
  if (Mismatch) {
	Diags->emitDiagMsg(ExprInput->getStartLoc(), diag::err_unary_op_incompatible,
					   Basic::Op::getUnaryOpSpelling(Op),
					   ExprInput->getType()->getName());

	return std::make_unique<ErrorExpr>(std::make_unique<UnaryOp>(std::move(ExprInput),
																 Op,
																 nullptr,
																 OpLoc,
																 ExprInput->getStartLoc()));
  }

  return std::make_unique<UnaryOp>(std::move(ExprInput), Op, ExprType, OpLoc, ExprInput->getStartLoc());
}

std::unique_ptr<Expr> SemaAnalyzer::actOnBinaryOp(std::unique_ptr<Expr> LHS,
												  Basic::Op::Binary Op,
												  std::unique_ptr<Expr> RHS) {
  if (!LHS->getType() || !LHS->getType()) {
	return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
		std::move(LHS), std::move(RHS), Op, nullptr));
  }

  if (!LHS->getType()->eqTo(RHS->getType())) {
	Diags->emitDiagMsgRange(
		LHS->getStartLoc(), RHS->getEndLoc(),
		diag::err_incompatible_binary_operands, LHS->getType()->getName(),
		RHS->getType()->getName(), Basic::Op::getBinaryOpSpelling(Op));
	return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
		std::move(LHS), std::move(RHS), Op, nullptr));
  }

  Type *resultType = nullptr;
  switch (Op) {
  case Basic::Op::BO_plus:
  case Basic::Op::BO_minus:
  case Basic::Op::BO_multiply:
  case Basic::Op::BO_divide: {
	if (!LHS->getType()->eqTo(RHS->getType())) {
	  Diags->emitDiagMsgRange(LHS->getStartLoc(),
							  RHS->getEndLoc(),
							  diag::err_incompatible_binary_operands,
							  LHS->getType()->getName(),
							  RHS->getType()->getName(),
							  Basic::Op::getBinaryOpSpelling(Op));
	  return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
		  std::move(LHS), std::move(RHS), Op, nullptr));
	}
	resultType = LHS->getType();
	break;
  }
  case Basic::Op::BO_assign:
  case Basic::Op::BO_plusassign:
  case Basic::Op::BO_minusassign:
  case Basic::Op::BO_multassign:
  case Basic::Op::BO_divassign:
	if (llvm::isa<FloatingLiteral>(LHS.get()) ||
		llvm::isa<IntegerLiteral>(LHS.get())) {
	  Diags->emitDiagMsg(LHS->getStartLoc(), diag::err_invalid_lhs, Basic::Op::getBinaryOpSpelling(Op));
	  return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
		  std::move(LHS), std::move(RHS), Op, nullptr));
	}
  case Basic::Op::BO_equals:
  case Basic::Op::BO_notequals:
  case Basic::Op::BO_lt:
  case Basic::Op::BO_gt:
  case Basic::Op::BO_ltequals:
  case Basic::Op::BO_gtequals:
	resultType =
		getType(Basic::Data::getBasicTypeSpelling(Basic::Data::bool_));
	break;
  case Basic::Op::BO_range:
  case Basic::Op::NUM_BINARY:break;
  }

  auto BinOp = std::make_unique<BinaryOp>(std::move(LHS), std::move(RHS), Op,
										  resultType);
  return BinOp;
}

std::unique_ptr<Expr>
SemaAnalyzer::actOnFunctionCall(Token &ID,
								llvm::SMLoc RParenLoc,
								u_ptr<llvm::SmallVector<u_ptr<Expr>>> PassedArgs) {

  Decl *FoundFunction = lookup(ID.getIdentifierTableEntry()); // check if name exists

  if (!FoundFunction) {
	Diags->emitDiagMsg(ID.getLoc(), diag::err_fn_not_found, ID.getIdentifier());
	return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(ID.getIdentifierTableEntry(),
																	  std::move(PassedArgs),
																	  ID.getLoc(),
																	  RParenLoc));
  }
  if (!llvm::isa<FunctionDecl>(FoundFunction)) { // is it a function
	Diags->emitDiagMsg(ID.getLoc(), diag::err_name_not_fn, ID.getLexeme());
	return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(ID.getIdentifierTableEntry(),
																	  std::move(PassedArgs),
																	  ID.getLoc(),
																	  RParenLoc));
  }
  FunctionDecl *FunctionPtr = llvm::dyn_cast<FunctionDecl>(FoundFunction);
  auto &FunctionParams = FunctionPtr->getParams();
  if (FunctionParams.size() != PassedArgs->size()) { // check formal params size vs passed arguments
	Diags->emitDiagMsgRange(ID.getLoc(),
							RParenLoc,
							diag::err_wrong_number_of_parameters,
							FunctionParams.size(),
							PassedArgs->size());
	return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(ID.getIdentifierTableEntry(),
																	  std::move(PassedArgs),
																	  ID.getLoc(),
																	  RParenLoc));
  }
  bool FailedTypeChecking = false;
  for (size_t i = 0; i < FunctionParams.size(); i++) { // type check each param
	if (!FunctionParams[i]->getTypePtr()->eqTo((*PassedArgs)[i]->getType())) {
	  Diags->emitDiagMsg((*PassedArgs)[i]->getStartLoc(),
						 diag::err_incompatible_type_passed,
						 FunctionParams[i]->getName(),
						 FunctionParams[i]->getTypePtr()->getName(),
						 (*PassedArgs)[i]->getType()->getName());
	  FailedTypeChecking = true;
	}
  }

  if (FailedTypeChecking) {
	return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(ID.getIdentifierTableEntry(),
																	  std::move(PassedArgs),
																	  ID.getLoc(),
																	  RParenLoc));
  }

  return std::make_unique<FunctionCall>(ID.getIdentifierTableEntry(),
										std::move(PassedArgs),
										ID.getLoc(),
										RParenLoc, FunctionPtr->getTypePtr());
}

std::unique_ptr<DeclStmt>
SemaAnalyzer::actOnVarDeclStmt(std::unique_ptr<VarDecl> NameDecl,
							   std::unique_ptr<Expr> ExprInput,
							   llvm::SMLoc SemicolonLoc) {
  auto TheDecl = NameDecl.get();
  if (lookup(NameDecl->getEntry())) {
	Diags->emitDiagMsgRange(NameDecl->getStart(), NameDecl->getEnd(), diag::err_var_redefinition, NameDecl->getName());
  }
  insert(std::move(NameDecl));

  if (!ExprInput) {
	return std::make_unique<DeclStmt>(TheDecl->getStart(), SemicolonLoc, TheDecl, nullptr);
  }
  if (!ExprInput->getType()) {
	return std::make_unique<DeclStmt>(TheDecl->getStart(), SemicolonLoc, TheDecl,
									  std::move(ExprInput));
  }

  if (!NameDecl->getTypePtr()->eqTo(ExprInput->getType())) {
	Diags->emitDiagMsgRange(
		ExprInput->getStartLoc(), ExprInput->getEndLoc(), diag::err_incompatible_type_var_decl,
		TheDecl->getName(), TheDecl->getTypePtr()->getName(),
		ExprInput->getType()->getName());
  }

  return std::make_unique<DeclStmt>(TheDecl->getStart(), SemicolonLoc, TheDecl, std::move(ExprInput));
}

std::unique_ptr<Expr>
funLang::SemaAnalyzer::actOnNameUsage(Token &Identifier) {
  assert(Identifier.getTag() == Basic::tok::Tag::identifier &&
	  "Trying to lookup a tag that isn't an identifier");
  Decl *Found = lookup(Identifier.getIdentifierTableEntry());
  if (!Found) {
	Diags->emitDiagMsg(Identifier.getLoc(), diag::err_var_not_found,
					   Identifier.getIdentifier());

	return std::make_unique<ErrorExpr>(std::make_unique<NameUsage>(Identifier.getIdentifierTableEntry(),
																   Identifier.getLoc(),
																   Identifier.getRightmostLoc(),
																   nullptr));
  }
  auto *Variable = llvm::dyn_cast<VarDecl>(Found);
  if (!Variable) {
	Diags->emitDiagMsg(Identifier.getLoc(), diag::err_var_not_found,
					   Identifier.getIdentifier());
	return std::make_unique<ErrorExpr>(std::make_unique<NameUsage>(Identifier.getIdentifierTableEntry(),
																   Identifier.getLoc(),
																   Identifier.getRightmostLoc(),
																   nullptr));
  }
  return std::make_unique<NameUsage>(Identifier.getIdentifierTableEntry(),
									 Identifier.getLoc(),
									 Identifier.getRightmostLoc(),
									 Variable->getTypePtr());
}

void
funLang::SemaAnalyzer::actOnFunctionDecl(FunctionDecl *Function, std::unique_ptr<CompoundStmt> CompoundToAttach) {
  Function->setCompound(std::move(CompoundToAttach));
  exitScope();
}

std::unique_ptr<IntegerLiteral>
SemaAnalyzer::actOnIntegerLiteral(Token &Literal) {

  uint32_t NumBits =
	  llvm::APInt::getSufficientBitsNeeded(Literal.getLexeme(), 10);

  llvm::APInt ApInt = llvm::APInt(NumBits, Literal.getLexeme(), 10);
  auto IntLiteral = std::make_unique<IntegerLiteral>(ApInt, Literal.getLoc(), Literal.getRightmostLoc());
  IntLiteral->setType(
	  getType(Basic::Data::getBasicTypeSpelling(Basic::Data::i32)));
  return IntLiteral;
}

std::unique_ptr<FloatingLiteral>
SemaAnalyzer::actOnFloatingLiteral(Token &Literal) {
  llvm::APFloat ApFloatSingle =
	  llvm::APFloat(llvm::APFloat::IEEEdouble(),
					Literal.getLexeme()); // TODO: find out how to use APFloat
  std::unique_ptr<FloatingLiteral> FloatLit =
	  std::make_unique<FloatingLiteral>(ApFloatSingle, Literal.getLoc(), Literal.getRightmostLoc());
  FloatLit->setType(getType("f32"));
  return FloatLit;
}

std::unique_ptr<BooleanLiteral>
SemaAnalyzer::actOnBooleanLiteral(Token &Literal) {
  assert((Literal.getTag() == Basic::tok::kw_true ||
	  Literal.getTag() == Basic::tok::kw_false) &&
	  "Token tag is not true or false in actOnBooleanLiteral");
  auto BoolLit = std::make_unique<BooleanLiteral>(
	  Literal.getTag() == Basic::tok::kw_true, Literal.getLoc(), Literal.getRightmostLoc());
  BoolLit->setType(
	  (getType(Basic::Data::getBasicTypeSpelling(Basic::Data::bool_))));

  return BoolLit;
}

std::unique_ptr<StrLiteral> SemaAnalyzer::actOnStrLiteral(Token &Literal) {
  assert(Literal.is(Basic::tok::string_literal) &&
	  "Tag is not a string in string literal");

  return std::make_unique<StrLiteral>(
	  Literal.getLexeme().size() - 2, Literal.getLexeme(), Literal.getLoc(), Literal.getRightmostLoc(),
	  getType(Basic::Data::getBasicTypeSpelling(Basic::Data::string)));
}

Decl *funLang::SemaAnalyzer::lookup(IDTableEntry *var) {
  Scope *s = DeclScope.get();
  while (s) {
	Decl *Found = s->lookup(var);
	if (Found) {
	  return Found;
	} else {
	  s = s->getParent();
	}
  }
  return nullptr;
}

std::unique_ptr<VarDecl>
SemaAnalyzer::actOnNameDecl(std::unique_ptr<TypeUse> Type, Token &Name) {
  //create named decl;

  auto Loc = Type->getTypeNameLoc();
  auto Entry = Name.getIdentifierTableEntry();

  return std::make_unique<VarDecl>(std::move(Type), Entry, Loc, Name.getLoc());;
}

Type *funLang::SemaAnalyzer::getType(llvm::StringRef TypeName) {
  auto Found = Types.find(TypeName);
  if (Found == Types.end()) {
	return nullptr;
  }
  return Found->second;

}

std::unique_ptr<Expr> SemaAnalyzer::actOnIndexOperation(std::unique_ptr<Expr> Accessed,
														std::unique_ptr<Expr> AccessExpr,
														llvm::SMLoc Left,
														llvm::SMLoc Right) {
  if (!AccessExpr->getType()->isIntType()) {
	Diags->emitDiagMsgRange(Left,
							Right,
							diag::err_array_index_expr_not_whole_number,
							AccessExpr->getType()->getName());
	return std::make_unique<ErrorExpr>(std::make_unique<ArrayIndex>(std::move(Accessed),
																	std::move(AccessExpr),
																	nullptr,
																	Left, Right));
  }

  auto *ArrTy = llvm::dyn_cast<PointerType>(Accessed->getType());;
  if (!ArrTy) {
	Diags->emitDiagMsgRange(Left, Right, diag::err_indexing_on_non_array, Accessed->getType()->getName());
	return std::make_unique<ErrorExpr>(std::make_unique<ArrayIndex>(std::move(Accessed),
																	std::move(AccessExpr),
																	nullptr,
																	Left, Right));
  }
  return std::make_unique<ArrayIndex>(std::move(Accessed),
									  std::move(AccessExpr),
									  ArrTy->getPointeeType(),
									  Left, Right);
}
std::unique_ptr<Expr> SemaAnalyzer::actOnMemberExpr(std::unique_ptr<Expr> Accessed, std::unique_ptr<Expr> Accessor) {
  Type *AccessedType = Accessed->getType();

  if (!AccessedType) {
	return std::make_unique<ErrorExpr>(std::make_unique<MemberAccess>(std::move(Accessed),
																	  nullptr,
																	  nullptr,
																	  Accessed->getStartLoc(),
																	  Accessor->getStartLoc()));
  }
  auto Name = llvm::dyn_cast<NameUsage>(Accessor.get());
  if (!Name) {
	return std::make_unique<ErrorExpr>(std::make_unique<MemberAccess>(std::move(Accessed),
																	  nullptr,
																	  nullptr,
																	  Accessed->getStartLoc(),
																	  Accessor->getStartLoc()));
  }

  if (auto RecordTy = llvm::dyn_cast<RecordType>(AccessedType)) {
	Decl *Get = RecordTy->lookup(Name->getEntry());
	if (!Get) {
	  Diags->emitDiagMsg(Accessor->getStartLoc(),
						 diag::err_type_member_does_not_exist,
						 RecordTy->getName(),
						 Name->getUsedName());
	  return std::make_unique<ErrorExpr>(std::make_unique<MemberAccess>(std::move(Accessed),
																		nullptr,
																		nullptr,
																		Accessed->getStartLoc(),
																		Accessor->getStartLoc()));
	}
	VarDecl *Member = llvm::dyn_cast<VarDecl>(Get);
	assert(Member);
	return std::make_unique<MemberAccess>(std::move(Accessed),
										  Member,
										  Member->getTypePtr(),
										  Accessed->getStartLoc(),
										  Accessor->getStartLoc());
  }
  Diags->emitDiagMsg(Accessor->getStartLoc(),
					 diag::err_type_has_no_members,
					 Name->getUsedName(),
					 AccessedType->getName());

  return std::make_unique<ErrorExpr>(std::make_unique<MemberAccess>(std::move(Accessed),
																	nullptr,
																	nullptr,
																	Accessed->getStartLoc(),
																	Accessor->getStartLoc()));
}

std::unique_ptr<loopStmt> SemaAnalyzer::actOnLoopStmt(std::unique_ptr<CompoundStmt> Compound, llvm::SMLoc LoopLoc) {
  return std::make_unique<loopStmt>(std::move(Compound), LoopLoc);
}

std::unique_ptr<Expr> SemaAnalyzer::actOnDereference(std::unique_ptr<Expr> ExprInput, size_t DerefCount) {
  // TODO
  return nullptr;
}
std::unique_ptr<ifStmt> SemaAnalyzer::actOnIfStmt(llvm::SMLoc IfLoc,
												  llvm::SMLoc EndOfExprLoc,
												  std::unique_ptr<Expr> IfCondition,
												  std::unique_ptr<CompoundStmt> FirstBlock,
												  std::unique_ptr<elifStmt> ElifChain,
												  std::unique_ptr<CompoundStmt> ElseBlock) {
  if (!IfCondition->getType()) {
	return std::make_unique<ifStmt>(IfLoc, std::move(IfCondition), EndOfExprLoc,
									std::move(FirstBlock),
									std::move(ElifChain),
									std::move(ElseBlock));
  }

  if (!IfCondition->getType()->isBoolType()) {
	Diags->emitDiagMsgRange(IfCondition->getStartLoc(),
							IfCondition->getEndLoc(),
							diag::err_if_condition_does_not_evaluate_to_bool /*, PLACEHOLDER*/);
  }

  return std::make_unique<ifStmt>(IfLoc,
								  std::move(IfCondition),
								  EndOfExprLoc,
								  std::move(FirstBlock),
								  std::move(ElifChain),
								  std::move(ElseBlock));
}

SemaAnalyzer::SemaAnalyzer(std::shared_ptr<DiagEngine> diag)
	: Diags(std::move(diag)),
	  Types(llvm::StringMap<Type *>()), DeclScope(std::make_unique<Scope>()), CurrentFunctionReturnType(nullptr) {
  init();
}

Scope::Scope() : Parent(nullptr), ScopeContext(nullptr), Kind(ScopeKind::GlobalScope) {}

Scope::Scope(std::unique_ptr<Scope> Parent, ScopeKind K)
	: Parent(std::move(Parent)), ScopeContext(Parent->getContext()), Kind(K) {}

bool Scope::insertInContext(std::unique_ptr<Decl> Dec) { return ScopeContext->insert(Dec->getEntry(), std::move(Dec)); }

DeclContext *Scope::getContext() { return ScopeContext; }

std::unique_ptr<Scope> Scope::moveParentScope() {
  return std::move(Parent);
}

bool Scope::isGlobalScope() {
  return Parent.get();
}

Scope *Scope::getParent() { return Parent.get(); }

Decl *Scope::lookup(IDTableEntry *Name) {
  auto Found = TheLookupTable.find(Name);
  if (Found == TheLookupTable.end()) {
	return nullptr;
  }
  return Found->second;
}

void funLang::SemaAnalyzer::enterScope(Scope::ScopeKind K) {
  DeclScope = std::make_unique<Scope>(std::move(DeclScope), K);
}

bool SemaAnalyzer::insert(std::unique_ptr<Decl> ToInsert) {
  DeclScope->insertInLexicalScope(ToInsert->getEntry(), ToInsert.get());
  return DeclScope->insertInContext(std::move(ToInsert));
}

void SemaAnalyzer::actOnParamDecl(llvm::SmallVector<std::unique_ptr<VarDecl>> &CurrentParamList,
								  std::unique_ptr<VarDecl> Param,
								  llvm::DenseMap<IDTableEntry *, VarDecl *> &CheckAgainst) {
  auto Found = CheckAgainst.find(Param->getEntry());

  if (Found != CheckAgainst.end()) {
	Diags->emitDiagMsg(Param->getStart(), diag::err_param_redifinition, Param->getName());
  } else {
	CurrentParamList.push_back(std::move(Param));
  }
}

Decl *funLang::SemaAnalyzer::lookupOne(funLang::IDTableEntry *Var) {
  return DeclScope->lookup(Var);
}

void SemaAnalyzer::exitFunctionScope() {
  exitScope();
}

std::unique_ptr<FunctionDecl> SemaAnalyzer::enterFunctionScope(std::unique_ptr<TypeUse> FunctionReturnType,
															   u_ptr<llvm::SmallVector<u_ptr<
																   ParamDecl>>> Params,
															   Token &FunctionName,
															   llvm::SMLoc RParenLoc) {
  CurrentFunctionReturnType = FunctionReturnType->getTypePtr();
  auto ParamsRef = Params.get();
  std::unique_ptr<FunctionDecl> NewCtx = std::make_unique<FunctionDecl>(
	  std::move(FunctionReturnType),
	  FunctionName.getIdentifierTableEntry(),
	  FunctionName.getLoc(),
	  RParenLoc,
	  std::move(Params),
	  DeclScope->getContext()
  );
  auto NewScope = std::make_unique<Scope>(std::move(DeclScope), Scope::FunctionScope, NewCtx.get());
  DeclScope = std::move(NewScope);

  DeclScope->insertInLexicalScope(NewCtx->getEntry(), NewCtx.get());
  for (auto &Param : *ParamsRef) {
	DeclScope->insertInLexicalScope(Param->getEntry(), Param.get());
  }

  return NewCtx;
}

std::unique_ptr<RecordDecl> SemaAnalyzer::enterStructScope(Token &StructDetails) {
  IDTableEntry *StructName = StructDetails.getIdentifierTableEntry();
  auto NewCtx = std::make_unique<RecordDecl>(StructName, nullptr, DeclScope->getContext());
  auto NewScope = std::make_unique<Scope>(std::move(DeclScope), Scope::StructScope, NewCtx.get());

  DeclScope = std::move(NewScope);

  return std::move(NewCtx);
}

void funLang::SemaAnalyzer::exitScope() {
  assert(!DeclScope->isGlobalScope() && "Attempted to exit global scope\n");
  DeclScope = DeclScope->moveParentScope();
}

std::unique_ptr<Stmt> SemaAnalyzer::actOnBreakStmt(Token &BreakLoc) {
  if (!DeclScope->getClosestLoopScope()) {
	Diags->emitDiagMsg(BreakLoc.getLoc(), diag::err_break_in_non_loop);
	return std::make_unique<ErrorStmt>(std::make_unique<BreakStmt>(BreakLoc.getLoc(), BreakLoc.getRightmostLoc()));
  }
  return std::make_unique<BreakStmt>(BreakLoc.getLoc(), BreakLoc.getRightmostLoc());
}

std::unique_ptr<Stmt> SemaAnalyzer::actOnNextStmt(Token &NextLoc) {
  if (!DeclScope->getClosestLoopScope()) {
	Diags->emitDiagMsg(NextLoc.getLoc(), diag::err_next_in_non_loop);
	return std::make_unique<ErrorStmt>(std::make_unique<NextStmt>(NextLoc.getLoc(), NextLoc.getRightmostLoc()));
  }

  return std::make_unique<NextStmt>(NextLoc.getLoc(), NextLoc.getRightmostLoc());
}
