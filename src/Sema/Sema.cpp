module;
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>
#include <memory>
module Sema;
import AST;

namespace funLang {
void SemaAnalyzer::init() {
  BuiltinTypes[BuiltInType::i32] = new BuiltInType(BuiltInType::i32);
  BuiltinTypes[BuiltInType::i64] = new BuiltInType(BuiltInType::i64);
  BuiltinTypes[BuiltInType::f32] = new BuiltInType(BuiltInType::f32);
  BuiltinTypes[BuiltInType::f64] = new BuiltInType(BuiltInType::f64);
  BuiltinTypes[BuiltInType::bool_] = new BuiltInType(BuiltInType::bool_);
  BuiltinTypes[BuiltInType::string] = new BuiltInType(BuiltInType::string);
  BuiltinTypes[BuiltInType::int_literal] =
      new BuiltInType(BuiltInType::int_literal);
  BuiltinTypes[BuiltInType::float_literal] =
      new BuiltInType(BuiltInType::float_literal);
}

Type *SemaAnalyzer::checkTyOperands(Type *LTy, Type *RTy) {
  // todo: i don't like this

  if (!LTy->isArithmetic() || !RTy->isArithmetic()) {
    // some types are not allowed to be added together
    return nullptr;
  }

  // we are not allowing pointers to be added to each other
  if (LTy == RTy && (LTy->isNumeric() && RTy->isNumeric())) {
    // they're both the same, good
    return LTy;
  }

  // they're both int-ish
  if (LTy->isIntType() && RTy->isIntType()) {
    // LHS is literal and RHS is not? return RHS; RHS is literal and LHS is not? return LHS
    if (LTy->isIntLiteral() && !RTy->isIntLiteral()) {
      return RTy;
    }
    if (RTy->isIntLiteral() && !LTy->isIntLiteral()) {
      return LTy;
    }
  }
  // they're both float-ish
  if (LTy->isFloatType() && RTy->isFloatType()) {
    if (LTy->isFloatLiteral() && !RTy->isFloatLiteral()) {
      return LTy;
    }
    if (RTy->isFloatLiteral() && !LTy->isFloatType()) {
      return RTy;
    }
  }

  if (LTy->isPointer() && RTy->isIntType()) {
    return LTy;
  }
  if (RTy->isPointer() && RTy->isIntType()) {
    // disallow floats to be added to pointers
    return RTy;
  }
  // finally, the user is trying to do arithmetic with two different, incompatible types, bye!
  return nullptr;
}

// Check if the RHS can be assigned to the LHS
// Types are assignable if they are the same or if you're allowed to do arithmetic with them e.g. i32 and int literal

bool SemaAnalyzer::TypesAreCompatible(Type *LHS, Type *RHS) {
  if (LHS == RHS) {
    return true;
  }
  return checkTyOperands(LHS, RHS);
}

Type *SemaAnalyzer::checkBinOperands(Expr &LHS, Expr &RHS) {
  // literals get turned into a concrete type
  return checkTyOperands(LHS.getType(), RHS.getType());
}

bool SemaAnalyzer::actOnTopLevelDecl(std::unique_ptr<Decl> TopLDecl) {
  auto Ref = TopLDecl.get();
  if (lookup(TopLDecl->getEntry())) {
    Diags.emitDiagMsg(Ref->getStart(), Diag::err_toplevel_redefinition,
                      TopLDecl->getName());
    return false;
  }

  insert(std::move(TopLDecl));
  return true;
}

void SemaAnalyzer::actOnStructMemberDecl(std::unique_ptr<Decl> Var) {}

std::unique_ptr<forStmt> SemaAnalyzer::actOnForStmt(
    std::unique_ptr<Stmt> Init, std::unique_ptr<Expr> Cond,
    std::unique_ptr<Expr> Inc, std::unique_ptr<CompoundStmt> Body,
    llvm::SMLoc Left, llvm::SMLoc Right) {
  if (Cond->isError()) {
    return forStmt::Create(Left, Right, std::move(Init), std::move(Cond),
                           std::move(Inc), std::move(Body));
  }

  if (!Cond->getType()->isBoolType()) {
    Diags.emitDiagMsgRange(Inc->getStartLoc(), Inc->getEndLoc(),
                           Diag::err_if_condition_does_not_evaluate_to_bool);
    return forStmt::Create(Left, Right, std::move(Init), std::move(Cond),
                           std::move(Inc), std::move(Body));
  }

  return forStmt::Create(Left, Right, std::move(Init), std::move(Cond),
                         std::move(Inc), std::move(Body));
}

u_ptr<whileStmt>
SemaAnalyzer::actOnWhileStmt(std::unique_ptr<Expr> Condition,
                             std::unique_ptr<CompoundStmt> Compound,
                             llvm::SMLoc Left, llvm::SMLoc Right) const {
  if (Condition->isError()) {
    return whileStmt::Create(Left, Right, std::move(Condition),
                             std::move(Compound));
  }
  if (!Condition->getType()->isBoolType()) {
    Diags.emitDiagMsgRange(Left, Right, Diag::err_while_condition_not_bool);
  }

  return whileStmt::Create(Left, Right, std::move(Condition),
                           std::move(Compound));
}

u_ptr<TypeUse> SemaAnalyzer::actOnTypeUse(const Token &TypeName) {
  if (TypeName.isBaseType()) {
    auto BuiltinTy =
        getBuiltinType(BuiltInType::mapTokenToType(TypeName.getTag()));
    return TypeUse::Create(TypeName.getLoc(), TypeName.getRightmostLoc(),
                           BuiltinTy);
  }

  if (Type *FoundType = getType(TypeName.getIdentifierTableEntry())) {
    return TypeUse::Create(TypeName.getLoc(), TypeName.getRightmostLoc(),
                           FoundType);
  }

  Diags.emitDiagMsg(TypeName.getLoc(), Diag::err_var_not_found,
                    TypeName.getLexeme());
  return TypeUse::InvalidTypeUse(TypeName.getLoc(), TypeName.getRightmostLoc());
}

u_ptr<Stmt> SemaAnalyzer::actOnReturnStmt(const llvm::SMLoc ReturnLoc,
                                          llvm::SMLoc SemicolonLoc,
                                          std::unique_ptr<Expr> ReturnExpr) {
  Type *ReturnTy = getClosestFunctionReturnType();
  if (ReturnExpr->isError() or !ReturnTy) {
    // poison
    return ReturnStmt::Create(std::move(ReturnExpr));
  }
  if (!ReturnExpr) {
    if (!ReturnTy->isVoidType()) {
      Diags.emitDiagMsg(ReturnLoc, Diag::err_naked_return_on_non_void_fn);
      return ErrorStmt::Create(ReturnStmt::Create(std::move(ReturnExpr)));
    }
    return ReturnStmt::Naked();
  }

  if (!TypesAreCompatible(ReturnExpr->getType(), ReturnTy)) {
    Diags.emitDiagMsgRange(ReturnExpr->getStartLoc(), ReturnExpr->getEndLoc(),
                           Diag::err_incompatible_ret);
    return ErrorStmt::Create(ReturnStmt::Create(std::move(ReturnExpr)));
  }

  return ReturnStmt::Create(std::move(ReturnExpr));
}

u_ptr<Expr> SemaAnalyzer::actOnUnaryOp(Op::Unary Op,
                                       std::unique_ptr<Expr> ExprInput,
                                       llvm::SMLoc OpLoc) {
  Type *ExprType = ExprInput->getType();
  if (!ExprType) {
    return ErrorExpr::Create(UnaryOp::Create(std::move(ExprInput), Op, nullptr,
                                             OpLoc, ExprInput->getStartLoc(),
                                             Expr::Value));
  }

  bool Mismatch = false;

  switch (Op) {
  case Op::UO_unaryMinus:
    if (!ExprInput->isComplementable()) {
      Mismatch = true;
    }
    break;
  case Op::UO_preInc:
  case Op::UO_preDec: {
    if (!ExprInput->isIncrementable()) {
      Mismatch = true;
    }
    break;
  }
  case Op::UO_lNot: {
    if (!ExprType->isBoolType()) {
      Mismatch = true;
    }
    break;
  }
  default: llvm_unreachable("Invalid unary operator");
  }
  if (Mismatch) {
    Diags.emitDiagMsg(ExprInput->getStartLoc(), Diag::err_unary_op_incompatible,
                      getUnaryOpSpelling(Op), ExprInput->getType());
    return ErrorExpr::Create(UnaryOp::Create(std::move(ExprInput), Op, nullptr,
                                             OpLoc, ExprInput->getStartLoc(),
                                             Expr::Value));
  }

  return UnaryOp::Create(std::move(ExprInput), Op, ExprType, OpLoc,
                         ExprInput->getStartLoc(), Expr::Value);
}

std::unique_ptr<Expr> SemaAnalyzer::actOnBinaryOp(std::unique_ptr<Expr> LHS,
                                                  Op::Binary Op,
                                                  std::unique_ptr<Expr> RHS) {
  if (!LHS->getType() or !RHS->getType()) {
    return ErrorExpr::Create(
        BinaryOp::Create(std::move(LHS), std::move(RHS), Op, nullptr));
  }

  if (!checkBinOperands(*LHS, *RHS)) {
    Diags.emitDiagMsgRange(LHS->getStartLoc(), RHS->getEndLoc(),
                           Diag::err_incompatible_binary_operands,
                           LHS->getType()->getName(), RHS->getType()->getName(),
                           getBinaryOpSpelling(Op));
    return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
        std::move(LHS), std::move(RHS), Op, nullptr));
  }

  Type *resultType = nullptr;
  switch (Op) {
  case Op::BO_plus:
  case Op::BO_minus:
  case Op::BO_multiply:
  case Op::BO_divide: {
    resultType = LHS->getType();
    break;
  }
  case Op::BO_assign:
    if (!LHS->isLocationExpr()) {
      Diags.emitDiagMsg(LHS->getStartLoc(), Diag::err_invalid_lhs,
                        getBinaryOpSpelling(Op));
      return ErrorExpr::Create(
          BinaryOp::Create(std::move(LHS), std::move(RHS), Op, nullptr));
    }
    resultType = getVoid();
    break;
  case Op::BO_plusassign:
  case Op::BO_minusassign:
  case Op::BO_multassign:
  case Op::BO_divassign:
    if (!LHS->isLocationExpr()) {
      Diags.emitDiagMsg(LHS->getStartLoc(), Diag::err_invalid_lhs,
                        getBinaryOpSpelling(Op));
      return ErrorExpr::Create(
          BinaryOp::Create(std::move(LHS), std::move(RHS), Op, nullptr));
    }
    resultType = getVoid();
  case Op::BO_equals:
  case Op::BO_notequals:
  case Op::BO_lt:
  case Op::BO_gt:
  case Op::BO_ltequals:
  case Op::BO_gtequals: resultType = getBool(); break;
  case Op::BO_range:
  case Op::NUM_BINARY: break;
  }

  auto BinOp = BinaryOp::Create(std::move(LHS), std::move(RHS), Op, resultType);
  return BinOp;
}

u_ptr<Expr>
SemaAnalyzer::actOnFunctionCall(const Token &ID, llvm::SMLoc RParenLoc,
                                llvm::SmallVector<u_ptr<Expr>> PassedArgs) {
  Decl *FoundFunction =
      lookup(ID.getIdentifierTableEntry());// check if name exists

  if (!FoundFunction) {
    Diags.emitDiagMsg(ID.getLoc(), Diag::err_fn_not_found, ID.getIdentifier());
    return ErrorExpr::Create(FunctionCall::Create(ID.getIdentifierTableEntry(),
                                                  std::move(PassedArgs),
                                                  ID.getLoc(), RParenLoc));
  }

  if (!llvm::isa<FunctionDecl>(FoundFunction)) {
    // is it a function
    Diags.emitDiagMsg(ID.getLoc(), Diag::err_name_not_fn, ID.getLexeme());
    return ErrorExpr::Create(FunctionCall::Create(ID.getIdentifierTableEntry(),
                                                  std::move(PassedArgs),
                                                  ID.getLoc(), RParenLoc));
  }

  auto *FunctionPtr = llvm::dyn_cast<FunctionDecl>(FoundFunction);
  auto ParamsVec = FunctionPtr->getParamsVector();
  if (ParamsVec.size() != PassedArgs.size()) {
    // check formal params size vs passed arguments
    Diags.emitDiagMsgRange(ID.getLoc(), RParenLoc,
                           Diag::err_wrong_number_of_parameters,
                           PassedArgs.size(), PassedArgs.size());
    return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(
        ID.getIdentifierTableEntry(), std::move(PassedArgs), ID.getLoc(),
        RParenLoc));
  }
  bool FailedTypeChecking = false;
  for (size_t i = 0; i < ParamsVec.size(); i++) {
    // type check each param
    if (!TypesAreCompatible(ParamsVec[i]->getTypePtr(),
                            PassedArgs[i]->getType())) {
      Diags.emitDiagMsg(
          PassedArgs[i]->getStartLoc(), Diag::err_incompatible_type_passed,
          ParamsVec[i]->getName(), ParamsVec[i]->getTypePtr()->getName(),
          PassedArgs[i]->getType()->getName());
      FailedTypeChecking = true;
    }
  }

  if (FailedTypeChecking) {
    return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(
        ID.getIdentifierTableEntry(), std::move(PassedArgs), ID.getLoc(),
        RParenLoc));
  }

  return std::make_unique<FunctionCall>(ID.getIdentifierTableEntry(),
                                        std::move(PassedArgs), ID.getLoc(),
                                        RParenLoc, FunctionPtr->getTypePtr());
}

std::unique_ptr<DeclStmt>
SemaAnalyzer::actOnVarDeclStmt(std::unique_ptr<VarDecl> NameDecl,
                               std::unique_ptr<Expr> ExprInput,
                               llvm::SMLoc SemicolonLoc) {
  auto TheDecl = NameDecl.get();
  if (lookup(NameDecl->getEntry())) {
    Diags.emitDiagMsgRange(NameDecl->getStart(), NameDecl->getEnd(),
                           Diag::err_var_redefinition, NameDecl->getName());
  }
  insert(std::move(NameDecl));

  if (!ExprInput) {
    return std::make_unique<DeclStmt>(TheDecl->getStart(), SemicolonLoc,
                                      TheDecl, nullptr);
  }
  if (!ExprInput->getType()) {
    return std::make_unique<DeclStmt>(TheDecl->getStart(), SemicolonLoc,
                                      TheDecl, std::move(ExprInput));
  }

  if (!TypesAreCompatible(NameDecl->getTypePtr(), ExprInput->getType())) {
    Diags.emitDiagMsgRange(ExprInput->getStartLoc(), ExprInput->getEndLoc(),
                           Diag::err_incompatible_type_var_decl,
                           TheDecl->getName(), TheDecl->getTypePtr()->getName(),
                           ExprInput->getType()->getName());
  }

  return std::make_unique<DeclStmt>(TheDecl->getStart(), SemicolonLoc, TheDecl,
                                    std::move(ExprInput));
}

std::unique_ptr<Expr> SemaAnalyzer::actOnNameUsage(Token &Identifier) {
  assert(Identifier.getTag() == tok::Tag::identifier
         && "Trying to lookup a tag that isn't an identifier");
  Decl *Found = lookup(Identifier.getIdentifierTableEntry());
  if (!Found) {
    Diags.emitDiagMsg(Identifier.getLoc(), Diag::err_var_not_found,
                      Identifier.getIdentifier());

    return std::make_unique<ErrorExpr>(std::make_unique<NameUsage>(
        Identifier.getIdentifierTableEntry(), nullptr, Identifier.getLoc(),
        Identifier.getRightmostLoc(), nullptr));
  }
  auto *Variable = llvm::dyn_cast<VarDecl>(Found);
  if (!Variable) {
    Diags.emitDiagMsg(Identifier.getLoc(), Diag::err_var_not_found,
                      Identifier.getIdentifier());
    return std::make_unique<ErrorExpr>(std::make_unique<NameUsage>(
        Identifier.getIdentifierTableEntry(), nullptr, Identifier.getLoc(),
        Identifier.getRightmostLoc(), nullptr));
  }
  return std::make_unique<NameUsage>(
      Identifier.getIdentifierTableEntry(), Variable, Identifier.getLoc(),
      Identifier.getRightmostLoc(), Variable->getTypePtr());
}

void SemaAnalyzer::actOnFunctionDecl(
    FunctionDecl *Function, std::unique_ptr<CompoundStmt> CompoundToAttach) {
  Function->setCompound(std::move(CompoundToAttach));
  exitScope();
}

std::unique_ptr<IntegerLiteral>
SemaAnalyzer::actOnIntegerLiteral(Token &Literal) {
  uint32_t NumBits =
      llvm::APInt::getSufficientBitsNeeded(Literal.getLexeme(), 10);

  llvm::APInt ApInt = llvm::APInt(NumBits, Literal.getLexeme(), 10);
  auto IntLiteral = std::make_unique<IntegerLiteral>(ApInt, Literal.getLoc(),
                                                     Literal.getRightmostLoc());
  IntLiteral->setType(getBuiltInType(BuiltInType::int_literal));
  return IntLiteral;
}

std::unique_ptr<FloatingLiteral>
SemaAnalyzer::actOnFloatingLiteral(Token &Literal) {
  llvm::APFloat ApFloatSingle =
      llvm::APFloat(llvm::APFloat::IEEEdouble(),
                    Literal.getLexeme());// TODO: find out how to use APFloat
  std::unique_ptr<FloatingLiteral> FloatLit = std::make_unique<FloatingLiteral>(
      ApFloatSingle, Literal.getLoc(), Literal.getRightmostLoc());
  FloatLit->setType(getBuiltInType(BuiltInType::float_literal));
  return FloatLit;
}

std::unique_ptr<BooleanLiteral>
SemaAnalyzer::actOnBooleanLiteral(Token &Literal) {
  assert((Literal.getTag() == tok::kw_true || Literal.getTag() == tok::kw_false)
         && "Token tag is not true or false in actOnBooleanLiteral");
  auto BoolLit = std::make_unique<BooleanLiteral>(
      Literal.getTag() == tok::kw_true, Literal.getLoc(),
      Literal.getRightmostLoc());
  BoolLit->setType(getBool());

  return BoolLit;
}

std::unique_ptr<StrLiteral> SemaAnalyzer::actOnStrLiteral(Token &Literal) {
  assert(Literal.is(tok::string_literal)
         && "Tag is not a string in string literal");

  return std::make_unique<StrLiteral>(
      Literal.getLexeme().size() - 2, Literal.getLexeme(), Literal.getLoc(),
      Literal.getRightmostLoc(), getBuiltInType(BuiltInType::string));
}

Decl *SemaAnalyzer::lookup(IDTableEntry *var) {
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

  return std::make_unique<VarDecl>(std::move(Type), Entry, Loc, Name.getLoc());
  ;
}

Type *SemaAnalyzer::getType(IDTableEntry *Ty) {
  auto Found = Types.find(Ty);
  if (Found == Types.end()) {
    return nullptr;
  }
  return Found->second;
}

std::unique_ptr<Expr>
SemaAnalyzer::actOnIndexOperation(std::unique_ptr<Expr> Accessed,
                                  std::unique_ptr<Expr> AccessExpr,
                                  llvm::SMLoc Left, llvm::SMLoc Right) {
  if (!AccessExpr->getType()->isIntType()) {
    Diags.emitDiagMsgRange(Left, Right,
                           Diag::err_array_index_expr_not_whole_number,
                           AccessExpr->getType()->getName());
    return std::make_unique<ErrorExpr>(std::make_unique<ArrayIndex>(
        std::move(Accessed), std::move(AccessExpr), nullptr, Left, Right));
  }

  auto *ArrTy = llvm::dyn_cast<PointerType>(Accessed->getType());
  ;
  if (!ArrTy) {
    Diags.emitDiagMsgRange(Left, Right, Diag::err_indexing_on_non_array,
                           Accessed->getType()->getName());
    return std::make_unique<ErrorExpr>(std::make_unique<ArrayIndex>(
        std::move(Accessed), std::move(AccessExpr), nullptr, Left, Right));
  }
  return std::make_unique<ArrayIndex>(std::move(Accessed),
                                      std::move(AccessExpr),
                                      ArrTy->getPointeeType(), Left, Right);
}
std::unique_ptr<Expr>
SemaAnalyzer::actOnMemberExpr(std::unique_ptr<Expr> Accessed,
                              std::unique_ptr<Expr> Accessor) {
  Type *AccessedType = Accessed->getType();

  if (!AccessedType) {
    return std::make_unique<ErrorExpr>(std::make_unique<MemberAccess>(
        std::move(Accessed), nullptr, nullptr, Accessed->getStartLoc(),
        Accessor->getStartLoc()));
  }
  auto Name = llvm::dyn_cast<NameUsage>(Accessor.get());
  if (!Name) {
    return std::make_unique<ErrorExpr>(std::make_unique<MemberAccess>(
        std::move(Accessed), nullptr, nullptr, Accessed->getStartLoc(),
        Accessor->getStartLoc()));
  }

  if (auto RecordTy = llvm::dyn_cast<RecordType>(AccessedType)) {
    Decl *Get = RecordTy->lookup(Name->getEntry());
    if (!Get) {
      Diags.emitDiagMsg(Accessor->getStartLoc(),
                        Diag::err_type_member_does_not_exist,
                        RecordTy->getName(), Name->getUsedName());
      return std::make_unique<ErrorExpr>(std::make_unique<MemberAccess>(
          std::move(Accessed), nullptr, nullptr, Accessed->getStartLoc(),
          Accessor->getStartLoc()));
    }
    VarDecl *Member = llvm::dyn_cast<VarDecl>(Get);
    assert(Member);
    return std::make_unique<MemberAccess>(
        std::move(Accessed), Member, Member->getTypePtr(),
        Accessed->getStartLoc(), Accessor->getStartLoc());
  }
  Diags.emitDiagMsg(Accessor->getStartLoc(), Diag::err_type_has_no_members,
                    Name->getUsedName(), AccessedType->getName());

  return std::make_unique<ErrorExpr>(std::make_unique<MemberAccess>(
      std::move(Accessed), nullptr, nullptr, Accessed->getStartLoc(),
      Accessor->getStartLoc()));
}

std::unique_ptr<loopStmt>
SemaAnalyzer::actOnLoopStmt(std::unique_ptr<CompoundStmt> Compound,
                            llvm::SMLoc LoopLoc) {
  return std::make_unique<loopStmt>(std::move(Compound), LoopLoc);
}

std::unique_ptr<Expr>
SemaAnalyzer::actOnDereference(std::unique_ptr<Expr> ExprInput,
                               size_t DerefCount) {
  // TODO
  return nullptr;
}
std::unique_ptr<ifStmt>
SemaAnalyzer::actOnIfStmt(llvm::SMLoc IfLoc, llvm::SMLoc EndOfExprLoc,
                          std::unique_ptr<Expr> IfCondition,
                          std::unique_ptr<CompoundStmt> FirstBlock,
                          std::unique_ptr<elifStmt> ElifChain,
                          std::unique_ptr<CompoundStmt> ElseBlock) {
  if (!IfCondition->getType()) {
    return std::make_unique<ifStmt>(IfLoc, std::move(IfCondition), EndOfExprLoc,
                                    std::move(FirstBlock), std::move(ElifChain),
                                    std::move(ElseBlock));
  }

  if (!IfCondition->getType()->isBoolType()) {
    Diags.emitDiagMsgRange(
        IfCondition->getStartLoc(), IfCondition->getEndLoc(),
        Diag::err_if_condition_does_not_evaluate_to_bool /*, PLACEHOLDER*/);
  }

  return std::make_unique<ifStmt>(IfLoc, std::move(IfCondition), EndOfExprLoc,
                                  std::move(FirstBlock), std::move(ElifChain),
                                  std::move(ElseBlock));
}

SemaAnalyzer::SemaAnalyzer(std::shared_ptr<DiagEngine> diag)
    : Diags(std::move(diag)), BuiltinTypes(llvm::SmallVector<BuiltInType *>()),
      DeclScope(std::make_unique<Scope>()), CurrentFunctionReturnType(nullptr) {
  init();
}

bool Scope::insertInContext(std::unique_ptr<Decl> Dec) {
  return ScopeContext->insert(Dec->getEntry(), std::move(Dec));
}

DeclContext *Scope::getContext() { return ScopeContext; }

std::unique_ptr<Scope> Scope::moveParentScope() { return std::move(Parent); }

bool Scope::isGlobalScope() { return Parent.get(); }

Scope *Scope::getParent() { return Parent.get(); }

Decl *Scope::lookup(const IDTableEntry *Name) {
  auto Found = LookupTable.find(Name);
  if (Found == LookupTable.end()) {
    return nullptr;
  }
  return Found->second;
}

void SemaAnalyzer::enterScope(Scope::ScopeKind K) {
  DeclScope = std::make_unique<Scope>(std::move(DeclScope), K);
}

bool SemaAnalyzer::insert(std::unique_ptr<Decl> ToInsert) {
  DeclScope->insertInLexicalScope(ToInsert->getEntry(), ToInsert.get());
  return DeclScope->insertInContext(std::move(ToInsert));
}

void SemaAnalyzer::actOnParamDecl(
    llvm::SmallVector<std::unique_ptr<ParamDecl>> &CurrentParamList,
    std::unique_ptr<ParamDecl> Param,
    llvm::DenseMap<IDTableEntry *, ParamDecl *> &CheckAgainst) {
  auto Found = CheckAgainst.find(Param->getEntry());

  if (Found != CheckAgainst.end()) {
    Diags.emitDiagMsg(Param->getStart(), Diag::err_param_redifinition,
                      Param->getName());
  } else {
    CurrentParamList.push_back(std::move(Param));
  }
}

Decl *SemaAnalyzer::lookupOne(IDTableEntry *Var) {
  return DeclScope->lookup(Var);
}

void SemaAnalyzer::exitFunctionScope() { exitScope(); }

std::unique_ptr<FunctionDecl> SemaAnalyzer::enterFunctionScope(
    std::unique_ptr<TypeUse> FunctionReturnType,
    u_ptr<llvm::SmallVector<u_ptr<ParamDecl>>> Params, Token &FunctionName,
    llvm::SMLoc RParenLoc) {
  CurrentFunctionReturnType = FunctionReturnType->getTypePtr();
  auto ParamsRef = Params.get();
  std::unique_ptr<FunctionDecl> NewCtx = std::make_unique<FunctionDecl>(
      std::move(FunctionReturnType), FunctionName.getIdentifierTableEntry(),
      FunctionName.getLoc(), RParenLoc, std::move(Params),
      DeclScope->getContext());
  auto NewScope = std::make_unique<Scope>(std::move(DeclScope),
                                          Scope::FunctionScope, NewCtx.get());
  DeclScope = std::move(NewScope);

  DeclScope->insertInLexicalScope(NewCtx->getEntry(), NewCtx.get());
  for (auto &Param : *ParamsRef) {
    DeclScope->insertInLexicalScope(Param->getEntry(), Param.get());
  }

  return NewCtx;
}

std::unique_ptr<RecordDecl>
SemaAnalyzer::enterStructScope(Token &StructDetails) {
  IDTableEntry *StructName = StructDetails.getIdentifierTableEntry();
  auto NewCtx = std::make_unique<RecordDecl>(StructName, nullptr,
                                             DeclScope->getContext());
  auto NewScope = std::make_unique<Scope>(std::move(DeclScope),
                                          Scope::StructScope, NewCtx.get());

  DeclScope = std::move(NewScope);

  return std::move(NewCtx);
}

void SemaAnalyzer::exitScope() {
  assert(!DeclScope->isGlobalScope() && "Attempted to exit global scope\n");
  DeclScope = DeclScope->moveParentScope();
}

std::unique_ptr<Stmt> SemaAnalyzer::actOnBreakStmt(Token &BreakLoc) {
  if (!DeclScope->getClosestLoopScope()) {
    Diags.emitDiagMsg(BreakLoc.getLoc(), Diag::err_break_in_non_loop);
    return std::make_unique<ErrorStmt>(std::make_unique<BreakStmt>(
        BreakLoc.getLoc(), BreakLoc.getRightmostLoc()));
  }
  return std::make_unique<BreakStmt>(BreakLoc.getLoc(),
                                     BreakLoc.getRightmostLoc());
}

std::unique_ptr<Stmt> SemaAnalyzer::actOnNextStmt(Token &NextLoc) {
  if (!DeclScope->getClosestLoopScope()) {
    Diags.emitDiagMsg(NextLoc.getLoc(), Diag::err_next_in_non_loop);
    return std::make_unique<ErrorStmt>(std::make_unique<NextStmt>(
        NextLoc.getLoc(), NextLoc.getRightmostLoc()));
  }

  return std::make_unique<NextStmt>(NextLoc.getLoc(),
                                    NextLoc.getRightmostLoc());
}

Type *SemaAnalyzer::getBuiltInType(BuiltInType::BTKind Ty) {
  return BuiltinTypes[Ty];
}

Type *SemaAnalyzer::getResultTyForBinOp(Expr *LHS, Expr *RHS) {
  /*
      Types should have been validated and be compatible with each other
    */
  Type *LHSTy = LHS->getType();
  Type *RHSTy = RHS->getType();
  if (LHSTy == RHSTy) {
    // they're both the same, not a problem if they're both literals;
    return LHSTy;
  }
  if (RHSTy->isIntLiteral() || RHSTy->isFloatLiteral()) {
    return LHSTy;
  }

  return RHSTy;// by elimination the LHS must be a literal and we want to promote the result to an actual type
}

void SemaAnalyzer::enterLoopScope() { enterScope(Scope::LoopScope); }
}// namespace funLang
