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
    BuiltinTypes[BuiltInType::int_literal] = new BuiltInType(BuiltInType::int_literal);
    BuiltinTypes[BuiltInType::float_literal] = new BuiltInType(BuiltInType::float_literal);
  }

  bool SemaAnalyzer::actOnTopLevelDecl(std::unique_ptr<Decl> TopLDecl) {
    auto Ref = TopLDecl.get();
    if (lookup(TopLDecl->getEntry())) {
      Diags.emitDiagMsg(Ref->getStart(), Diag::err_toplevel_redefinition, TopLDecl->getName());
      return false;
    }

    insert(std::move(TopLDecl));
    return true;
  }

  void SemaAnalyzer::actOnStructMemberDecl(std::unique_ptr<Decl> Var) {
    // const auto Field = llvm::dyn_cast<ParamDecl *>(*Var);
    // assert(Field && "Not in a struct!");
    // if (lookupOne(Field->getEntry())) {
    //   //	auto Record = llvm::dyn_cast<RecordDecl>(DeclScope->getContext());
    //   Diags->emitDiagMsg(Var->getStart(), Diag::err_struct_var_redefinition, Var->getName());
    //   return;
    // }
    insert(std::move(Var));
  }

  std::unique_ptr<forStmt> SemaAnalyzer::actOnForStmt(
    std::unique_ptr<Stmt> Init,
    std::unique_ptr<Expr> Cond,
    std::unique_ptr<Expr> Inc,
    std::unique_ptr<CompoundStmt> Body,
    llvm::SMLoc Left,
    llvm::SMLoc Right) {
    if (!Cond->getType()) {
      return std::make_unique<forStmt>(Left, Right, std::move(Init), std::move(Cond), std::move(Inc), std::move(Body));
    }

    if (!Cond->getType()->isBoolType()) {
      Diags.emitDiagMsgRange(Inc->getStartLoc(), Inc->getEndLoc(), Diag::err_if_condition_does_not_evaluate_to_bool);
      return nullptr;
    }

    return std::make_unique<forStmt>(Left, Right, std::move(Init), std::move(Cond), std::move(Inc), std::move(Body));
  }

  std::unique_ptr<whileStmt>
  SemaAnalyzer::actOnWhileStmt(std::unique_ptr<Expr> Condition,
                               std::unique_ptr<CompoundStmt> Compound,
                               llvm::SMLoc Left,
                               llvm::SMLoc Right) {
    if (!Condition->getType()) {
      return std::make_unique<whileStmt>(std::move(Condition),
                                         std::move(Compound),
                                         Left,
                                         Right);
    }
    if (!Condition->getType()->isBoolType()) {
      Diags.emitDiagMsgRange(Left, Right, Diag::err_while_condition_not_bool);
    }

    return std::make_unique<whileStmt>(std::move(Condition),
                                       std::move(Compound),
                                       Left,
                                       Right);
  }
  std::unique_ptr<TypeUse> SemaAnalyzer::actOnTypeUse(Token &TypeName, size_t IndirectionCount) {
    if (TypeName.isBaseType()) {
      auto BuiltinTy = getBuiltInType(BuiltInType::mapTokenToType(TypeName.getTag()));

      return std::make_unique<TypeUse>(BuiltinTy, TypeName.getLoc(), TypeName.getRightmostLoc());
    }

    if (Type *FoundType = getType(TypeName.getIdentifierTableEntry())) {
      return std::make_unique<TypeUse>(FoundType, TypeName.getLoc(), TypeName.getRightmostLoc());
    } else {
      Diags.emitDiagMsg(TypeName.getLoc(),
                        Diag::err_var_not_found,
                        TypeName.getLexeme());
      return std::make_unique<TypeUse>(nullptr, TypeName.getLoc(), TypeName.getRightmostLoc());
    }
  }

  std::unique_ptr<ReturnStmt>
  funLang::SemaAnalyzer::actOnReturnStmt(llvm::SMLoc ReturnLoc,
                                         llvm::SMLoc SemicolonLoc,
                                         std::unique_ptr<Expr> ReturnExpr) {
    if (!CurrentFunctionReturnType || !ReturnExpr->getType()) {
      // poison
      return std::make_unique<ReturnStmt>(std::move(ReturnExpr));
    }
    if (!ReturnExpr) {
      if (!CurrentFunctionReturnType->isVoidType()) {
        Diags.emitDiagMsg(ReturnLoc, Diag::err_naked_return_on_non_void_fn);
      }
      return std::make_unique<ReturnStmt>(nullptr);
    }

    if (!CurrentFunctionReturnType->eqTo(ReturnExpr->getType())) {
      Diags.emitDiagMsgRange(ReturnExpr->getStartLoc(), ReturnExpr->getEndLoc(), Diag::err_incompatible_ret);
      return std::make_unique<ReturnStmt>(
        std::make_unique<ErrorExpr>(std::move(ReturnExpr)));
    }
    return std::make_unique<ReturnStmt>(std::move(ReturnExpr));
  }

  std::unique_ptr<Expr>
  funLang::SemaAnalyzer::actOnUnaryOp(Basic::Op::Unary Op,
                                      std::unique_ptr<Expr> ExprInput,
                                      llvm::SMLoc OpLoc) {
    Type *ExprType = ExprInput->getType();
    if (!ExprType) {
      return std::make_unique<ErrorExpr>(std::make_unique<UnaryOp>(std::move(ExprInput),
                                                                   Op,
                                                                   nullptr,
                                                                   OpLoc,
                                                                   ExprInput->getStartLoc()));
    }

    bool Mismatch = false;

    switch (Op) {
    case Basic::Op::UO_unaryMinus:
      if (!ExprInput->isComplementable()) {
        Mismatch = true;
      }
      break;
    case Basic::Op::UO_preInc:
    case Basic::Op::UO_preDec: {
      if (!ExprInput->isIncrementable()) {
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
      Diags.emitDiagMsg(ExprInput->getStartLoc(),
                        Diag::err_unary_op_incompatible,
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
                                                    Op::Binary Op,
                                                    std::unique_ptr<Expr> RHS) {
    if (!LHS->getType() || !LHS->getType()) {
      return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
        std::move(LHS),
        std::move(RHS),
        Op,
        nullptr));
    }

    if (!LHS->isCompatibleWith(RHS.get())) {
      Diags.emitDiagMsgRange(LHS->getStartLoc(),
                             RHS->getEndLoc(),
                             Diag::err_incompatible_binary_operands,
                             LHS->getType()->getName(),
                             RHS->getType()->getName(),
                             Basic::Op::getBinaryOpSpelling(Op));
      return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
        std::move(LHS),
        std::move(RHS),
        Op,
        nullptr));
    }

    Type *resultType = nullptr;
    switch (Op) {
    case Basic::Op::BO_plus:
    case Basic::Op::BO_minus:
    case Basic::Op::BO_multiply:
    case Basic::Op::BO_divide: {
      resultType = LHS->getType();
      break;
    }
    case Basic::Op::BO_assign:
      if (!LHS->isAssignable()) {
        Diags.emitDiagMsg(LHS->getStartLoc(), Diag::err_invalid_lhs, Basic::Op::getBinaryOpSpelling(Op));
        return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
          std::move(LHS),
          std::move(RHS),
          Op,
          nullptr));
      }
      resultType = getVoid();
      break;
    case Basic::Op::BO_plusassign:
    case Basic::Op::BO_minusassign:
    case Basic::Op::BO_multassign:
    case Basic::Op::BO_divassign:
      if (!LHS->isArithAssignable()) {
        Diags.emitDiagMsg(LHS->getStartLoc(), Diag::err_invalid_lhs, Basic::Op::getBinaryOpSpelling(Op));
        return std::make_unique<ErrorExpr>(std::make_unique<BinaryOp>(
          std::move(LHS),
          std::move(RHS),
          Op,
          nullptr));
      }
      resultType = getVoid();
    case Basic::Op::BO_equals:
    case Basic::Op::BO_notequals:
    case Basic::Op::BO_lt:
    case Basic::Op::BO_gt:
    case Basic::Op::BO_ltequals:
    case Basic::Op::BO_gtequals:
      resultType = getBool();
      break;
    case Basic::Op::BO_range:
    case Basic::Op::NUM_BINARY: break;
    }

    auto BinOp = std::make_unique<BinaryOp>(std::move(LHS),
                                            std::move(RHS),
                                            Op,
                                            resultType);
    return BinOp;
  }

  std::unique_ptr<Expr>
  SemaAnalyzer::actOnFunctionCall(Token &ID,
                                  llvm::SMLoc RParenLoc,
                                  u_ptr<llvm::SmallVector<u_ptr<Expr> > > PassedArgs) {
    Decl *FoundFunction = lookup(ID.getIdentifierTableEntry()); // check if name exists

    if (!FoundFunction) {
      Diags.emitDiagMsg(ID.getLoc(), Diag::err_fn_not_found, ID.getIdentifier());
      return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(ID.getIdentifierTableEntry(),
                                                                        std::move(PassedArgs),
                                                                        ID.getLoc(),
                                                                        RParenLoc));
    }
    if (!llvm::isa<FunctionDecl>(FoundFunction)) {
      // is it a function
      Diags.emitDiagMsg(ID.getLoc(), Diag::err_name_not_fn, ID.getLexeme());
      return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(ID.getIdentifierTableEntry(),
                                                                        std::move(PassedArgs),
                                                                        ID.getLoc(),
                                                                        RParenLoc));
    }
    FunctionDecl *FunctionPtr = llvm::dyn_cast<FunctionDecl>(FoundFunction);
    auto &FunctionParams = FunctionPtr->getParams();
    if (FunctionParams.size() != PassedArgs->size()) {
      // check formal params size vs passed arguments
      Diags.emitDiagMsgRange(ID.getLoc(),
                             RParenLoc,
                             Diag::err_wrong_number_of_parameters,
                             FunctionParams.size(),
                             PassedArgs->size());
      return std::make_unique<ErrorExpr>(std::make_unique<FunctionCall>(ID.getIdentifierTableEntry(),
                                                                        std::move(PassedArgs),
                                                                        ID.getLoc(),
                                                                        RParenLoc));
    }
    bool FailedTypeChecking = false;
    for (size_t i = 0; i < FunctionParams.size(); i++) {
      // type check each param
      if (!FunctionParams[i]->getTypePtr()->LHSTyCompatibleRHSTy((*PassedArgs)[i]->getType())) {
        Diags.emitDiagMsg((*PassedArgs)[i]->getStartLoc(),
                          Diag::err_incompatible_type_passed,
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
                                          RParenLoc,
                                          FunctionPtr->getTypePtr());
  }

  std::unique_ptr<DeclStmt>
  SemaAnalyzer::actOnVarDeclStmt(std::unique_ptr<VarDecl> NameDecl,
                                 std::unique_ptr<Expr> ExprInput,
                                 llvm::SMLoc SemicolonLoc) {
    auto TheDecl = NameDecl.get();
    if (lookup(NameDecl->getEntry())) {
      Diags.emitDiagMsgRange(NameDecl->getStart(),
                             NameDecl->getEnd(),
                             Diag::err_var_redefinition,
                             NameDecl->getName());
    }
    insert(std::move(NameDecl));

    if (!ExprInput) {
      return std::make_unique<DeclStmt>(TheDecl->getStart(), SemicolonLoc, TheDecl, nullptr);
    }
    if (!ExprInput->getType()) {
      return std::make_unique<DeclStmt>(TheDecl->getStart(),
                                        SemicolonLoc,
                                        TheDecl,
                                        std::move(ExprInput));
    }

    if (!NameDecl->getTypePtr()->LHSTyCompatibleRHSTy(ExprInput->getType())) {
      Diags.emitDiagMsgRange(
        ExprInput->getStartLoc(),
        ExprInput->getEndLoc(),
        Diag::err_incompatible_type_var_decl,
        TheDecl->getName(),
        TheDecl->getTypePtr()->getName(),
        ExprInput->getType()->getName());
    }

    return std::make_unique<DeclStmt>(TheDecl->getStart(), SemicolonLoc, TheDecl, std::move(ExprInput));
  }

  std::unique_ptr<Expr>
  funLang::SemaAnalyzer::actOnNameUsage(Token &Identifier) {
    assert(Identifier.getTag() == Basic::tok::Tag::identifier && "Trying to lookup a tag that isn't an identifier");
    Decl *Found = lookup(Identifier.getIdentifierTableEntry());
    if (!Found) {
      Diags.emitDiagMsg(Identifier.getLoc(),
                        Diag::err_var_not_found,
                        Identifier.getIdentifier());

      return std::make_unique<ErrorExpr>(std::make_unique<NameUsage>(Identifier.getIdentifierTableEntry(),
                                                                     nullptr,
                                                                     Identifier.getLoc(),
                                                                     Identifier.getRightmostLoc(),
                                                                     nullptr));
    }
    auto *Variable = llvm::dyn_cast<VarDecl>(Found);
    if (!Variable) {
      Diags.emitDiagMsg(Identifier.getLoc(),
                        Diag::err_var_not_found,
                        Identifier.getIdentifier());
      return std::make_unique<ErrorExpr>(std::make_unique<NameUsage>(Identifier.getIdentifierTableEntry(),
                                                                     nullptr,
                                                                     Identifier.getLoc(),
                                                                     Identifier.getRightmostLoc(),
                                                                     nullptr));
    }
    return std::make_unique<NameUsage>(Identifier.getIdentifierTableEntry(),
                                       Variable,
                                       Identifier.getLoc(),
                                       Identifier.getRightmostLoc(),
                                       Variable->getTypePtr());
  }

  void funLang::SemaAnalyzer::actOnFunctionDecl(FunctionDecl *Function,
                                                std::unique_ptr<CompoundStmt> CompoundToAttach) {
    Function->setCompound(std::move(CompoundToAttach));
    exitScope();
  }

  std::unique_ptr<IntegerLiteral>
  SemaAnalyzer::actOnIntegerLiteral(Token &Literal) {
    uint32_t NumBits =
        llvm::APInt::getSufficientBitsNeeded(Literal.getLexeme(), 10);

    llvm::APInt ApInt = llvm::APInt(NumBits, Literal.getLexeme(), 10);
    auto IntLiteral = std::make_unique<IntegerLiteral>(ApInt, Literal.getLoc(), Literal.getRightmostLoc());
    IntLiteral->setType(getBuiltInType(BuiltInType::int_literal));
    return IntLiteral;
  }

  std::unique_ptr<FloatingLiteral>
  SemaAnalyzer::actOnFloatingLiteral(Token &Literal) {
    llvm::APFloat ApFloatSingle =
        llvm::APFloat(llvm::APFloat::IEEEdouble(),
                      Literal.getLexeme()); // TODO: find out how to use APFloat
    std::unique_ptr<FloatingLiteral> FloatLit =
        std::make_unique<FloatingLiteral>(ApFloatSingle, Literal.getLoc(), Literal.getRightmostLoc());
    FloatLit->setType(getBuiltInType(BuiltInType::float_literal));
    return FloatLit;
  }

  std::unique_ptr<BooleanLiteral>
  SemaAnalyzer::actOnBooleanLiteral(Token &Literal) {
    assert(
      (Literal.getTag() == Basic::tok::kw_true || Literal.getTag() == Basic::tok::kw_false) &&
      "Token tag is not true or false in actOnBooleanLiteral");
    auto BoolLit = std::make_unique<BooleanLiteral>(
      Literal.getTag() == Basic::tok::kw_true,
      Literal.getLoc(),
      Literal.getRightmostLoc());
    BoolLit->setType(getBool());

    return BoolLit;
  }

  std::unique_ptr<StrLiteral> SemaAnalyzer::actOnStrLiteral(Token &Literal) {
    assert(Literal.is(Basic::tok::string_literal) && "Tag is not a string in string literal");

    return std::make_unique<StrLiteral>(
      Literal.getLexeme().size() - 2,
      Literal.getLexeme(),
      Literal.getLoc(),
      Literal.getRightmostLoc(),
      getBuiltInType(BuiltInType::string));
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

  Type *funLang::SemaAnalyzer::getType(IDTableEntry *Ty) {
    auto Found = Types.find(Ty);
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
      Diags.emitDiagMsgRange(Left,
                             Right,
                             Diag::err_array_index_expr_not_whole_number,
                             AccessExpr->getType()->getName());
      return std::make_unique<ErrorExpr>(std::make_unique<ArrayIndex>(std::move(Accessed),
                                                                      std::move(AccessExpr),
                                                                      nullptr,
                                                                      Left,
                                                                      Right));
    }

    auto *ArrTy = llvm::dyn_cast<PointerType>(Accessed->getType());;
    if (!ArrTy) {
      Diags.emitDiagMsgRange(Left, Right, Diag::err_indexing_on_non_array, Accessed->getType()->getName());
      return std::make_unique<ErrorExpr>(std::make_unique<ArrayIndex>(std::move(Accessed),
                                                                      std::move(AccessExpr),
                                                                      nullptr,
                                                                      Left,
                                                                      Right));
    }
    return std::make_unique<ArrayIndex>(std::move(Accessed),
                                        std::move(AccessExpr),
                                        ArrTy->getPointeeType(),
                                        Left,
                                        Right);
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
        Diags.emitDiagMsg(Accessor->getStartLoc(),
                          Diag::err_type_member_does_not_exist,
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
    Diags.emitDiagMsg(Accessor->getStartLoc(),
                      Diag::err_type_has_no_members,
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
      return std::make_unique<ifStmt>(IfLoc,
                                      std::move(IfCondition),
                                      EndOfExprLoc,
                                      std::move(FirstBlock),
                                      std::move(ElifChain),
                                      std::move(ElseBlock));
    }

    if (!IfCondition->getType()->isBoolType()) {
      Diags.emitDiagMsgRange(IfCondition->getStartLoc(),
                             IfCondition->getEndLoc(),
                             Diag::err_if_condition_does_not_evaluate_to_bool /*, PLACEHOLDER*/);
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
      BuiltinTypes(llvm::SmallVector<BuiltInType *>()), DeclScope(std::make_unique<Scope>()),
      CurrentFunctionReturnType(nullptr) {
    init();
  }

  bool Scope::insertInContext(std::unique_ptr<Decl> Dec) {
    return ScopeContext->insert(Dec->getEntry(), std::move(Dec));
  }

  DeclContext *Scope::getContext() { return ScopeContext; }

  std::unique_ptr<Scope> Scope::moveParentScope() {
    return std::move(Parent);
  }

  bool Scope::isGlobalScope() {
    return Parent.get();
  }

  Scope *Scope::getParent() { return Parent.get(); }

  Decl *Scope::lookup(const IDTableEntry *Name) {
    auto Found = LookupTable.find(Name);
    if (Found == LookupTable.end()) {
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

  void SemaAnalyzer::actOnParamDecl(llvm::SmallVector<std::unique_ptr<ParamDecl> > &CurrentParamList,
                                    std::unique_ptr<ParamDecl> Param,
                                    llvm::DenseMap<IDTableEntry *, ParamDecl *> &CheckAgainst) {
    auto Found = CheckAgainst.find(Param->getEntry());

    if (Found != CheckAgainst.end()) {
      Diags.emitDiagMsg(Param->getStart(), Diag::err_param_redifinition, Param->getName());
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
                                                                   ParamDecl> > >
                                                                 Params,
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
      DeclScope->getContext());
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
      Diags.emitDiagMsg(BreakLoc.getLoc(), Diag::err_break_in_non_loop);
      return std::make_unique<ErrorStmt>(std::make_unique<BreakStmt>(BreakLoc.getLoc(), BreakLoc.getRightmostLoc()));
    }
    return std::make_unique<BreakStmt>(BreakLoc.getLoc(), BreakLoc.getRightmostLoc());
  }

  std::unique_ptr<Stmt> SemaAnalyzer::actOnNextStmt(Token &NextLoc) {
    if (!DeclScope->getClosestLoopScope()) {
      Diags.emitDiagMsg(NextLoc.getLoc(), Diag::err_next_in_non_loop);
      return std::make_unique<ErrorStmt>(std::make_unique<NextStmt>(NextLoc.getLoc(), NextLoc.getRightmostLoc()));
    }

    return std::make_unique<NextStmt>(NextLoc.getLoc(), NextLoc.getRightmostLoc());
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

    return RHSTy; // by elimination the LHS must be a literal and we want to promote the result to an actual type
  }

  void SemaAnalyzer::enterLoopScope() {
    enterScope(Scope::LoopScope);
  }
}
