#include "Sema/Sema.hpp"
#include "AST/AST.hpp"
#include "Basic/Basic.hpp"
#include "Basic/Diag.hpp"

bool funLang::SemaAnalyzer::actOnReturnStmt(ReturnStmt &RetExpr) {
  if (!currentFnRetType) { // poison
    return true;
  }

  auto *RetExprInput = RetExpr.getExprInput();
  auto *FnRetTypeDecl = currentFnRetType->getTypeDecl();
  if (!RetExprInput) {
    if (FnRetTypeDecl->isVoid()) {
      diags->emitDiagMsg(RetExpr.getLoc(),
                         diag::err_naked_return_on_non_void_fn);
      return false;
    } else {
      return true;
    }
  }
  if (!FnRetTypeDecl->areCompatible(RetExprInput->getType())) {
    diags->emitDiagMsg(RetExprInput->getLoc(), diag::err_incompatible_ret,
                       FnRetTypeDecl->getName(),
                       RetExprInput->getType()->getName());
    return false;
  }

  return true;
}

bool funLang::SemaAnalyzer::actOnUnaryOp(UnaryOp &unary) {
  auto unaryExprInputType = unary.getExprInput().getType();
  if (!unaryExprInputType) {
    return true;
  }
  bool Failed = false;
  switch (unary.getOp()) {
  case Basic::Op::UO_unaryMinus: {
    if (!unaryExprInputType->isNumericType()) {
      Failed = true;
      break;
    }
    unary.setType(unaryExprInputType);
  }
  case Basic::Op::UO_preInc:
  case Basic::Op::UO_preDec: {
    if (unaryExprInputType->isIntType()) {
      Failed = true;
      break;
    }
    unary.setType(unaryExprInputType);
  }
  case Basic::Op::UO_lNot: {
    if (!unaryExprInputType->isBoolType()) {
      Failed = true;
      break;
    }
    unary.setType(unaryExprInputType);
  };
  default:
    break;
  }

  if (Failed) {
    diags->emitDiagMsg(unary.getLoc(), diag::err_unary_op_incompatible,
                       Basic::Op::getUnaryOpSpelling(unary.getOp()),
                       unary.getType()->getName());

    unary.setType(nullptr);
  }
  return Failed;
}

bool funLang::SemaAnalyzer::actOnFnCall(FunctionCall &fnCall) {
  Decl *FunctionLookup = lookup(fnCall.getName());
  if (!FunctionLookup) {
    diags->emitDiagMsg(fnCall.getLoc(), diag::err_fn_not_found,
                       fnCall.getName());
    return false;
  }
  auto *FunctionCasted = llvm::dyn_cast<FunctionNode>(FunctionLookup);
  assert(FunctionCasted && "Function cast from Decl lookup did not work");

  if (FunctionCasted->getArgDecls().size() != fnCall.getArgs()->getSize()) {
    diags->emitDiagMsg(fnCall.getLoc(), diag::err_wrong_number_of_parameters,
                       FunctionCasted->getName(),
                       std::to_string(FunctionCasted->getArgDecls().size()),
                       std::to_string(fnCall.getArgs()->getSize()));
    return false;
  }

  bool Success = true;
  for (size_t i = 0; i < FunctionCasted->getArgDecls().size(); i++) {
    auto *FnCallArgType = fnCall.getArgs()->getArgsVec()[i]->getType();
    auto *FunctionArgDeclType =
        FunctionCasted->getArgDecls()[i]->getUnderlyingTypeDecl();
    if (FunctionArgDeclType->canImplicitlyPromoteOther(FnCallArgType)) {
      continue;
    } else if (!FunctionArgDeclType->eq(FnCallArgType)) {
      Success = false;
      diags->emitDiagMsg(
          FnCallArgType->getLoc(), diag::err_incompatible_type_passed,
          FunctionArgDeclType->getName(), FnCallArgType->getName());
    }
  }
  return Success;
}

bool funLang::SemaAnalyzer::actOnVarDeclStmt(VarDeclStmt &declStmt) {
  if (!currentScope->insert(declStmt.toDecl())) {
    diags->emitDiagMsg(declStmt.getLoc(), diag::err_var_redefinition,
                       declStmt.getName());
    Decl *look = lookup(declStmt.getName());
    diags->emitDiagMsg(look->getLoc(), diag::note_var_redefinition,
                       look->getName());
    return false;
  }
  if (!declStmt.getExpr()->getType()->areCompatible(
          declStmt.getTypeUse().getTypeDecl())) {
    diags->emitDiagMsg(declStmt.getExpr()->getLoc(),
                       diag::err_incompatible_type_var_decl, declStmt.getName(),
                       declStmt.getTypeUse().getTypeDecl()->getName(),
                       declStmt.getExpr()->getType()->getName());
    return false;
  }

  return true;
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
  assert(identifier.getTag() == Basic::tok::Tag::identifier &&
         "Trying to lookup a tag that isn't an identifier");
  if (!lookup(identifier.getLexeme())) {
    diags->emitDiagMsg(identifier.getLoc(), diag::err_var_not_found,
                       identifier.getLexeme());
    return false;
  }
  return true;
}

bool funLang::SemaAnalyzer::actOnFnDecl(FunctionNode &Fn) {
  if (Decl *Same = lookup(Fn.getName())) {
    diags->emitDiagMsg(Fn.getLoc(), diag::err_var_redefinition);
    diags->emitDiagMsg(Same->getLoc(), diag::note_var_redefinition,
                       Same->getName());
    return false;
  }

  return true;
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
  return std::move(currentFnRetType);
}

bool funLang::SemaAnalyzer::actOnTopLevelDecl(Decl &TopLDecl) {
  if (Decl *search = lookup(TopLDecl.getName())) {
    diags->emitDiagMsg(TopLDecl.getLoc(), diag::err_toplevel_redefinition,
                       search->getName());
    diags->emitDiagMsg(search->getLoc(), diag::note_var_redefinition,
                       search->getName());
    return false;
  }
  currentScope->insert(&TopLDecl);
  return true;
}

// check for the expr is done in actOnStructDecl
bool funLang::SemaAnalyzer::actOnStructVarDecl(VarDeclStmt &DeclStmt) {
  if (Decl *Found = lookupOneScope(DeclStmt.getName())) {
    diags->emitDiagMsg(DeclStmt.getLoc(), diag::err_struct_var_redefinition,
                       DeclStmt.getName());
    diags->emitDiagMsg(Found->getLoc(), diag::note_var_redefinition,
                       Found->getName());
    return false;
  }
  return true;
}

void funLang::SemaAnalyzer::init() {
  auto boolType = new TypeDecl("bool", Basic::Data::Type::bool_);
  auto i32Type = new TypeDecl("i32", Basic::Data::Type::i32);
  auto i64Type = new TypeDecl("i64", Basic::Data::Type::i64);
  auto f32Type = new TypeDecl("f32", Basic::Data::f32);
  auto f64Type = new TypeDecl("f64", Basic::Data::f64);
  auto stringType = new TypeDecl("string", Basic::Data::string);
  auto voidType = new TypeDecl("void", Basic::Data::void_);

  auto IntLiteral = new TypeDecl("integer literal", Basic::Data::int_literal);
  auto FloatLiteral =
      new TypeDecl("floating literal", Basic::Data::floating_literal);

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
      diags->emitDiagMsg(Val->getLoc(), diag::err_struct_var_initialization,
                         Val->getName(), StructDecl.getName());
      Success = false;
    }
  }
  return Success;
}
bool funLang::SemaAnalyzer::actOnMultDivOp(BinaryOp &MultiplyOrDivide) {}

bool funLang::SemaAnalyzer::actOnAddSubOp(BinaryOp &AddOrSubtract) {
  return false;
}

bool funLang::SemaAnalyzer::actOnComparisonOp(BinaryOp &CmpOp) {
  if (!CmpOp.getLhs().getType()->eq(CmpOp.getRhs().getType())) {
    diags->emitDiagMsgRange(CmpOp.getLhs().getLoc(), CmpOp.getRhs().getLoc(),
                            diag::err_incompatible_binary_operands,
                            CmpOp.getLhs().getType()->getName(),
                            CmpOp.getRhs().getType()->getName(),
                            Basic::Op::getBinaryOpSpelling(CmpOp.getOp()));
    CmpOp.setType(nullptr);
    return false;
  }
  CmpOp.setType(
      lookupType(Basic::Data::getBasicTypeSpelling(Basic::Data::bool_)));
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

TypeDecl *funLang::SemaAnalyzer::lookupBaseType(
    Basic::Data::Type Type) { // wrapper function to type less
  return lookupType(Basic::Data::getBasicTypeSpelling(Type));
}

TypeDecl *funLang::SemaAnalyzer::isEqualToBaseType(
    std::initializer_list<Basic::Data::Type> Types, TypeDecl *Type) {
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
