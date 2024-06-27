#include "Sema/Sema.hpp"
#include "AST/AST.hpp"
#include "Basic/Basic.hpp"
#include "Basic/Diag.hpp"
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/SMLoc.h>
#include <memory>

std::unique_ptr<ReturnStmt>
funLang::SemaAnalyzer::actOnReturnStmt(llvm::SMLoc ReturnLoc,
                                       std::unique_ptr<Expr> ReturnExpr) {
  if (!currentFnRetType) { // poison
    return std::make_unique<ReturnStmt>(std::move(ReturnExpr));
  }
  if (!ReturnExpr) {
    BuiltInType *ShouldBeVoid =
        llvm::dyn_cast<BuiltInType>(currentFnRetType->getTypeDecl());
    if (!ShouldBeVoid) {
      diags->emitDiagMsg(ReturnLoc, diag::err_naked_return_on_non_void_fn);
    } else {
      if (!ShouldBeVoid->isVoid()) {
        diags->emitDiagMsg(ReturnLoc, diag::err_return_on_void_fn);
      }
    }
    return std::make_unique<ReturnStmt>(std::move(ReturnExpr));
  }

  if (llvm::isa<PointerType>(currentFnRetType->getTypeDecl())) {
    PointerType *Ptr =
        llvm::dyn_cast<PointerType>(currentFnRetType->getTypeDecl());
    if (!llvm::isa<PointerType>(ReturnExpr->getType())) {
      diags->emitDiagMsg(ReturnExpr->getLoc(), diag::err_incompatible_ret,
                         Ptr->getFullName(), ReturnExpr->getType()->getName());
    } else {
      PointerType *ExprPtr = llvm::dyn_cast<PointerType>(ReturnExpr->getType());

      if (!Ptr->getPointee()->eq(ExprPtr->getPointee())) {
        diags->emitDiagMsg(ReturnExpr->getLoc(), diag::err_incompatible_ret,
                           Ptr->getFullName(),
                           ReturnExpr->getType()->getName());
      }
    }

  } else {
    if (!currentFnRetType->getTypeDecl()->eq(ReturnExpr->getType())) {
      diags->emitDiagMsg(ReturnExpr->getLoc(), diag::err_incompatible_ret,
                         currentFnRetType->getTypeDecl()->getName(),
                         ReturnExpr->getType()->getName());
    }
  }
  return std::make_unique<ReturnStmt>(std::move(ReturnExpr));
}

std::unique_ptr<UnaryOp>
funLang::SemaAnalyzer::actOnUnaryOp(Basic::Op::Unary Op,
                                    std::unique_ptr<Expr> ExprInput) {
  if (!llvm::isa<BuiltInType *>(ExprInput->getType())) {
    diags->emitDiagMsg(ExprInput->getLoc(), diag::err_unary_op_incompatible,
                       Basic::Op::getUnaryOpSpelling(Op),
                       ExprInput->getType()->getName());
    return std::make_unique<UnaryOp>(std::move(ExprInput), Op, nullptr);
  }
  bool Mismatch = false;
  TypeDecl *resultType = nullptr;
  BuiltInType *BuiltIn = llvm::dyn_cast<BuiltInType>(ExprInput->getType());
  switch (Op) {
  case Basic::Op::UO_unaryMinus: {
    if (!BuiltIn->isNumericType()) {
      Mismatch = true;
    } else {
      resultType = ExprInput->getType();
    }
    break;
  }
  case Basic::Op::UO_preInc:
  case Basic::Op::UO_preDec: {
    if (!BuiltIn->isIntType()) {
      Mismatch = true;
    } else {
      resultType = ExprInput->getType();
    }
    break;
  }
  case Basic::Op::UO_lNot: {
    if (!BuiltIn->isBoolType()) {
      Mismatch = true;
    } else {
      resultType = ExprInput->getType();
    }
    break;
  }
  case Basic::Op::NUM_UNARY:
    llvm_unreachable("Invalid unary operator");
    break;
  }
  if (Mismatch) {
    diags->emitDiagMsg(ExprInput->getLoc(), diag::err_unary_op_incompatible,
                       Basic::Op::getUnaryOpSpelling(Op),
                       ExprInput->getType()->getName());
  }

  return std::make_unique<UnaryOp>(std::move(ExprInput), Op, resultType);
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

std::unique_ptr<VarDeclStmt>
funLang::SemaAnalyzer::actOnVarDeclStmt(std::unique_ptr<TypeUse> Type,
                                        Token IDTok,
                                        std::unique_ptr<Expr> ExprInput) {
  if (Decl *Lookup = lookup(IDTok.getIdentifier())) {
    diags->emitDiagMsg(IDTok.getLoc(), diag::err_var_redefinition,
                       IDTok.getIdentifier());
    diags->emitDiagMsg(Lookup->getLoc(), diag::note_var_redefinition,
                       Lookup->getName());
    return std::make_unique<VarDeclStmt>(std::move(Type), IDTok.getIdentifier(),
                                         std::move(ExprInput), IDTok.getLoc());
  }

  if (llvm::isa<BuiltInType>(Type->getTypeDecl()) &&
      llvm::isa<BuiltInType>(ExprInput->getType())) {
    BuiltInType *DeclType = llvm::dyn_cast<BuiltInType>(Type->getTypeDecl());
    BuiltInType *ExprType = llvm::dyn_cast<BuiltInType>(ExprInput->getType());

    if (!DeclType->areCompatible(ExprType)) {
      diags->emitDiagMsg(
          ExprType->getLoc(), diag::err_incompatible_type_var_decl,
          IDTok.getIdentifier(), DeclType->getName(), ExprType->getName());
    }

    // TODO: Add an Expr class to represent well-formed, but semantically
    // incorrect expressions, like the RecoveryExpr AST node in Clang.
  } else if (!Type->getTypeDecl()->eq(ExprInput->getType())) {
    diags->emitDiagMsg(ExprInput->getLoc(),
                       diag::err_incompatible_type_var_decl,
                       IDTok.getIdentifier(), Type->getTypeDecl()->getName(),
                       ExprInput->getType()->getName());
  }

  auto ValidDeclStmt =
      std::make_unique<VarDeclStmt>(std::move(Type), IDTok.getIdentifier(),
                                    std::move(ExprInput), IDTok.getLoc());
  currentScope->insert(ValidDeclStmt->toDecl());

  return ValidDeclStmt;
}

void funLang::SemaAnalyzer::exitScope() {
  assert(currentScope->getParent() && "Attempted to exit global scope.\n");
  currentScope = currentScope->getParent();
}

void funLang::SemaAnalyzer::enterScope() {
  std::shared_ptr<Scope> innerScope = std::make_shared<Scope>(currentScope);
  currentScope = innerScope;
}

std::unique_ptr<NameUsage>
funLang::SemaAnalyzer::actOnNameUsage(Token &Identifier) {
  assert(Identifier.getTag() == Basic::tok::Tag::identifier &&
         "Trying to lookup a tag that isn't an identifier");
  Decl *Found = lookup(Identifier.getLexeme());
  if (!Found) {
    diags->emitDiagMsg(Identifier.getLoc(), diag::err_var_not_found,
                       Identifier.getLexeme());
    return std::make_unique<NameUsage>(Identifier.getIdentifier(),
                                       Identifier.getLoc());
  }
  VarDecl *Variable = llvm::dyn_cast<VarDecl>(Found);
  if (!Variable) {
    diags->emitDiagMsg(Identifier.getLoc(), diag::err_var_not_found,
                       Identifier.getLexeme());
    return nullptr;
  }
  return std::make_unique<NameUsage>(Identifier.getIdentifier(),
                                     Identifier.getLoc(),
                                     Variable->getUnderlyingTypeDecl());
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
  auto BoolTy = new BuiltInType(Basic::Data::Type::bool_, "bool");
  auto I32Ty = new BuiltInType(Basic::Data::Type::i32, "i32");
  auto I64Ty = new BuiltInType(Basic::Data::Type::i64, "i64");
  auto F32Ty = new BuiltInType(Basic::Data::f32, "f32");
  auto F64Ty = new BuiltInType(Basic::Data::f64, "f64");
  auto StringTy = new BuiltInType(Basic::Data::string, "string");
  auto VoidTy = new BuiltInType(Basic::Data::void_, "void");

  auto IntLiteral =
      new BuiltInType(Basic::Data::int_literal, "integer literal");
  auto FloatLiteral =
      new BuiltInType(Basic::Data::floating_literal, "floating point literal");

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

bool funLang::SemaAnalyzer::actOnComparisonOp(BinaryOp &CmpOp) {}

TypeDecl *funLang::SemaAnalyzer::lookupType(llvm::StringRef Type) {
  if (Decl *Found = lookup(Type)) {
    if (Found->getKind() != Decl::DK_TYPE) {
      return nullptr;
    }
    return llvm::dyn_cast<TypeDecl>(Found);
  }
  return nullptr;
}

Decl *funLang::SemaAnalyzer::lookupOneScope(llvm::StringRef varName) {
  return currentScope->find(varName);
}
