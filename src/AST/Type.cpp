//
// Created by will on 7/17/24.
//
#include "AST/Type.hpp"
#include "AST/Decl.hpp"
#include "AST/Stmt.hpp"
#include "llvm/Support/Casting.h"
using namespace funLang;
bool funLang::Type::isIntType() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::i32 || Ty->getKind() == BuiltInType::i64;
  }
  return false;
}

bool funLang::Type::isVoidType() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::void_;
  }
  return false;
}

bool funLang::Type::isFloatType() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::f32 || Ty->getKind() == BuiltInType::f64;
  }
  return false;
}

bool funLang::Type::isBoolType() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::bool_;
  }
  return false;
}

bool Type::isI32() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::i32;
  }
  return false;
}

bool Type::isI64() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::i64;
  }
  return false;
}

bool Type::isF64() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::f64;
  }
  return false;
}

bool Type::isF32() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::f32;
  }
  return false;
}

bool funLang::Type::LHSTyCompatibleRHSTy(Type *Other) {
  auto *LHS = llvm::dyn_cast<BuiltInType>(OriginalType);
  if (!LHS) {
	return false;
  }
  auto *RHS = llvm::dyn_cast<BuiltInType>(Other);
  if (!RHS) {
	return false;
  }
  if (LHS->isIntType() || LHS->isIntLiteral()) {
	return LHS->eqTo(RHS) || RHS->isIntLiteral();
  } else if (LHS->isFloatType() || LHS->isFloatLiteral()) {
	return LHS->eqTo(RHS) || RHS->isFloatLiteral();
  }
  return false;
}

bool Type::isIntLiteral() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(this)) {
	return Ty->getKind() == BuiltInType::int_literal;
  }
  return false;
}

bool Type::isFloatLiteral() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(this)) {
	return Ty->getKind() == BuiltInType::float_literal;
  }
  return false;
}
bool Type::isBuiltIn() { return llvm::isa<BuiltInType>(this); }

Decl *RecordType::lookup(IDTableEntry *MemberName) { return Record->getDecl(MemberName); }
