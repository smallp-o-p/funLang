//
// Created by will on 7/17/24.
//
#include "Type/Type.hpp"
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

bool funLang::Type::isIntLiteral() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::int_literal;
  }
  return false;
}

bool funLang::Type::isFloatLiteral() {
  if (auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
	return Ty->getKind() == BuiltInType::floating_literal;
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

bool funLang::Type::ArithmeticCompatibleWithEachOther(Type *Other) {
  auto *LHS = llvm::dyn_cast<BuiltInType>(OriginalType);
  if (!LHS) {
	return false;
  }
  auto *RHS = llvm::dyn_cast<BuiltInType>(Other);
  if (!RHS) {
	return false;
  }
  switch (LHS->getKind()) {
  case BuiltInType::i32: return Other->isI32() || Other->isIntLiteral();
  case BuiltInType::i64: return Other->isI64() || Other->isIntLiteral();
  case BuiltInType::int_literal: return Other->isIntLiteral() || Other->isIntType();
  case BuiltInType::f32: return Other->isF32() || Other->isFloatLiteral();
  case BuiltInType::f64: return Other->isF64() || Other->isFloatLiteral();
  case BuiltInType::floating_literal: return Other->isFloatLiteral();
  default: return false;
  }
}

