//
// Created by will on 7/17/24.
//
module;
#include "llvm/Support/Casting.h"
module funLangAST;
import :Decl;
import :Stmt;

namespace funLang {
  bool Type::isIntType() const {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
      return Ty->getKind() == BuiltInType::i32 || Ty->getKind() == BuiltInType::i64;
    }
    return false;
  }

  bool Type::isVoidType() const {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
      return Ty->getKind() == BuiltInType::void_;
    }
    return false;
  }

  bool Type::isFloatType() const {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
      return Ty->getKind() == BuiltInType::f32 || Ty->getKind() == BuiltInType::f64;
    }
    return false;
  }

  bool Type::isBoolType() const {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
      return Ty->getKind() == BuiltInType::bool_;
    }
    return false;
  }

  bool Type::isI32() const {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
      return Ty->getKind() == BuiltInType::i32;
    }
    return false;
  }

  bool Type::isI64() const {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
      return Ty->getKind() == BuiltInType::i64;
    }
    return false;
  }

  bool Type::isF64() const {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
      return Ty->getKind() == BuiltInType::f64;
    }
    return false;
  }

  bool Type::isF32() const {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
      return Ty->getKind() == BuiltInType::f32;
    }
    return false;
  }

  bool Type::LHSTyCompatibleRHSTy(Type *Other) const {
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
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(this)) {
      return Ty->getKind() == BuiltInType::int_literal;
    }
    return false;
  }

  bool Type::isFloatLiteral() {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(this)) {
      return Ty->getKind() == BuiltInType::float_literal;
    }
    return false;
  }
  bool Type::isBuiltIn() { return llvm::isa<BuiltInType>(this); }

  Decl *RecordType::lookup(const IDTableEntry *MemberName) const { return Record->getFirstDeclName(MemberName); }
}
