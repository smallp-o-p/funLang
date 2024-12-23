//
// Created by will on 7/17/24.
//
module;
#include "llvm/Support/Casting.h"
module AST;
import Basic;

namespace funLang {
  bool Type::isIntType() const {
    if (const auto *Ty = llvm::dyn_cast<BuiltInType>(OriginalType)) {
      return Ty->getKind() == BuiltInType::i32 || Ty->getKind() == BuiltInType::i64 || Ty->getKind() ==
          BuiltInType::int_literal;
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
      return Ty->getKind() == BuiltInType::f32 || Ty->getKind() == BuiltInType::f64 || Ty->getKind() ==
          BuiltInType::float_literal;
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

  bool Type::isPointer() {
    return llvm::isa<PointerType>(this);
  }

  bool Type::isBuiltIn() { return llvm::isa<BuiltInType>(this); }

  Decl *RecordType::lookup(const IDTableEntry *MemberName) const { return Record->getFirstDeclName(MemberName); }
}
