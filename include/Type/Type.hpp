//
// Created by will on 7/17/24.
//

#ifndef FUNLANG_INCLUDE_TYPE_TYPE_HPP
#define FUNLANG_INCLUDE_TYPE_TYPE_HPP
#include "Basic/Basic.hpp"
#include "Sema/Sema.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/FoldingSet.h"

namespace funLang {
class TypeTestFixture;
class Type;
class BuiltInType;
class TagType;
class RecordType;
class EnumType;
class PointerType;
class ArrayType;

class Type {
  friend class funLang::SemaAnalyzer;
  friend class TypeTestFixture;
protected:
  enum TypeClass {
	TK_BUILTIN,
	TK_POINTER,
	TK_RECORD,
	TK_ARRAY,
	TK_ENUM,
  };
  TypeClass Class;
  Type *OriginalType;

protected:
  Type(Type *OriginalType, TypeClass Kind) : OriginalType(OriginalType), Class(Kind) {}
public:
  TypeClass getClass() const { return Class; }

  bool isIntType();
  bool isVoidType();
  bool isIntLiteral();
  bool isFloatLiteral();
  bool isI32();
  bool isI64();
  bool isF32();
  bool isF64();
  bool isFloatType();
  bool isBoolType();
  bool ArithmeticCompatibleWithEachOther(Type *Other);
  bool eqTo(Type *Other) { return this == Other; }
};

class BuiltInType : public Type {
  friend class Type;
  friend class TypeTestFixture;
protected:
  enum BTKind {
	#define DATA(ID, SP) ID,
	#include "Basic/defs/BuiltInTypes.def"
	NUM_DATA_TYPES
  };
private:
  BTKind BuiltInKind;

public:
  explicit BuiltInType(BTKind BuiltIn) : BuiltInKind(BuiltIn), Type(this, TK_BUILTIN) {}
  static bool classof(const Type *T) { return T->getClass() == TK_BUILTIN; }
  BTKind getKind() { return BuiltInKind; }
};
class PointerType : public Type {
private:
  Type *PointeeType;
public:
  PointerType(Type *PointeeType) : PointeeType(PointeeType), Type(this, TK_POINTER) {}
  Type *getPointeeType() { return PointeeType; }
};
}

#endif //FUNLANG_INCLUDE_TYPE_TYPE_HPP
