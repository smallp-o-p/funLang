//
// Created by will on 7/17/24.
//
#pragma once
#include "Basic/Basic.hpp"
#include "Sema/Sema.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/FoldingSet.h"

namespace funLang {
class Decl;
class RecordDecl;

class TypeTestFixture;
class Type;
class BuiltInType;
class TagType;
class RecordType;
class EnumType;
class PointerType;
class ArrayType;

class Type {
  friend class SemaAnalyzer;
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
  bool isI32();
  bool isI64();
  bool isF32();
  bool isF64();
  bool isFloatType();
  bool isBoolType();
  bool ArithmeticCompatibleWithEachOther(Type *Other);
  bool eqTo(Type *Other) { return this == Other; }
  llvm::StringRef getName() { return "placeholder name for types"; }
};

class BuiltInType : public Type {
  friend class Type;
  friend class SemaAnalyzer;
  friend class TypeTestFixture;
protected:
  enum BTKind {
	#define DATA(ID, SP) ID,
	#include "../Basic/defs/BuiltInTypes.def"
	NUM_DATA_TYPES
  };
private:
  BTKind BuiltInKind;

public:
  explicit BuiltInType(BTKind BuiltIn) : BuiltInKind(BuiltIn), Type(this, TK_BUILTIN) {}
  static bool classof(const Type *T) { return T->getClass() == TK_BUILTIN; }
  BTKind getKind() { return BuiltInKind; }
};
class PointerType : public Type, llvm::FoldingSetNodeID {
private:
  Type *PointeeType;
public:
  explicit PointerType(Type *PointeeType) : PointeeType(PointeeType), Type(this, TK_POINTER) {}
  Type *getPointeeType() { return PointeeType; }

  static bool classof(const Type *T) { return T->getClass() == TK_POINTER; }
};

class RecordType : public Type {
  friend class Type;
protected:
  RecordDecl *Record;
public:
  explicit RecordType(RecordDecl *Record) : Record(Record), Type(this, TK_RECORD) {}
  Decl *lookup(llvm::StringRef MemberName);

  static bool classof(const Type *T) { return T->getClass() == TK_RECORD; }
};

class EnumType : public Type {
  friend class Type;

public:
  static bool classof(const Type *T) { return T->getClass() == TK_ENUM; }

};
}

