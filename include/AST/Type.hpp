#pragma once
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/StringRef.h"
import Basic;
import Basic.IdentifierTable;

namespace funLang {
  using namespace Basic;
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
    [[nodiscard]] TypeClass getClass() const { return Class; }
    bool isBuiltIn();
    bool isIntType();
    bool isVoidType();
    bool isI32();
    bool isI64();
    bool isF32();
    bool isF64();
    bool isFloatType();
    bool isBoolType();
    bool LHSTyCompatibleRHSTy(Type *Other);
    bool eqTo(Type *Other) { return this == Other; }
    bool isIntLiteral();
    bool isFloatLiteral();
    llvm::StringRef getName() { return "placeholder name for types"; }
  };

  class BuiltInType : public Type {
    friend class Type;
    friend class SemaAnalyzer;
    friend class TypeTestFixture;

  protected:
    enum BTKind : size_t {
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
    static BTKind mapTokenToType(Basic::tok::Tag Tag) {
      assert(Tag >= tok::kw_i32 && Tag < Basic::tok::kw_last_type);
      switch (Tag) {
      case tok::kw_i32: return i32;
      case tok::kw_i64: return i64;
      case tok::kw_f32: return f32;
      case tok::kw_f64: return f64;
      case tok::kw_string: return string;
      case tok::kw_char: return char_;
      case tok::kw_void: return void_;
      case tok::kw_bool: return bool_;
      default:;
      }
      llvm_unreachable("can't get here");
    }
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
    Decl *lookup(IDTableEntry *MemberName);

    static bool classof(const Type *T) { return T->getClass() == TK_RECORD; }
  };

  class EnumType : public Type {
    friend class Type;

  public:
    static bool classof(const Type *T) { return T->getClass() == TK_ENUM; }
  };
}// namespace funLang
