//
// Created by will on 10/21/24.
//
module;
#include <cassert>
#include <llvm/ADT/FoldingSet.h>
export module AST:Type;
import Basic;

namespace funLang {
  class Decl;
  class RecordDecl;
  class VarDecl;

  export class Type {
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
    Type(Type *OriginalType, const TypeClass Kind) : Class(Kind), OriginalType(OriginalType) {
    }

  public:
    [[nodiscard]] TypeClass getClass() const { return Class; }
    bool isBuiltIn();
    [[nodiscard]] bool isIntType() const;
    [[nodiscard]] bool isVoidType() const;
    [[nodiscard]] bool isI32() const;
    [[nodiscard]] bool isI64() const;
    [[nodiscard]] bool isF32() const;
    [[nodiscard]] bool isF64() const;
    [[nodiscard]] bool isFloatType() const;
    [[nodiscard]] bool isBoolType() const;
    bool eqTo(const Type *Other) const { return this == Other; }
    bool isIntLiteral();
    bool isFloatLiteral();
  };

  export class BuiltInType : public Type {
    friend class Type;

  public:
    enum BTKind : size_t {
#define DATA(ID, SP) ID,
#include "Basic/defs/BuiltInTypes.def"
      NUM_DATA_TYPES
    };
    explicit BuiltInType(const BTKind BuiltIn) : Type(this, TK_BUILTIN), BuiltInKind(BuiltIn) {
    }
    static bool classof(const Type *T) { return T->getClass() == TK_BUILTIN; }
    [[nodiscard]] BTKind getKind() const { return BuiltInKind; }
    static BTKind mapTokenToType(const Basic::tok::Tag Tag) {
      using namespace Basic;
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
      default: ;
      }
      llvm_unreachable("can't get here");
    }

  private:
    BTKind BuiltInKind;
  };

  export class PointerType : public Type, llvm::FoldingSetNodeID {
    Type *PointeeType;

  public:
    explicit PointerType(Type *PointeeType) : Type(this, TK_POINTER), PointeeType(PointeeType) {
    }
    [[nodiscard]] Type *getPointeeType() const { return PointeeType; }

    static bool classof(const Type *T) { return T->getClass() == TK_POINTER; }
  };

  class RecordType : public Type {
    friend class Type;

  protected:
    RecordDecl *Record;

  public:
    explicit RecordType(RecordDecl *Record) : Type(this, TK_RECORD), Record(Record) {
    }
    Decl *lookup(const IDTableEntry *MemberName) const;

    static bool classof(const Type *T) { return T->getClass() == TK_RECORD; }
  };

  class EnumType : public Type {
    friend class Type;

  public:
    static bool classof(const Type *T) { return T->getClass() == TK_ENUM; }
  };
}
