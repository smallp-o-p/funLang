#pragma once

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

import Basic;
import Basic.IdentifierTable;
namespace funLang {
class Type;
class TypeUse;

class CompilationUnit;
class FunctionDecl;
class Decl;
class DeclContext;
class RecordDecl;
class VarDecl;
class SemaAnalyzer;
class ParamDecl;

class Stmt;
class CompoundStmt;
class Expr;

class Decl {
public:
  enum DeclKind {
    DK_FN,
    DK_VAR,
    DK_TYPE,
    DK_TRAIT,
    DK_PARAM,
  };

private:
  DeclKind Kind;
  IDTableEntry *DeclName;
  llvm::SMLoc Start, End;

public:
  explicit Decl(DeclKind Kind, IDTableEntry *Entry, llvm::SMLoc Start, llvm::SMLoc End)
      : Kind(Kind), DeclName(Entry), Start(Start), End(End) {}
  DeclKind getKind() const { return Kind; }
  IDTableEntry *getEntry() { return DeclName; }
  llvm::StringRef getName() { return DeclName->first(); }
  const llvm::SMLoc getStart() const { return Start; }
  const llvm::SMLoc getEnd() const { return End; }
  void setStart(llvm::SMLoc LocStart) { Start = LocStart; }
  void setEnd(llvm::SMLoc SEnd) { End = SEnd; }
  void setRange(llvm::SMLoc LocStart, llvm::SMLoc LocEnd) {
    Start = LocStart;
    End = LocEnd;
  }
};

class DeclContext {// all declarations are thrown into a DeclContext and will be in the same level no matter how they're scoped lexically
  // this implementation will not allow us to have any shadowing
private:
  DeclContext *ParentContext;
  llvm::DenseMap<IDTableEntry *, std::unique_ptr<Decl>> Decls;

public:
  explicit DeclContext(DeclContext *ParentContext)
      : ParentContext(ParentContext),
        Decls(llvm::DenseMap<IDTableEntry *, std::unique_ptr<Decl>>()) {}
  explicit DeclContext(llvm::DenseMap<IDTableEntry *, std::unique_ptr<Decl>> Decls)
      : ParentContext(nullptr), Decls(std::move(Decls)) {}

  void setParentContext(DeclContext *Parent) { ParentContext = Parent; }

  bool insert(IDTableEntry *Name, std::unique_ptr<Decl> Dec) {
    auto Pair = Decls.insert({Name, std::move(Dec)});
    return Pair.second;
  }

  Decl *getDecl(IDTableEntry *Name) {
    auto Found = Decls.find(Name);
    if (Found != Decls.end()) {
      return nullptr;
    }
    return Found->second.get();
  }
};

class CompilationUnit : public DeclContext {
public:
  explicit CompilationUnit(llvm::DenseMap<IDTableEntry *, std::unique_ptr<Decl>> Globals) : DeclContext(std::move(
                                                                                                Globals)) {}
};

class FunctionDecl : public Decl, public DeclContext {
private:
  std::unique_ptr<TypeUse> ReturnType;
  std::unique_ptr<CompoundStmt> Stmts;
  u_ptr<llvm::SmallVector<u_ptr<ParamDecl>>> Params;

public:
  FunctionDecl(std::unique_ptr<TypeUse> retType,
               llvm::StringMapEntry<std::nullopt_t> *Name,
               std::unique_ptr<CompoundStmt> Stmts,
               llvm::SMLoc TypeLoc,
               llvm::SMLoc RLoc,
               std::unique_ptr<llvm::SmallVector<std::unique_ptr<ParamDecl>>> Params,
               DeclContext *Parent);

  FunctionDecl(std::unique_ptr<TypeUse> retType,
               IDTableEntry *Name,
               llvm::SMLoc TypeLoc,
               llvm::SMLoc RLoc,
               std::unique_ptr<llvm::SmallVector<std::unique_ptr<ParamDecl>>> Parms,
               DeclContext *Ctx);

  Type *getTypePtr();
  llvm::SmallVector<std::unique_ptr<ParamDecl>> &getParams() { return *Params; }
  void setCompound(std::unique_ptr<CompoundStmt> Compound) { Stmts = std::move(Compound); };
  CompoundStmt &getCompound() const { return *Stmts; }
  static bool classof(const Decl *D) { return D->getKind() == DK_FN; }
};

class TraitDecl : public DeclContext {
public:
  explicit TraitDecl(DeclContext *Parent) : DeclContext(Parent) {}
};

class RecordDecl : public Decl, public DeclContext {
private:
  Type *TypePtr;
  Decl *implDecl = nullptr;

public:
  RecordDecl(IDTableEntry *name, Type *TypePtr, DeclContext *Parent)
      : Decl(DK_TYPE, name, llvm::SMLoc(), llvm::SMLoc()), DeclContext(Parent), TypePtr(TypePtr) {}
  static bool classof(const Decl *D) { return D->getKind() == DK_TYPE; }
  void setTypePtr(Type *T) { TypePtr = T; }
  Type *getTypePtr() { return TypePtr; }
};

class TypeUse {
private:
  llvm::SMLoc FirstQualifierLoc;
  llvm::SMLoc TypeNameLoc;
  Type *TypePtr;

public:
  TypeUse(funLang::Type *type, llvm::SMLoc QualifierLoc, llvm::SMLoc TypeNameLoc)
      : TypePtr(type), FirstQualifierLoc(QualifierLoc), TypeNameLoc(TypeNameLoc) {}
  funLang::Type *getTypePtr() { return TypePtr; }
  const llvm::SMLoc &getFirstQualifierLoc() const { return FirstQualifierLoc; }
  const llvm::SMLoc &getTypeNameLoc() const { return TypeNameLoc; }
};

class VarDecl : public Decl {
private:
  std::unique_ptr<TypeUse> UsedType;
  // qualifiers and stuff would go here?
public:
  VarDecl(std::unique_ptr<TypeUse> Type, IDTableEntry *Name, llvm::SMLoc Left,
          llvm::SMLoc Right)
      : Decl(DK_VAR, Name, Left, Right), UsedType(std::move(Type)) {}
  Type *getTypePtr() const { return UsedType->getTypePtr(); }
  static bool classof(const Decl *D) { return D->getKind() == DK_VAR; }
};

class ParamDecl : public Decl {
private:
  std::unique_ptr<Expr> Init;
  DeclContext *Container;
  u_ptr<TypeUse> T;

public:
  ParamDecl(IDTableEntry *Name,
            std::unique_ptr<Expr> Init,
            DeclContext *Container,
            llvm::SMLoc Left,
            llvm::SMLoc Right,
            u_ptr<TypeUse> T);

  Type *getTypePtr() { return T->getTypePtr(); }
  static bool classof(const Decl *D) { return D->getKind() == DK_PARAM; }
};

}// namespace funLang