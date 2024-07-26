#pragma once
#include "Basic/Basic.hpp"
#include "Lex/Lex.hpp"
#include "Type.hpp"
#include "Stmt.hpp"
#include "llvm/ADT/StringMap.h"
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>
#include "llvm/ADT/DenseMap.h"
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

namespace funLang {
class Type;

class TypeProperties;
class CompilationUnit;
class FunctionDecl;
class Decl;
class DeclContext;
class RecordDecl;
class VarDecl;
class SemaAnalyzer;

class CompoundStmt;
class Expr;

typedef llvm::StringMapEntry<std::nullopt_t> IDTableEntry;

class Decl {
public:
  enum DeclKind {
	DK_FN,
	DK_VAR,
	DK_TYPE,
	DK_TYPEPTR,
	DK_TRAIT,
	DK_PARAM,
  };

private:
  DeclKind Kind;
  IDTableEntry *DeclName;
  llvm::SMLoc Start, End;

public:
  explicit Decl(DeclKind Kind, llvm::StringMapEntry<std::nullopt_t> *IDTableEntry, llvm::SMLoc Start, llvm::SMLoc End)
	  : Kind(Kind), DeclName(IDTableEntry), Start(Start), End(End) {}
  DeclKind getKind() const { return Kind; }
  IDTableEntry *getEntry() { return DeclName; }
  llvm::StringRef getName() { return DeclName->first(); }
  const llvm::SMLoc getStart() const { return Start; }
  const llvm::SMLoc getEnd() const { return End; }
};

class DeclContext { // all declarations are thrown into a DeclContext and will be in the same level no matter how they're scoped lexically
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

  Decl *lookup(llvm::StringMapEntry<std::nullopt_t> *Name);

  void setParentContext(DeclContext *Parent) { ParentContext = Parent; }

  bool insert(llvm::StringMapEntry<std::nullopt_t> *Name, std::unique_ptr<Decl> Dec) {
	auto Pair = Decls.insert({Name, std::move(Dec)});
	return Pair.second;
  }
};

class CompilationUnit : public DeclContext {
public:
  explicit CompilationUnit(llvm::DenseMap<IDTableEntry *, std::unique_ptr<Decl>> Globals) : DeclContext(std::move(
	  Globals)) {}

};

class FunctionDecl : public Decl, DeclContext {
private:
  std::unique_ptr<TypeUse> ReturnType;
  std::unique_ptr<CompoundStmt> Compound;
  llvm::SmallVector<Decl *> Params;

public:
  FunctionDecl(std::unique_ptr<TypeUse> retType,
			   llvm::StringMapEntry<std::nullopt_t> *Name,
			   std::unique_ptr<CompoundStmt> compound,
			   llvm::SMLoc TypeLoc,
			   llvm::SMLoc RLoc,
			   llvm::SmallVector<Decl *> Params,
			   DeclContext *Parent)
	  : ReturnType(std::move(retType)),
		Compound(std::move(compound)), Params(std::move(Params)), Decl(DK_FN, Name, TypeLoc, RLoc),
		DeclContext(Parent) {}

  Type *getTypePtr();
  llvm::SmallVector<Decl *> &getParams() { return Params; }
  CompoundStmt &getCompound() const { return *Compound; }
  static bool classof(const Decl *D) { return D->getKind() == DK_FN; }
};

class TraitDecl : public DeclContext {
public:
  explicit TraitDecl(DeclContext *Parent) : DeclContext(Parent) {}
};

class RecordDecl : public Decl, DeclContext {
private:
  Type *TypePtr;

public:
  RecordDecl(llvm::StringMapEntry<std::nullopt_t> *name, Type *TypePtr,
			 llvm::SMLoc TypeLoc, llvm::SMLoc RBraceLoc, DeclContext *Parent)
	  : Decl(DK_TYPE, name, TypeLoc, RBraceLoc), DeclContext(Parent), TypePtr(TypePtr) {}
  static bool classof(const Decl *D) {
	return D->getKind() >= DK_TYPE && D->getKind() <= DK_TYPEPTR;
  }
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
public:
  ParamDecl(IDTableEntry *Name, std::unique_ptr<Expr> Init, DeclContext *Container, llvm::SMLoc Left, llvm::SMLoc Right)
	  : Decl(DK_PARAM,
			 Name,
			 Left,
			 Right),
		Init(std::move(Init)), Container(Container) {}
};

}