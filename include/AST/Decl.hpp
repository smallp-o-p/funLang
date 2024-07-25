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
class TopLevelDecls;
class FunctionDecl;
class Decl;
class DeclContext;
class RecordDecl;
class VarDecl;
class SemaAnalyzer;

class CompoundStmt;
class Expr;

class Decl {
public:
  enum DeclKind {
	DK_FN,
	DK_VAR,
	DK_TYPE,
	DK_TYPEBUILTIN,
	DK_TYPEARRAY,
	DK_TYPEPTR,
	DK_ARG,
	DK_TRAIT,
  };

private:
  DeclKind Kind;
  llvm::StringRef DeclName;

public:
  explicit Decl(DeclKind kind, llvm::StringRef name)
	  : Kind(kind), DeclName(name) {}
  Decl(DeclKind kind, llvm::StringRef name, llvm::SMLoc Left, llvm::SMLoc Right)
	  : Kind(kind), DeclName(name) {}
  Decl(DeclKind kind, llvm::StringRef name, llvm::SMLoc Left)
	  : Kind(kind), DeclName(name) {}
  DeclKind getKind() const { return Kind; }
  llvm::StringRef getName() { return DeclName; }
};

class DeclContext { // all declarations are thrown into a DeclContext and will be in the same level no matter how they're scoped lexically
private:
  DeclContext *ParentContext;
  llvm::StringMap<std::unique_ptr<Decl>> Decls;

public:
  DeclContext() : ParentContext(nullptr), Decls(llvm::StringMap<std::unique_ptr<Decl>>()) {}
  explicit DeclContext(DeclContext *ParentContext)
	  : ParentContext(ParentContext), Decls(llvm::StringMap<std::unique_ptr<Decl>>()) {}
  explicit DeclContext(llvm::StringMap<std::unique_ptr<Decl>> Decls)
	  : ParentContext(nullptr), Decls(std::move(Decls)) {}

  Decl *lookup(llvm::StringRef Name) {
	auto Found = Decls.find(Name);
	if (Found == Decls.end()) {
	  return nullptr;
	}
	return Found->second.get();
  }

  void setParentContext(DeclContext *Parent) {
	ParentContext = Parent;
  }

  bool insert(llvm::StringRef Name, std::unique_ptr<Decl> Dec) {
	auto Pair = Decls.insert(std::pair<llvm::StringRef, std::unique_ptr<Decl>>(Name, std::move(Dec)));
	return Pair.second;
  }
};

class CompilationUnit : public DeclContext {
private:
  std::unique_ptr<TopLevelDecls> funcs;
public:
//  explicit CompilationUnit(std::unique_ptr<DeclContext> GlobalDecls) : Decls(std::move(GlobalDecls)) {}
  CompilationUnit() : funcs(nullptr) {}
};

class TraitDecl : public Decl {
  RecordDecl *OtherType;
  std::unique_ptr<FunctionDecl> ToCall;

public:
  TraitDecl(llvm::StringRef Name, llvm::SMLoc Loc, RecordDecl *Other,
			std::unique_ptr<FunctionDecl> ToCall)
	  : Decl(DK_TRAIT, Name, Loc), ToCall(std::move(ToCall)), OtherType(Other) {}
};

class RecordDecl : public Decl {
private:
  std::unique_ptr<DeclContext> Ctx;
  Type *TypePtr;
  llvm::StringMap<TraitDecl *> Traits;
  size_t SizeInBits;

public:
  RecordDecl(llvm::StringRef name, Type *TypePtr,
			 llvm::SMLoc TypeLoc, llvm::SMLoc NameLoc, size_t SizeInBits)
	  : Decl(DK_TYPE, name, TypeLoc, NameLoc), SizeInBits(SizeInBits), TypePtr(TypePtr) {}
  RecordDecl(llvm::StringRef Name, DeclKind DKind, size_t SizeInBits)
	  : Decl(DKind, Name), Ctx(nullptr), SizeInBits(SizeInBits) {}

  DeclContext *getCtx() { return Ctx.get(); }
  static bool classof(const Decl *D) {
	return D->getKind() >= DK_TYPE && D->getKind() <= DK_TYPEPTR;
  }

  Type *getTypePtr() { return TypePtr; }
  size_t getSize() { return SizeInBits; }
};

class TypeUse {
private:
  llvm::SMLoc FirstQualifierLoc;
  llvm::SMLoc TypeNameLoc;
  funLang::Type *type;

public:
  explicit TypeUse(llvm::SMLoc loc) : type(nullptr) {}
  TypeUse(funLang::Type *type, llvm::SMLoc loc) : type(type) {}
  funLang::Type *getTypePtr() { return type; }
};

class TopLevelDecls {
private:
  std::unordered_map<std::string, std::unique_ptr<Decl>> fnMap;
public:
  explicit TopLevelDecls(
	  std::unordered_map<std::string, std::unique_ptr<Decl>> fnMap)
	  : fnMap(std::move(fnMap)) {}

  std::unordered_map<std::string, std::unique_ptr<Decl>> &getTopLevelMap();
};

class VarDecl : public Decl {
private:
  std::unique_ptr<TypeUse> UsedType;
  // qualifiers and stuff would go here?
public:
  VarDecl(std::unique_ptr<TypeUse> Type, llvm::StringRef Name, llvm::SMLoc Left,
		  llvm::SMLoc Right)
	  : Decl(DK_VAR, Name, Left, Right), UsedType(std::move(Type)) {}
  Type *getTypePtr() const { return UsedType->getTypePtr(); }
  static bool classof(const Decl *D) { return D->getKind() == DK_VAR; }
};

class FunctionDecl : public Decl {
private:
  std::unique_ptr<TypeUse> ReturnType;
  std::unique_ptr<CompoundStmt> Compound;
  std::unique_ptr<DeclContext> Ctx;
  llvm::SmallVector<Decl *> Params;

public:
  FunctionDecl(std::unique_ptr<TypeUse> retType,
			   llvm::StringRef name,
			   std::unique_ptr<CompoundStmt> compound,
			   llvm::SMLoc loc,
			   llvm::SmallVector<Decl *> Params,
			   std::unique_ptr<DeclContext> Ctx)
	  : ReturnType(std::move(retType)),
		Compound(std::move(compound)), Params(std::move(Params)), Decl(DK_FN, name, loc), Ctx(std::move(Ctx)) {}

  Type *getTypePtr() { return ReturnType->getTypePtr(); }
  llvm::SmallVector<Decl *> &getParams() { return Params; }
  CompoundStmt &getCompound() const { return *Compound; }
  static bool classof(const Decl *D) { return D->getKind() == DK_FN; }
};

class CallArgList {
private:
  std::vector<std::unique_ptr<Expr>> args;

public:
  explicit CallArgList(std::vector<std::unique_ptr<Expr>> a)
	  : args(std::move(a)) {}

  size_t getSize() { return args.size(); }
  std::vector<std::unique_ptr<Expr>> &getArgsVec() { return args; };
};

}