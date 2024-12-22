//
// Created by will on 10/21/24.
//
module;
#include <llvm/Support/SMLoc.h>
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"
export module AST:Decl;
import Basic;

namespace funLang {
  class Type;
  class Stmt;
  class CompoundStmt;

  export
  {
    class TypeUse {
      const llvm::SMLoc FirstQualifierLoc, TypeNameLoc;
      Type *TypePtr;

    public:
      TypeUse(Type *type, const llvm::SMLoc QualifierLoc, const llvm::SMLoc TypeNameLoc)
        : FirstQualifierLoc(QualifierLoc), TypeNameLoc(TypeNameLoc), TypePtr(type) {
      }
      [[nodiscard]] Type *getTypePtr() const { return TypePtr; }
      [[nodiscard]] const llvm::SMLoc &getFirstQualifierLoc() const { return FirstQualifierLoc; }
      [[nodiscard]] const llvm::SMLoc &getTypeNameLoc() const { return TypeNameLoc; }
    };

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
      IDTableEntry *DeclName{};
      const llvm::SMLoc Start, End;
      u_ptr<Decl> Next{};

    public:
      explicit Decl(const DeclKind Kind, IDTableEntry *DeclName, const llvm::SMLoc Start, const llvm::SMLoc End)
        : Kind(Kind), DeclName(DeclName), Start(Start), End(End), Next(nullptr) {
      }
      explicit Decl(const DeclKind Kind,
                    IDTableEntry *DeclName,
                    const llvm::SMLoc Start,
                    const llvm::SMLoc End,
                    u_ptr<Decl> Next)
        : Kind(Kind), DeclName(DeclName), Start(Start), End(End), Next(std::move(Next)) {
      }

      void setNext(u_ptr<Decl> N) { Next = std::move(N); }
      [[nodiscard]] Decl *getNext() const { return Next.get(); }
      [[nodiscard]] DeclKind getKind() const { return Kind; }
      [[nodiscard]] IDTableEntry *getEntry() const { return DeclName; }
      [[nodiscard]] llvm::StringRef getName() const { return DeclName->first(); }
      [[nodiscard]] llvm::SMLoc getStart() const { return Start; }
      [[nodiscard]] llvm::SMLoc getEnd() const { return End; }
    };

    class DeclContext {
      // all declarations are thrown into a DeclContext and will be in the same level no matter how they're scoped lexically
      DeclContext *ParentContext{};
      u_ptr<Decl> FirstDecl{};
      Decl *LastDecl{};

    public:
      explicit DeclContext(DeclContext *ParentContext)
        : ParentContext(ParentContext) {
      }
      explicit DeclContext(u_ptr<Decl> Dec)
        : FirstDecl(std::move(Dec)) {
      }

      void setParentContext(DeclContext *Parent) { ParentContext = Parent; }

      llvm::SmallVector<Decl *> lookupDeclName(const IDTableEntry *Name) const {
        auto D = FirstDecl.get();
        auto LookupResults = llvm::SmallVector<Decl *, 16>();
        while (D) {
          if (D->getEntry() == Name) {
            LookupResults.push_back(D);
          }
          D = D->getNext();
        }
        return LookupResults;
      }
      Decl *getFirstDeclName(const IDTableEntry *Name) const {
        const auto D = FirstDecl.get();

        while (D) {
          if (D->getEntry() == Name) {
            return D;
          }
        }
        return nullptr;
      }

      void addDecl(u_ptr<Decl> D) {
        LastDecl->setNext(std::move(D));
        LastDecl = LastDecl->getNext();
      }

      static bool classof(const Decl *D) {
        switch (D->getKind()) {
        case Decl::DK_FN:
          return true;
        default:
          return false;
        }
      }
      static bool classof([[maybe_unused]] const DeclContext *DC) {
        return true;
      }
    };

    class CompilationUnit : public DeclContext {
    public:
      explicit CompilationUnit(u_ptr<Decl> D) : DeclContext(std::move(D)) {
      }
    };

    class ParamDecl : public Decl {
      u_ptr<Stmt> Init;
      DeclContext *Container;
      const u_ptr<TypeUse> T{};

    public:
      ParamDecl(IDTableEntry *Name,
                u_ptr<Stmt> Init,
                DeclContext *Container,
                u_ptr<ParamDecl> Next,
                llvm::SMLoc Left,
                llvm::SMLoc Right,
                u_ptr<TypeUse> T);

      [[nodiscard]] Type *getTypePtr() const { return T->getTypePtr(); }
      static bool classof(const Decl *D) { return D->getKind() == DK_PARAM; }
    };

    class FunctionDecl : public Decl, public DeclContext {
      u_ptr<CompoundStmt> Statements;
      u_ptr<TypeUse> ReturnType{};
      u_ptr<ParamDecl> Params{};

    public:
      FunctionDecl(u_ptr<TypeUse> retType,
                   IDTableEntry *Name,
                   u_ptr<CompoundStmt> Statements,
                   llvm::SMLoc TypeLoc,
                   llvm::SMLoc RLoc,
                   u_ptr<ParamDecl> Params,
                   DeclContext *ParentCtx);
      ~FunctionDecl();
      [[nodiscard]] Type *getTypePtr() const { return ReturnType->getTypePtr(); }
      [[nodiscard]] ParamDecl &getParams() const { return *Params; }
      [[nodiscard]] CompoundStmt *getCompound() const { return Statements.get(); }
      static FunctionDecl *castFromDeclContext(const DeclContext *DC) {
        return static_cast<FunctionDecl *>(const_cast<DeclContext *>(DC));
      }
      static bool classof(const Decl *D) { return D->getKind() == DK_FN; }
    };

    class TraitDecl : public DeclContext {
    public:
      explicit TraitDecl(DeclContext *Parent) : DeclContext(Parent) {
      }
    };

    class RecordDecl : public Decl, public DeclContext {
      Type *TypePtr{};
      Decl *implDecl{};

    public:
      RecordDecl(IDTableEntry *name, Type *TypePtr, DeclContext *Parent)
        : Decl(DK_TYPE, name, llvm::SMLoc(), llvm::SMLoc()), DeclContext(Parent), TypePtr(TypePtr) {
      }
      static bool classof(const Decl *D) { return D->getKind() == DK_TYPE; }
      void setTypePtr(Type *T) { TypePtr = T; }
      [[nodiscard]] Type *getTypePtr() const { return TypePtr; }
    };

    class VarDecl : public Decl {
      u_ptr<TypeUse> UsedType{};
      // qualifiers and stuff would go here?
    public:
      VarDecl(u_ptr<TypeUse> Type,
              IDTableEntry *Name,
              const llvm::SMLoc Left,
              const llvm::SMLoc Right)
        : Decl(DK_VAR, Name, Left, Right), UsedType(std::move(Type)) {
      }
      Type *getTypePtr() const { return UsedType->getTypePtr(); }
      static bool classof(const Decl *D) { return D->getKind() == DK_VAR; }
    };
  }
} // namespace funLang
