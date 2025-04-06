//
// Created by will on 10/21/24.
//
export module AST:Decl;
import Basic;
import Lex;
namespace funLang {
class Type;
class Stmt;
class Expr;
class CompoundStmt;

export {
  class TypeUse {
    using TypeUsePtr = u_ptr<TypeUse>;
    llvm::SMLoc FirstQualifierLoc, TypeNameLoc;
    Type *TypePtr{};
    TypeUse(Type *type, const llvm::SMLoc QualifierLoc,
            const llvm::SMLoc TypeNameLoc)
        : FirstQualifierLoc(QualifierLoc), TypeNameLoc(TypeNameLoc),
          TypePtr(type) {}

  public:
    static TypeUsePtr Create(llvm::SMLoc QualLoc, llvm::SMLoc TypeNameLoc,
                             Type *Ty) {
      return u_ptr<TypeUse>(new TypeUse(Ty, QualLoc, TypeNameLoc));
    }
    static TypeUsePtr InvalidTypeUse(const llvm::SMLoc QualLoc,
                                     const llvm::SMLoc TypeNameLoc) {
      return u_ptr<TypeUse>(new TypeUse(nullptr, QualLoc, TypeNameLoc));
    }
    [[nodiscard]] Type *getTypePtr() const { return TypePtr; }
    [[nodiscard]] llvm::SMLoc getFirstQualifierLoc() const {
      return FirstQualifierLoc;
    }
    [[nodiscard]] llvm::SMLoc getTypeNameLoc() const { return TypeNameLoc; }
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
    const Symbol *DeclName{};
    const llvm::SMLoc Start, End;
    u_ptr<Decl> Next{};

  public:
    explicit Decl(const DeclKind Kind, const Symbol *DeclName,
                  const llvm::SMLoc Start, const llvm::SMLoc End)
        : Kind(Kind), DeclName(DeclName), Start(Start), End(End),
          Next(nullptr) {}
    explicit Decl(const DeclKind Kind, const Symbol *DeclName,
                  const llvm::SMLoc Start, const llvm::SMLoc End,
                  u_ptr<Decl> Next)
        : Kind(Kind), DeclName(DeclName), Start(Start), End(End),
          Next(std::move(Next)) {}

    void setNext(u_ptr<Decl> N) { Next = std::move(N); }
    [[nodiscard]] Decl *getNext() const { return Next.get(); }
    [[nodiscard]] DeclKind getKind() const { return Kind; }
    [[nodiscard]] const Symbol *getEntry() const { return DeclName; }
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
        : ParentContext(ParentContext) {}
    explicit DeclContext(u_ptr<Decl> Dec)
        : FirstDecl(std::move(Dec)), LastDecl(Dec.get()) {}

    void setParentContext(DeclContext *Parent) { ParentContext = Parent; }

    llvm::SmallVector<Decl *> lookupDeclName(const Symbol *Name) const {
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
    Decl *getFirstDeclName(const Symbol *Name) const {
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
      case Decl::DK_FN: return true;
      default: return false;
      }
    }
    static bool classof([[maybe_unused]] const DeclContext *DC) { return true; }
  };

  class CompilationUnit : public DeclContext {
    explicit CompilationUnit(u_ptr<Decl> D) : DeclContext(std::move(D)) {}
    CompilationUnit() : DeclContext(nullptr) {}

  public:
    static u_ptr<CompilationUnit> Init() {
      return std::unique_ptr<CompilationUnit>(new CompilationUnit());
    }
  };

  class VarDecl : public Decl {
    friend class ActionRes;
    u_ptr<TypeUse> UsedType{};

  protected:
    explicit VarDecl(const DeclKind DK = DK_VAR, u_ptr<TypeUse> Type,
                     const Token &Name)
        : Decl(DK, Name.getIdentifierTableEntry(), Type->getTypeNameLoc(),
               Name.getRightmostLoc()),
          UsedType(std::move(Type)) {}
    VarDecl(u_ptr<VarDecl> D, const DeclKind DK)
        : Decl(DK, D->getEntry(), D->getStart(), D->getEnd()),
          UsedType(std::move(D->moveTypeUse())) {}

  public:
    u_ptr<TypeUse> moveTypeUse() { return std::move(UsedType); }
    [[nodiscard]] Type *getTypePtr() const { return UsedType->getTypePtr(); }
    static bool classof(const Decl *D) { return D->getKind() == DK_VAR; }
  };

  class ParamDecl : public VarDecl {
    friend class ActionRes;
    u_ptr<Expr> Init{};

  protected:
    ParamDecl(u_ptr<VarDecl> D, u_ptr<Expr> Init)
        : VarDecl(std::move(D), DK_PARAM), Init(std::move(Init)) {}

  public:
    Expr *getInit() { return Init.get(); }
    static bool classof(const Decl *D) { return D->getKind() == DK_PARAM; }
  };

  class FunctionDecl : public Decl, public DeclContext {
    u_ptr<Stmt> Body;
    u_ptr<TypeUse> ReturnType{};
    u_ptr<ParamDecl> Params{};

    FunctionDecl(u_ptr<TypeUse> retType, Symbol *Name,
                 u_ptr<CompoundStmt> Statements, SourceLoc TypeLoc,
                 SourceLoc RLoc, u_ptr<ParamDecl> Params,
                 DeclContext *ParentCtx);

  public:
    static auto Create(u_ptr<TypeUse> retType, Symbol *Name,
                       u_ptr<CompoundStmt> Statements, u_ptr<ParamDecl> Params,
                       DeclContext *ParentCtx);
    [[nodiscard]] Type *getTypePtr() const { return ReturnType->getTypePtr(); }
    [[nodiscard]] ParamDecl *getParams() const { return Params.get(); }
    [[nodiscard]] llvm::SmallVector<ParamDecl *> getParamsVector() const {
      Decl *Temp = Params.get();
      llvm::SmallVector<ParamDecl *> ParamVec{};
      while (Temp) {
        ParamVec.push_back(cast<ParamDecl>(Temp));
        Temp = Temp->getNext();
      }
      return ParamVec;
    }
    [[nodiscard]] CompoundStmt *getCompound() const {
      return llvm::dyn_cast<CompoundStmt>(Body.get());
    }
    static FunctionDecl *castFromDeclContext(DeclContext *DC) {
      return static_cast<FunctionDecl *>(DC);
    }
    void setCompoundStmt(u_ptr<Stmt> C) { Body = std::move(C); }
    static bool classof(const Decl *D) { return D->getKind() == DK_FN; }
  };

  class TraitDecl : public DeclContext {
  public:
    explicit TraitDecl(DeclContext *Parent) : DeclContext(Parent) {}
  };

  class RecordDecl : public Decl, public DeclContext {
    Type *TypePtr{};
    Decl *Impl{};

  public:
    RecordDecl(const Symbol *Name, Type *TypePtr, DeclContext *Parent)
        : Decl(DK_TYPE, Name, llvm::SMLoc(), llvm::SMLoc()),
          DeclContext(Parent), TypePtr(TypePtr) {}
    static bool classof(const Decl *D) { return D->getKind() == DK_TYPE; }
    void setTypePtr(Type *T) { TypePtr = T; }
    [[nodiscard]] Type *getTypePtr() const { return TypePtr; }
  };
}

}// namespace funLang
