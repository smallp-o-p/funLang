//
// Created by will on 12/17/24.
//

import Basic;
import funLangAST;
export module Sema:Scope;
#include "llvm/ADT/DenseMap.h"

namespace funLang {
  class Scope {
    friend class SemaAnalyzer;
    enum ScopeKind {
      FunctionScope,
      LoopScope,
      BlockScope,
      StructScope,
      GlobalScope,
    };

    u_ptr<Scope> Parent{};
    DeclContext *ScopeContext{};
    llvm::DenseMap<IDTableEntry *, Decl *> LookupTable{};
    ScopeKind Kind;

  public:
    Scope();
    explicit Scope(u_ptr<Scope> Parent, ScopeKind K);
    Scope(u_ptr<Scope> Parent, const ScopeKind K, DeclContext *Ctx)
      : Parent(std::move(Parent)), ScopeContext(Ctx), Kind(K) {
    }
    bool insertInContext(u_ptr<Decl> Dec);
    u_ptr<Scope> moveParentScope() { return std::move(Parent); }
    Scope *getParent() const { return Parent.get(); }
    DeclContext *getContext() const { return ScopeContext; };
    Decl *lookup(const IDTableEntry *Name) {
      const auto Found = LookupTable.find(Name);
      return Found != LookupTable.end() ? Found->second : nullptr;
    };

    [[nodiscard]] bool isFunctionScope() const { return Kind == FunctionScope; }
    [[nodiscard]] bool isStructScope() const { return Kind == StructScope; }
    [[nodiscard]] bool isLoopScope() const { return Kind == LoopScope; }
    [[nodiscard]] bool isBlockScope() const { return Kind == BlockScope; }

    Scope *getClosestLoopScope() {
      auto S = this;
      while (!S->isLoopScope()) {
        S = S->getParent();
        if (!S) {
          return nullptr;
        }
      }
      return S;
    }

    bool insertInLexicalScope(IDTableEntry *ID, Decl *Dec) {
      return LookupTable.insert({ID, Dec}).second;
    }
    bool isGlobalScope() const { return Parent.get() != nullptr; }
  };
}
