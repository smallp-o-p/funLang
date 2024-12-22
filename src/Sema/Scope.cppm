//
// Created by will on 12/17/24.
//
module;
#include <llvm/Support/Casting.h>
#include "llvm/ADT/DenseMap.h"
export module Sema:Scope;
import Basic;
import AST;

namespace funLang {
  export class Scope {
  public:
    enum ScopeKind : unsigned {
      FunctionScope = 0x01,
      LoopScope = 0x02,
      BlockScope = 0x04,
      StructScope = 0x08,
      GlobalScope = 0x10,
    };

    class Iterator {
      friend class Scope;
      Scope *Current{};

      explicit Iterator(Scope *S) : Current(S) {
      }

    public:
      using iterator_category = std::input_iterator_tag;
      using difference_type = std::ptrdiff_t;
      using value_type = Scope;
      using pointer = Scope *;
      using reference = Scope &;

      bool operator==(const Iterator &Other) const { return Current == Other.Current; }
      bool operator!=(const Iterator &Other) const { return Current != Other.Current; }
      bool operator==(const Scope *Other) const { return Current == Other; }
      bool operator!=(const Scope *Other) const { return Current != Other; }
      bool operator !() const { return Current == nullptr; }
      explicit operator bool() const { return Current != nullptr; }
      Iterator &operator++() {
        Current = Current->getParent();
        return *this;
      }

      Iterator operator++(int) {
        const Iterator Tmp = *this;
        Current = Current->getParent();
        return Tmp;
      }
      pointer operator->() const { return Current; }
      reference operator*() const { return *Current; }
      [[nodiscard]] pointer get() const { return Current; }
      [[nodiscard]] Iterator next() const { return Iterator(Current->getParent()); }
    };

  private:
    u_ptr<Scope> Parent{};
    DeclContext *ScopeContext{};
    llvm::DenseMap<IDTableEntry *, Decl *> LookupTable{};
    ScopeKind Kind;

  public:
    Scope() : Parent(nullptr), Kind(GlobalScope) {
    }
    Scope(u_ptr<Scope> Parent, const ScopeKind K): Parent(std::move(Parent)),
                                                   ScopeContext(Parent->getContext()),
                                                   Kind(K) {
    }

    Scope(u_ptr<Scope> Parent, const ScopeKind K, DeclContext *Ctx)
      : Parent(std::move(Parent)), ScopeContext(Ctx), Kind(K) {
    }
    Iterator begin() const { return Iterator(const_cast<Scope *>(this)); }
    Iterator end() const { return Iterator(nullptr); }
    bool insertInContext(u_ptr<Decl> Dec);
    u_ptr<Scope> moveParentScope() { return std::move(Parent); }
    [[nodiscard]] Scope *getParent() const { return Parent.get(); }
    [[nodiscard]] DeclContext *getContext() const { return ScopeContext; }
    Decl *lookup(const IDTableEntry *Name) {
      const auto Found = LookupTable.find(Name);
      return Found != LookupTable.end() ? Found->second : nullptr;
    }
    [[nodiscard]] Type *getFunctionReturnType() const {
      const auto Res = std::find_if(begin(), end(), [&](const Scope &S) { return S.isFunctionScope(); });
      return Res != end() ? FunctionDecl::castFromDeclContext(Res->getContext())->getTypePtr() : nullptr;
    }

    [[nodiscard]] bool isFunctionScope() const { return Kind == FunctionScope; }
    [[nodiscard]] bool isStructScope() const { return Kind == StructScope; }
    [[nodiscard]] bool isLoopScope() const { return Kind == LoopScope; }
    [[nodiscard]] bool isBlockScope() const { return Kind == BlockScope; }

    Scope *getClosestLoopScope() {
      const auto Res = std::find_if(begin(), end(), [&](const Scope &S) { return S.isLoopScope(); });
      return Res != end() ? Res.get() : nullptr;
    }

    bool insertInLexicalScope(IDTableEntry *ID, Decl *Dec) {
      return LookupTable.insert({ID, Dec}).second;
    }
    bool isGlobalScope() const { return Parent.get() != nullptr; }
  };
}
