//
// Created by will on 10/5/24.
//
export module Basic:IdentifierTable;
import llvm;
import std_modules;

namespace funLang {
export {
  using Symbol = llvm::StringMapEntry<std::nullopt_t>;
  class SymbolTable {
    llvm::StringSet<llvm::BumpPtrAllocator> SeenIdentifiers{};

  public:
    SymbolTable() = default;
    decltype(auto) insert(const llvm::StringRef Key) {
      auto [Str, Inserted] = SeenIdentifiers.insert(Key);
      return Inserted ? &*Str : nullptr;
    }

    decltype(auto) get(const llvm::StringRef Key) {
      const auto Str = SeenIdentifiers.find(Key);
      return Str != SeenIdentifiers.end() ? &*Str : nullptr;
    }
  };
}
}// namespace funLang
