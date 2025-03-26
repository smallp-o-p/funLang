//
// Created by will on 10/5/24.
//
export module Basic:IdentifierTable;
import llvm;
import std_modules;
#include <llvm/ADT/StringMap.h>
#include <optional>
#include <tuple>

namespace funLang {
export {
  using IDTableEntry = llvm::StringMapEntry<std::nullopt_t>;
  class IdentifierTable {
    llvm::StringSet<llvm::BumpPtrAllocator> SeenIdentifiers{};

  public:
    IdentifierTable() = default;
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
